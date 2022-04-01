#include "translate.hpp"

#include <cstdio>

#include "ast.hpp"
#include "print.hpp"

namespace lcc {

namespace {

namespace ssa {

struct SparseSet {
    void init(size_t capacity, size_t valLim) {
        dense = mem::c_malloc<size_t>(capacity);
        sparse = mem::c_malloc<size_t>(valLim);
        size = 0;
#ifndef NDEBUG
        this->capacity = capacity;
        this->valLim = valLim;
#endif
    }
    void clear() { size = 0; }
    bool try_add(size_t val) {
        assert(val < valLim);
        if (contains(val)) return false;
        dense[size] = val;
        sparse[val] = size++;
        return true;
    }
    size_t pop() {
        assert(size > 0);
        return dense[--size];
    }
    bool contains(size_t val) {
        assert(val < valLim);
        size_t s = sparse[val];
        return s < size && dense[s] == val;
    }
    size_t *dense;
    size_t *sparse;
    size_t size;
#ifndef NDEBUG
    size_t capacity;  // size of dense list
    size_t valLim;    // size of sparse list
#endif
};

struct VariableRef {
    size_t varId;
    ValId *valLoc;
    bool isDefNotUse;
    VariableRef *nextRef;
};

struct VariableRefList {
    VariableRef *start;
    VariableRef *end;
};

struct VariableInfo {
    size_t varId;
    LList<BlockId> defSites;
    LList<ValId> idStack;
};

struct TranslationContext {
    BlockId blockCnt{0};
    ValId valCnt{0};
    LList<BasicBlock *> *dominators;

    LMap<LStringView, VariableInfo *, lstr_hash, lstr_equal> varMap;
    LList<VariableInfo *> varInfos;
    SparseSet liveVarSet;

    LList<VariableRefList> refs;  // Keep track of use and define for variable renaming
};

Inst *create_inst(InstKind kind) {
    Inst *inst = mem::malloc<Inst>();
    inst->kind = kind;
    inst->next = nullptr;
    return inst;
}

void add_inst(BasicBlock *block, Inst *inst) {
    if (block->end) {
        block->end->next = inst;
    } else {
        block->start = inst;
    }
    block->end = inst;
}

static BasicBlock *succBuff[2];

BasicBlock **get_successors(size_t &numSucc, BasicBlock *block) {
    switch (block->terminator->kind) {
        case TerminatorKind::kGoto: {
            succBuff[0] = block->terminator->tgoto.target;
            numSucc = 1;
            return succBuff;
        }
        case TerminatorKind::kCond: {
            succBuff[0] = block->terminator->cond.then;
            succBuff[1] = block->terminator->cond.alt;
            numSucc = 2;
            return succBuff;
        }
        case TerminatorKind::kRet: numSucc = 0; return 0;
    }
}

Terminator *create_terminator(TerminatorKind kind) {
    Terminator *terminator = mem::malloc<Terminator>();
    terminator->kind = kind;
    return terminator;
}

BasicBlock *create_block() {
    BasicBlock *block = mem::malloc<BasicBlock>();
    block->start = nullptr;
    block->end = nullptr;
    block->terminator = nullptr;
    return block;
}

VariableRef *create_variable_reference(TranslationContext &tctx, BlockId blockId) {
    while (tctx.refs.size <= blockId) {
        tctx.refs.add({nullptr, nullptr});
    }
    VariableRefList &refList = tctx.refs.get(blockId);

    VariableRef *newRef = mem::p_malloc<VariableRef>();
    newRef->nextRef = nullptr;
    if (refList.start) {
        refList.end->nextRef = newRef;
    } else {
        refList.start = newRef;
    }
    refList.end = newRef;
    return newRef;
}

BasicBlock *create_block(TranslationContext &tctx) {
    BasicBlock *block = create_block();
    block->id = tctx.blockCnt++;
    return block;
}

void translate_block(TranslationContext &tctx, BasicBlock *entry, BasicBlock *exit, Node *block);

ValId &translate_expr(TranslationContext &tctx, BasicBlock *curr, Node *expr) {
    switch (expr->kind) {
        case NodeKind::kIntLit: {
            Inst *aconst = create_inst(InstKind::kConst);
            aconst->aconst.intVal = expr->intLit.intVal;
            add_inst(curr, aconst);
            mem::p_free(expr);
            return aconst->aconst.dest = tctx.valCnt++;
        }
        case NodeKind::kName: {
            if (expr->name.ref->decl.isAssignToGlobal) {
                todo("Use of global variable '%s'\n", lstr_raw_str(expr->name.ident));
                assert(false);
            }
            Inst *usage = create_inst(InstKind::kUsage);
            VariableRef *varRef = create_variable_reference(tctx, curr->id);
            varRef->varId = (*tctx.varMap.get(expr->name.ident))->varId;
            varRef->valLoc = &usage->usage.useId;
            varRef->isDefNotUse = false;
            add_inst(curr, usage);
            mem::p_free(expr);
            return usage->usage.dest = tctx.valCnt++;
        }
        case NodeKind::kInfix: {
            Inst *bin = create_inst(InstKind::kBin);
            bin->bin.op = expr->infix.op;
            bin->bin.left = translate_expr(tctx, curr, expr->infix.left);
            bin->bin.right = translate_expr(tctx, curr, expr->infix.right);
            add_inst(curr, bin);
            mem::p_free(expr);
            return bin->bin.dest = tctx.valCnt++;
        }
        default: assert(false && "TODO: no translation implemented for expression kind"); unreachable();
    }
}

void translate_if(TranslationContext &tctx, BasicBlock *entry, BasicBlock *exit, Node *ifstmt) {
    for (;;) {
        entry->terminator = create_terminator(TerminatorKind::kCond);
        entry->terminator->cond.predicate = translate_expr(tctx, entry, ifstmt->ifstmt.cond);

        BasicBlock *then = create_block(tctx);
        entry->terminator->cond.then = then;
        translate_block(tctx, then, exit, ifstmt->ifstmt.then);

        if (!ifstmt->ifstmt.alt) {
            entry->terminator->cond.alt = exit;
            return;
        }
        BasicBlock *alt = create_block(tctx);
        entry->terminator->cond.alt = alt;
        if (ifstmt->ifstmt.alt->kind == NodeKind::kBlock) {
            translate_block(tctx, alt, exit, ifstmt->ifstmt.alt);
            return;
        }
        assert(ifstmt->ifstmt.alt->kind == NodeKind::kIf);
        entry = alt;
        ifstmt = ifstmt->ifstmt.alt;
    }
}

void translate_while(TranslationContext &tctx, BasicBlock *entry, BasicBlock *exit, Node *whilestmt) {
    entry->terminator = create_terminator(TerminatorKind::kCond);
    entry->terminator->cond.alt = exit;
    entry->terminator->cond.predicate = translate_expr(tctx, entry, whilestmt->whilestmt.cond);

    // Goto targets for break and continue statements
    whilestmt->whilestmt.info->entry = entry;
    whilestmt->whilestmt.info->exit = exit;

    BasicBlock *loop = create_block(tctx);
    entry->terminator->cond.then = loop;

    loop->terminator = create_terminator(TerminatorKind::kGoto);
    loop->terminator->tgoto.target = entry;

    translate_block(tctx, loop, entry, whilestmt->whilestmt.loop);
}

void translate_block(TranslationContext &tctx, BasicBlock *entry, BasicBlock *exit, Node *block) {
    assert(block->kind == NodeKind::kBlock);
    BasicBlock *curr = entry;
    for (StatementListNode *stmtNode = block->block.start; stmtNode; stmtNode = stmtNode->next) {
        Node *stmt = stmtNode->stmt;
        switch (stmt->kind) {
            case NodeKind::kDecl: {
                if (stmt->decl.lval->kind == NodeKind::kName) {
                    VariableInfo *varInfo;
                    if (VariableInfo **varInfoRef = tctx.varMap.get(stmt->decl.lval->name.ident)) {
                        varInfo = *varInfoRef;
                        LList<BlockId> &defSites = varInfo->defSites;
                        // Ensure no duplicates before addition
                        if (!defSites.size || defSites.last() != curr->id) {
                            defSites.add(curr->id);
                        }
                    } else {
                        varInfo = mem::malloc<VariableInfo>();
                        varInfo->varId = tctx.varMap.size;
                        varInfo->defSites = {};
                        varInfo->idStack = {};
                        varInfo->defSites.add(curr->id);
                        tctx.varMap.try_put(stmt->decl.lval->name.ident, varInfo);
                        tctx.varInfos.add(varInfo);
                    }
                    ValId *valLoc = &translate_expr(tctx, curr, stmt->decl.rval);
                    VariableRef *varRef = create_variable_reference(tctx, curr->id);
                    varRef->varId = varInfo->varId;
                    varRef->valLoc = valLoc;
                    varRef->isDefNotUse = true;
                }
                break;
            }
            case NodeKind::kIf: {
                if (stmtNode->next) {
                    BasicBlock *ifExit = create_block();
                    translate_if(tctx, curr, ifExit, stmt);
                    ifExit->id = tctx.blockCnt++;
                    curr = ifExit;
                    break;
                } else {
                    translate_if(tctx, curr, exit, stmt);
                    return;
                }
                break;
            }
            case NodeKind::kWhile: {
                if (curr->start) {
                    BasicBlock *cond = create_block(tctx);
                    curr->terminator = create_terminator(TerminatorKind::kGoto);
                    curr->terminator->tgoto.target = cond;
                    curr = cond;
                }
                if (stmtNode->next) {
                    BasicBlock *whileExit = create_block();
                    translate_while(tctx, curr, whileExit, stmt);
                    whileExit->id = tctx.blockCnt++;
                    curr = whileExit;
                    break;
                } else {
                    translate_while(tctx, curr, exit, stmt);
                    return;
                }
                break;
            }
            case NodeKind::kLoopBr: {
                curr->terminator = create_terminator(TerminatorKind::kGoto);
                if (stmt->loopbr.isBreak) {
                    curr->terminator->tgoto.target = stmt->loopbr.ref->whilestmt.info->exit;
                } else {
                    curr->terminator->tgoto.target = stmt->loopbr.ref->whilestmt.info->entry;
                }
                return;
            }
            default: break;
        }
    }
    curr->terminator = create_terminator(TerminatorKind::kGoto);
    curr->terminator->tgoto.target = exit;
    mem::p_free(block);
}

struct DominatorForestContext {
    size_t *ancestor;
    size_t *label;
    size_t *child;
    size_t *size;
};

void link(size_t *semi, DominatorForestContext &dfctx, size_t to, size_t from) {
    size_t curr = from;
    while (semi[dfctx.label[from]] < semi[dfctx.label[dfctx.child[curr]]]) {
        size_t ch = dfctx.child[curr];
        if (dfctx.size[curr] + dfctx.size[dfctx.child[ch]] >= dfctx.size[ch] << 1) {
            dfctx.ancestor[ch] = curr;
            dfctx.child[curr] = dfctx.child[ch];
        } else {
            dfctx.size[ch] = dfctx.size[curr];
            dfctx.ancestor[curr] = ch;
            curr = ch;
        }
    }
    dfctx.label[curr] = dfctx.label[from];
    dfctx.size[to] += dfctx.size[from];
    if (dfctx.size[to] < dfctx.size[from] << 1) {
        size_t temp = curr;
        curr = dfctx.child[to];
        dfctx.child[to] = temp;
    }
    while (curr) {
        dfctx.ancestor[curr] = to;
        curr = dfctx.child[curr];
    }
}

void compress(size_t *semi, DominatorForestContext &dfctx, size_t node) {
    size_t ancest = dfctx.ancestor[node];
    if (dfctx.ancestor[ancest]) {
        compress(semi, dfctx, ancest);
        if (semi[dfctx.label[ancest]] < semi[dfctx.label[node]]) {
            dfctx.label[node] = dfctx.label[ancest];
        }
        dfctx.ancestor[node] = dfctx.ancestor[ancest];
    }
}

size_t eval(size_t *semi, DominatorForestContext &dfctx, size_t node) {
    if (dfctx.ancestor[node]) {
        compress(semi, dfctx, node);
        size_t ancest = dfctx.ancestor[node];
        if (semi[dfctx.label[ancest]] >= semi[dfctx.label[node]]) {
            return dfctx.label[node];
        } else {
            return dfctx.label[ancest];
        }
    }
    return dfctx.label[node];
}

void rename_block(TranslationContext &tctx, BasicBlock *curr) {
    size_t *numDefs = mem::c_malloc<size_t>(tctx.varMap.size);
    for (size_t i = 0; i < tctx.varMap.size; i++) {
        numDefs[i] = 0;
    }

    Inst *phi = curr->start;
    while (phi && phi->kind == InstKind::kPhi) {
        VariableInfo *varInfo = tctx.varInfos.get(phi->phi.varId);
        varInfo->idStack.add(phi->phi.dest);
        numDefs[varInfo->varId]++;
        phi = phi->next;
    }

    if (curr->id < tctx.refs.size) {
        VariableRef *currRef = tctx.refs.get(curr->id).start;
        while (currRef) {
            VariableInfo *varInfo = tctx.varInfos.get(currRef->varId);
            if (currRef->isDefNotUse) {
                varInfo->idStack.add(*currRef->valLoc);
                numDefs[varInfo->varId]++;
            } else {
                *currRef->valLoc = varInfo->idStack.last();
            }
            currRef = currRef->nextRef;
        }
        mem::p_free(currRef);
    }

    // Add joins to phi nodes
    size_t numSucc;
    BasicBlock **succ = get_successors(numSucc, curr);
    for (size_t i = 0; i < numSucc; i++) {
        Inst *phi = succ[i]->start;
        while (phi && phi->kind == InstKind::kPhi) {
            VariableInfo *varInfo = tctx.varInfos.get(phi->phi.varId);
            phi->phi.joins.add(varInfo->idStack.last());
            phi = phi->next;
        }
    }
    // Translate dominator children
    for (size_t i = 0; i < tctx.dominators[curr->id + 1].size; i++) {
        rename_block(tctx, tctx.dominators[curr->id + 1].get(i));
    }
    // Pop id stack for each variable
    for (size_t i = 0; i < tctx.varMap.size; i++) {
        tctx.varInfos.get(i)->idStack.size -= numDefs[i];
    }
    mem::c_free(numDefs);
}

BasicBlock *translate_function_body(Node *functionBody) {
    TranslationContext tctx{};
    tctx.varMap.init();
    BasicBlock *entry = create_block(tctx);
    BasicBlock *exit = create_block();
    translate_block(tctx, entry, exit, functionBody);
    exit->id = tctx.blockCnt++;
    exit->terminator = create_terminator(TerminatorKind::kRet);

    // Fast dominator algorithm by Lengauer and Tarjan
    // Uses the "sophisticated" eval and link methods
    // Allocate memory for data structures
    size_t numNodes = tctx.blockCnt + 1;
    size_t arenaSize = sizeof(BasicBlock *) * numNodes;
    arenaSize += sizeof(size_t) * numNodes * 8;
    arenaSize += sizeof(LList<size_t>) * numNodes * 2;
    arenaSize += sizeof(LList<BasicBlock *>) * numNodes;
    void *arena = mem::c_malloc<int8_t>(arenaSize);
    BasicBlock **blocks = (BasicBlock **)arena;
    arena = &blocks[numNodes];
    size_t *idom = (size_t *)arena;
    arena = &idom[numNodes];
    size_t *vertex = (size_t *)arena;
    arena = &vertex[numNodes];
    size_t *semi = (size_t *)arena;
    arena = &semi[numNodes];
    size_t *parent = (size_t *)arena;
    arena = &parent[numNodes];
    DominatorForestContext dfctx;
    dfctx.ancestor = (size_t *)arena;
    arena = &dfctx.ancestor[numNodes];
    dfctx.label = (size_t *)arena;
    arena = &dfctx.label[numNodes];
    dfctx.child = (size_t *)arena;
    arena = &dfctx.child[numNodes];
    dfctx.size = (size_t *)arena;
    arena = &dfctx.size[numNodes];
    LList<size_t> *pred = (LList<size_t> *)arena;
    arena = &pred[numNodes];
    LList<size_t> *bucket = (LList<size_t> *)arena;
    arena = &bucket[numNodes];
    tctx.dominators = (LList<BasicBlock *> *)arena;
    arena = &tctx.dominators[numNodes];

    // Initialize data structures
    for (size_t i = 0; i < numNodes; i++) {
        semi[i] = 0;
        dfctx.ancestor[i] = 0;
        dfctx.child[i] = 0;
        dfctx.size[i] = 1;
        pred[i] = {};
        bucket[i] = {};
        tctx.dominators[i] = {};
    }
    blocks[0] = nullptr;
    idom[0] = 0;
    idom[entry->id + 1] = 0;
    vertex[0] = 0;
    dfctx.label[0] = 0;
    dfctx.size[0] = 0;

    // Iterative DFS numbering
    LList<BasicBlock *> stack = {};
    stack.add(entry);
    size_t cnt = 0;
    while (stack.size) {
        BasicBlock *top = stack.get(stack.size - 1);
        stack.size--;
        size_t tn = top->id + 1;
        assert(tn < numNodes);
        if (semi[tn]) continue;
        semi[tn] = ++cnt;
        assert(cnt < numNodes);
        blocks[tn] = top;
        vertex[cnt] = dfctx.label[tn] = tn;

        size_t numSucc;
        BasicBlock **succ = get_successors(numSucc, top);
        for (size_t i = 0; i < numSucc; i++) {
            size_t pn = succ[i]->id + 1;
            pred[pn].add(tn);
            if (!semi[pn]) {
                parent[pn] = tn;
                stack.add(succ[i]);
            }
        }
    }
    for (size_t i = numNodes - 1; i >= 2; i--) {
        // Compute semi-dominators
        size_t currN = vertex[i];
        for (size_t k = 0; k < pred[currN].size; k++) {
            size_t sevaln = semi[eval(semi, dfctx, pred[currN].get(k))];
            if (sevaln < semi[currN]) semi[currN] = sevaln;
        }
        bucket[vertex[semi[currN]]].add(currN);
        link(semi, dfctx, parent[currN], currN);
        // Implicitly define immediate dominator for each vertex
        LList<size_t> &parenBucket = bucket[parent[currN]];
        for (size_t k = 0; k < parenBucket.size; k++) {
            size_t vn = parenBucket.get(k);
            size_t evaln = eval(semi, dfctx, vn);
            idom[vn] = semi[evaln] < semi[vn] ? evaln : parent[currN];
        }
        parenBucket.size = 0;
    }

    // Explicitly define immediate dominators
    for (size_t i = 2; i < numNodes; i++) {
        size_t wn = vertex[i];
        if (idom[wn] != vertex[semi[wn]]) idom[wn] = idom[idom[wn]];
        tctx.dominators[idom[wn]].add(blocks[wn]);
    }

    // Simple Dominance Frontier algorithm by Cooper et al.
    SparseSet *domFronts = mem::c_malloc<SparseSet>(numNodes);
    for (size_t i = 0; i < numNodes; i++) {
        domFronts[i].init(numNodes, numNodes);
    }
    for (size_t i = 1; i < numNodes; i++) {
        size_t n = vertex[i];
        if (pred[n].size <= 1) continue;
        for (size_t p = 0; p < pred[n].size; p++) {
            size_t runner = pred[n].get(p);
            while (runner != idom[n]) {
                domFronts[runner].try_add(n);
                runner = idom[runner];
            }
        }
    }

    // Iterated dominance-frontier and phi node insertion
    SparseSet philist;
    philist.init(numNodes, numNodes);
    SparseSet worklist;
    worklist.init(numNodes, numNodes);
    for (size_t i = 0; i < tctx.varInfos.size; i++) {
        worklist.clear();
        philist.clear();
        VariableInfo *var = tctx.varInfos.get(i);
        for (size_t k = 0; k < var->defSites.size; k++) {
            worklist.try_add(var->defSites.get(k) + 1);
        }
        while (worklist.size) {
            size_t def = worklist.pop();
            for (size_t k = 0; k < domFronts[def].size; k++) {
                size_t dfy = domFronts[def].dense[k];
                if (!philist.contains(dfy)) {
                    Inst *phi = create_inst(InstKind::kPhi);
                    phi->phi.varId = var->varId;
                    phi->phi.dest = tctx.valCnt++;
                    phi->phi.joins.init(pred[dfy].size);

                    phi->next = blocks[dfy]->start;
                    blocks[dfy]->start = phi;

                    philist.try_add(dfy);

                    // TODO: optimization where a node shouldn't be added if
                    // the variable is already defined there
                    if (dfy != def) worklist.try_add(dfy);
                }
            }
        }
    }

    // Translation
    tctx.liveVarSet.init(tctx.varMap.size, tctx.varMap.size);
    rename_block(tctx, entry);

    mem::c_free(tctx.liveVarSet.dense);
    mem::c_free(tctx.liveVarSet.sparse);
    // Free data structures
    for (size_t i = 0; i < numNodes; i++) {
        mem::c_free(pred[i].data);
        mem::c_free(bucket[i].data);
        mem::c_free(tctx.dominators[i].data);
    }
    mem::c_free((void *)((int8_t *)arena - arenaSize));
    // TODO: free all the VariableInfo * inside the map
    mem::c_free(tctx.varMap.table);
    mem::p_free(functionBody);
    return entry;
}

}  // namespace ssa

void r_print_block(BlockId &bid, BasicBlock *basicBlock) {
    printf("b%d\n", basicBlock->id);
    Inst *inst = basicBlock->start;
    while (inst) {
        switch (inst->kind) {
            case InstKind::kPhi:
                printf("  v%d = phi(", inst->phi.dest);
                for (size_t k = 0; k < inst->phi.joins.size; k++) {
                    printf("v%d", inst->phi.joins.get(k));
                    if (k < inst->phi.joins.size - 1) {
                        printf(", ");
                    }
                }
                printf(")\n");
                break;
            case InstKind::kConst: printf("  v%d = %d\n", inst->aconst.dest, inst->aconst.intVal); break;
            case InstKind::kUsage: printf("  v%d = v%d\n", inst->usage.dest, inst->usage.useId); break;
            case InstKind::kBin:
                const char *opstr;
                switch (inst->bin.op) {
                    case TokenType::kAdd: opstr = "add"; break;
                    default: opstr = "?";
                }
                printf("  v%d = %s(v%d, v%d)\n", inst->bin.dest, opstr, inst->bin.left, inst->bin.right);
                break;
            case InstKind::kCall: todo("Unimplemented call print\n"); assert(false);
        }
        inst = inst->next;
    }
    bid++;
    switch (basicBlock->terminator->kind) {
        case TerminatorKind::kGoto: {
            BasicBlock *exit = basicBlock->terminator->tgoto.target;
            printf("  goto b%d\n", exit->id);
            if (bid == exit->id) r_print_block(bid, exit);
            break;
        }
        case TerminatorKind::kCond: {
            BasicBlock *then = basicBlock->terminator->cond.then;
            BasicBlock *alt = basicBlock->terminator->cond.alt;
            printf("  if v%d -> b%d b%d\n", basicBlock->terminator->cond.predicate, then->id, alt->id);

            if (bid == then->id) r_print_block(bid, then);
            if (bid == alt->id) r_print_block(bid, alt);
            break;
        }
        case TerminatorKind::kRet: printf("  ret\n"); break;
    }
}

void print_block(BasicBlock *basicBlock) {
    BlockId id{0};
    r_print_block(id, basicBlock);
}

void translate_global_decl(Node *decl) {
    if (decl->decl.rval->kind == NodeKind::kFunc) {
        if (decl->decl.rval->func.body->kind == NodeKind::kBlock) {
            BasicBlock *block = ssa::translate_function_body(decl->decl.rval->func.body);
            mem::p_free(decl->decl.rval);
            print_block(block);
        }
    }
}
}  // namespace

void translate_package(Package *package) {
    for (size_t i = 0; i < package->globalDeclList.size; i++) {
        translate_global_decl(package->globalDeclList.get(i)->declNode);
    }
}
}  // namespace lcc
