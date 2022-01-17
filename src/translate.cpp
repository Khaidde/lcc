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

struct VariableInfo {
    size_t varId;
    LList<BlockId> defSites;
    ValId currId;
    LList<ValId> idStack;
};

struct TranslationContext {
    BlockId blockCnt{0};
    ValId valCnt{0};
    LList<BasicBlock *> *dominators;
    LList<StatementListNode *> blockEntryStmts;
    LMap<LStringView, VariableInfo *, lstr_hash, lstr_equal> varMap;
    SparseSet liveVarSet;
};

BasicBlock *create_block() {
    BasicBlock *block = mem::malloc<BasicBlock>();
    block->start = nullptr;
    block->end = nullptr;
    block->exits = {};
    return block;
}

BasicBlock *create_block(TranslationContext &tctx) {
    BasicBlock *block = create_block();
    block->id = tctx.blockCnt++;
    return block;
}

void cfg_block(TranslationContext &tctx, BasicBlock *entry, BasicBlock *exit, Node *block);

void cfg_if(TranslationContext &tctx, BasicBlock *entry, BasicBlock *exit, Node *ifstmt) {
    for (;;) {
        BasicBlock *then = create_block(tctx);
        entry->exits.add(then);
        cfg_block(tctx, then, exit, ifstmt->ifstmt.then);
        if (!ifstmt->ifstmt.alt) {
            entry->exits.add(exit);
            return;
        }
        if (ifstmt->ifstmt.alt->kind == NodeKind::kBlock) {
            BasicBlock *alt = create_block(tctx);
            entry->exits.add(alt);
            cfg_block(tctx, alt, exit, ifstmt->ifstmt.alt);
            return;
        }
        assert(ifstmt->ifstmt.alt->kind == NodeKind::kIf);
        ifstmt = ifstmt->ifstmt.alt;
    }
}

void cfg_while(TranslationContext &tctx, BasicBlock *entry, BasicBlock *exit, Node *whilestmt) {
    // Goto targets for break and continue statements
    whilestmt->whilestmt.info->entry = entry;
    whilestmt->whilestmt.info->exit = exit;
    BasicBlock *loop = create_block(tctx);
    entry->exits.add(loop);
    entry->exits.add(exit);
    cfg_block(tctx, loop, entry, whilestmt->whilestmt.loop);
}

void cfg_block(TranslationContext &tctx, BasicBlock *entry, BasicBlock *exit, Node *block) {
    assert(block->kind == NodeKind::kBlock);
    BasicBlock *curr = entry;
    for (StatementListNode *stmtNode = block->block.start; stmtNode; stmtNode = stmtNode->next) {
        if (tctx.blockEntryStmts.size < tctx.blockCnt) tctx.blockEntryStmts.add(stmtNode);
        Node *stmt = stmtNode->stmt;
        switch (stmt->kind) {
            case NodeKind::kDecl: {
                if (stmt->decl.lval->kind == NodeKind::kName) {
                    LList<BlockId> *defsites;
                    if (VariableInfo **varInfo = tctx.varMap.get(stmt->decl.lval->name.ident)) {
                        defsites = &(*varInfo)->defSites;
                    } else {
                        VariableInfo *newInfo = mem::malloc<VariableInfo>();
                        newInfo->varId = tctx.varMap.size;
                        newInfo->defSites = {};
                        newInfo->idStack = {};
                        defsites = &newInfo->defSites;
                        tctx.varMap.try_put(stmt->decl.lval->name.ident, newInfo);
                    }
                    // Ensure no duplicates before addition
                    if (!defsites->size || defsites->last() != curr->id) {
                        defsites->add(curr->id);
                    }
                }
                break;
            }
            case NodeKind::kIf: {
                if (stmtNode->next) {
                    BasicBlock *ifExit = create_block();
                    cfg_if(tctx, curr, ifExit, stmt);
                    ifExit->id = tctx.blockCnt++;
                    curr = ifExit;
                } else {
                    cfg_if(tctx, curr, exit, stmt);
                    return;
                }
                break;
            }
            case NodeKind::kWhile: {
                if (stmtNode != block->block.start) {
                    BasicBlock *cond = create_block(tctx);
                    tctx.blockEntryStmts.add(stmtNode);
                    curr->exits.add(cond);
                    curr = cond;
                }
                if (stmtNode->next) {
                    BasicBlock *whileExit = create_block();
                    cfg_while(tctx, curr, whileExit, stmt);
                    whileExit->id = tctx.blockCnt++;
                    curr = whileExit;
                    break;
                } else {
                    cfg_while(tctx, curr, exit, stmt);
                    return;
                }
            }
            case NodeKind::kLoopBr: {
                if (stmt->loopbr.isBreak) {
                    curr->exits.add(stmt->loopbr.ref->whilestmt.info->exit);
                } else {
                    curr->exits.add(stmt->loopbr.ref->whilestmt.info->entry);
                }
                return;
            }
            default: break;
        }
    }
    curr->exits.add(exit);
    mem::p_free<Node>(block);
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

IrInst *create_inst(IrInstKind kind) {
    IrInst *inst = mem::malloc<IrInst>();
    inst->kind = kind;
    inst->next = nullptr;
    return inst;
}

void add_inst(BasicBlock *block, IrInst *inst) {
    if (block->end) {
        block->end->next = inst;
    } else {
        block->start = inst;
    }
    block->end = inst;
}

ValId translate_expr(TranslationContext &tctx, BasicBlock *curr, Node *expr) {
    switch (expr->kind) {
        case NodeKind::kIntLit: {
            IrInst *aconst = create_inst(IrInstKind::kConst);
            aconst->aconst.intVal = expr->intLit.intVal;
            add_inst(curr, aconst);
            mem::p_free<Node>(expr);
            return aconst->aconst.dest = tctx.valCnt++;
        }
        case NodeKind::kName: {
            if (expr->name.ref->decl.isAssignToGlobal) {
                todo("Use of global variable '%s'\n", lstr_raw_str(expr->name.ident));
                assert(false);
            }
            VariableInfo *varInfo = *tctx.varMap.get(expr->name.ident);
            mem::p_free<Node>(expr);
            return varInfo->currId;
        }
        case NodeKind::kInfix: {
            IrInst *bin = create_inst(IrInstKind::kBin);
            bin->bin.op = expr->infix.op;
            bin->bin.left = translate_expr(tctx, curr, expr->infix.left);
            bin->bin.right = translate_expr(tctx, curr, expr->infix.right);
            add_inst(curr, bin);
            mem::p_free<Node>(expr);
            return bin->bin.dest = tctx.valCnt++;
        }
        default: assert(false && "TODO: no translation implemented for expression kind"); return 0;
    }
}

void translate_block(TranslationContext &tctx, BasicBlock *curr) {
    if (curr->id >= tctx.blockCnt - 1) return;
    VariableInfo **liveVarList = mem::c_malloc<VariableInfo *>(tctx.varMap.size);
    for (IrInst *phi = curr->start; phi; phi = phi->next) {
        VariableInfo *varInfo = *tctx.varMap.get(phi->phi.varName);
        varInfo->currId = phi->phi.dest = tctx.valCnt++;
        tctx.liveVarSet.try_add(varInfo->varId);
        liveVarList[tctx.liveVarSet.size - 1] = varInfo;
    }
    for (StatementListNode *stmtNode = tctx.blockEntryStmts.get(curr->id); stmtNode; stmtNode = stmtNode->next) {
        Node *stmt = stmtNode->stmt;
        if (stmt->kind == NodeKind::kWhile) {
            break;
        }
        if (stmt->kind == NodeKind::kIf) {
            IrInst *cond = create_inst(IrInstKind::kCond);
            cond->cond.cond = translate_expr(tctx, curr, stmt->ifstmt.cond);
            cond->cond.then = curr->exits.get(0);
            add_inst(curr, cond);
            mem::p_free<Node>(stmt);
            break;
        }
        switch (stmt->kind) {
            case NodeKind::kDecl: {
                if (!stmt->decl.isDecl && stmt->decl.isAssignToGlobal) {
                    todo("Assignment to global '%s'\n", lstr_raw_str(stmt->decl.lval->name.ident));
                    assert(false);
                }
                ValId declId = translate_expr(tctx, curr, stmt->decl.rval);
                if (stmt->decl.lval->kind == NodeKind::kName) {
                    VariableInfo *varInfo = *tctx.varMap.get(stmt->decl.lval->name.ident);
                    varInfo->currId = declId;
                    if (tctx.liveVarSet.try_add(varInfo->varId)) {
                        liveVarList[tctx.liveVarSet.size - 1] = varInfo;
                    }
                    mem::p_free<Node>(stmt->decl.lval);
                } else {
                    todo("Unimplemented translation of assignment with non-name lval\n");
                    assert(false);
                }
                break;
            }
            default: break;
        }
        mem::p_free<Node>(stmt);
    }
    // Add joins to phi nodes
    for (size_t i = 0; i < curr->exits.size; i++) {
        for (IrInst *phi = curr->exits.get(i)->start; phi && phi->kind == IrInstKind::kPhi; phi = phi->next) {
            VariableInfo *varInfo = *tctx.varMap.get(phi->phi.varName);
            phi->phi.joins.add(varInfo->currId);
        }
    }
    // Save value id of live variables
    size_t numLiveVars = tctx.liveVarSet.size;
    for (size_t i = 0; i < numLiveVars; i++) {
        liveVarList[i]->idStack.add(liveVarList[i]->currId);
    }
    tctx.liveVarSet.clear();
    // Translate dominator children
    for (size_t i = 0; i < tctx.dominators[curr->id + 1].size; i++) {
        translate_block(tctx, tctx.dominators[curr->id + 1].get(i));
    }
    // Restore value id of live variables
    for (size_t i = 0; i < numLiveVars; i++) {
        liveVarList[i]->idStack.size--;
        if (liveVarList[i]->idStack.size) {
            liveVarList[i]->currId = liveVarList[i]->idStack.last();
        }
    }
    mem::c_free(liveVarList);
}

BasicBlock *translate_function_body(Node *functionBody) {
    TranslationContext tctx{};
    tctx.blockEntryStmts = {};
    tctx.varMap.init();
    BasicBlock *entry = create_block(tctx);
    BasicBlock *exit = create_block();
    cfg_block(tctx, entry, exit, functionBody);
    exit->id = tctx.blockCnt++;
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
        for (size_t i = 0; i < top->exits.size; i++) {
            size_t pn = top->exits.get(i)->id + 1;
            pred[pn].add(tn);
            if (semi[pn]) continue;
            parent[pn] = tn;
            stack.add(top->exits.get(i));
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
    for (size_t i = 0; i < tctx.varMap.capacity; i++) {
        // TODO: more efficient iteration of variable name deflists
        if (tctx.varMap.table[i].psl) {
            worklist.clear();
            philist.clear();
            VariableInfo *var = tctx.varMap.table[i].val;
            for (size_t k = 0; k < var->defSites.size; k++) {
                worklist.try_add(var->defSites.get(k) + 1);
            }
            while (worklist.size) {
                size_t def = worklist.pop();
                for (size_t k = 0; k < domFronts[def].size; k++) {
                    size_t dfy = domFronts[def].dense[k];
                    if (!philist.contains(dfy)) {
                        IrInst *phi = create_inst(IrInstKind::kPhi);
                        phi->phi.varName = tctx.varMap.table[i].key;
                        phi->phi.joins.init(pred[dfy].size);
                        add_inst(blocks[dfy], phi);
                        philist.try_add(dfy);
                        // TODO: optimization where a node shouldn't be added if the variable
                        // is already defined there
                        if (dfy != def) worklist.try_add(dfy);
                    }
                }
            }
        }
    }
    // Translation
    tctx.liveVarSet.init(tctx.varMap.size, tctx.varMap.size);
    translate_block(tctx, entry);
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
    IrInst *inst = basicBlock->start;
    while (inst) {
        switch (inst->kind) {
            case IrInstKind::kPhi:
                printf("  v%d = phi(", inst->phi.dest);
                for (size_t k = 0; k < inst->phi.joins.size; k++) {
                    printf("v%d", inst->phi.joins.get(k));
                    if (k < inst->phi.joins.size - 1) {
                        printf(", ");
                    }
                }
                printf(")\n");
                break;
            case IrInstKind::kConst: printf("  v%d = %d\n", inst->aconst.dest, inst->aconst.intVal); break;
            case IrInstKind::kBin:
                const char *opstr;
                switch (inst->bin.op) {
                    case TokenType::kAdd: opstr = "add"; break;
                    default: opstr = "?";
                }
                printf("  v%d = %s(v%d, v%d)\n", inst->bin.dest, opstr, inst->bin.left, inst->bin.right);
                break;
            case IrInstKind::kCond:
                printf("  if v%d -> b%d", inst->cond.cond, inst->cond.then->id);
                if (inst->cond.alt) {
                    printf(" else b%d", inst->cond.alt->id);
                }
                printf("\n");
                break;
            default: todo("Unimplemented instruction print\n"); assert(false);
        }
        inst = inst->next;
    }
    bid++;
    for (size_t i = 0; i < basicBlock->exits.size; i++) {
        printf("->%d\n", basicBlock->exits.get(i)->id);
    }
    for (size_t i = 0; i < basicBlock->exits.size; i++) {
        BasicBlock *exit = basicBlock->exits.get(i);
        if (bid == exit->id) r_print_block(bid, exit);
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
