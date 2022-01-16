#include "translate.hpp"

#include <cstdio>

#include "ast.hpp"
#include "print.hpp"

namespace lcc {

namespace {

namespace ssa {

struct AssignInfo {
    IrInst *inst;
};

/*
uint32_t val_hash(ValId &val) { return val; }

bool val_equal(ValId &a, ValId &b) { return a == b; }
*/

struct TranslationContext {
    BlockId blockCnt{0};
    ValId valCnt{0};

    // LMap<ValId, AssignInfo, val_hash, val_equal> assigns;
    // LMap<LStringView, ValId, lstr_hash, lstr_equal> varnameId;

    LMap<LStringView, LList<BlockId> *, lstr_hash, lstr_equal> defBlocks;
};

BasicBlock *create_block() {
    BasicBlock *block = mem::malloc<BasicBlock>();
    block->insts = {};
    block->exits = {};
    return block;
}

BasicBlock *create_block(TranslationContext &tctx) {
    BasicBlock *block = create_block();
    block->id = tctx.blockCnt++;
    return block;
}

void translate_block(TranslationContext &tctx, BasicBlock *entry, BasicBlock *exit, Node *block);

void translate_if(TranslationContext &tctx, BasicBlock *entry, BasicBlock *exit, Node *ifstmt) {
    for (;;) {
        BasicBlock *then = create_block(tctx);
        entry->exits.add(then);
        translate_block(tctx, then, exit, ifstmt->ifstmt.then);

        if (!ifstmt->ifstmt.alt) {
            entry->exits.add(exit);
            return;
        }

        if (ifstmt->ifstmt.alt->kind == NodeKind::kBlock) {
            BasicBlock *alt = create_block(tctx);
            entry->exits.add(alt);
            translate_block(tctx, alt, exit, ifstmt->ifstmt.alt);
            return;
        }

        assert(ifstmt->ifstmt.alt->kind == NodeKind::kIf);
        ifstmt = ifstmt->ifstmt.alt;
    }
}

void translate_while(TranslationContext &tctx, BasicBlock *entry, BasicBlock *exit, Node *whilestmt) {
    BasicBlock *cond = create_block(tctx);
    entry->exits.add(cond);

    // Goto targets for break and continue statements
    whilestmt->whilestmt.info->entry = cond;
    whilestmt->whilestmt.info->exit = exit;

    BasicBlock *loop = create_block(tctx);
    cond->exits.add(loop);
    cond->exits.add(exit);
    translate_block(tctx, loop, cond, whilestmt->whilestmt.loop);
}

IrInst *create_inst(IrInstKind kind) {
    IrInst *inst = mem::malloc<IrInst>();
    inst->kind = kind;
    return inst;
}

/*
ValId translate_expr(TranslationContext &tctx, BasicBlock *curr, Node *expr) {
switch (expr->kind) {
case NodeKind::kIntLit: {
    IrInst *constInst = create_inst(IrInstKind::kConst);
    constInst->aconst.intVal = expr->intLit.intVal;
    curr->insts.add(constInst);
    return constInst->aconst.dest = tctx.valCnt++;
}
case NodeKind::kName: return *tctx.varnameId.get(expr->name.ident);
case NodeKind::kInfix: {
    IrInst *binInst = create_inst(IrInstKind::kBin);
    binInst->abin.op = expr->infix.op;
    binInst->abin.left = translate_expr(tctx, curr, expr->infix.left);
    binInst->abin.right = translate_expr(tctx, curr, expr->infix.right);
    curr->insts.add(binInst);
    return binInst->abin.dest = tctx.valCnt++;
}
default: assert(false && "TODO: no translation implemented for expression kind\n"); return 0;
}
}
*/

void translate_block(TranslationContext &tctx, BasicBlock *entry, BasicBlock *exit, Node *block) {
    assert(block->kind == NodeKind::kBlock);

    BasicBlock *curr = entry;
    for (size_t i = 0; i < block->block.stmts.size; i++) {
        Node *stmt = block->block.stmts.get(i);
        switch (stmt->kind) {
            case NodeKind::kDecl: {
                if (stmt->decl.lval->kind == NodeKind::kName) {
                    if (!stmt->decl.isDecl && stmt->decl.isAssignToGlobal) {
                        todo("Assignment to global '%s'\n", lstr_raw_str(stmt->decl.lval->name.ident));
                        assert(false);
                    }

                    LList<BlockId> *blockList;
                    if (LList<BlockId> **ptrBlockDefs = tctx.defBlocks.get(stmt->decl.lval->name.ident)) {
                        blockList = *ptrBlockDefs;
                    } else {
                        blockList = mem::malloc<LList<BlockId>>();
                        *blockList = {};
                        tctx.defBlocks.try_put(stmt->decl.lval->name.ident, blockList);
                    }
                    // Ensure no duplicates before addition
                    if (!blockList->size || blockList->last() != curr->id) {
                        blockList->add(curr->id);
                    }
                }
                break;
            }
            case NodeKind::kIf: {
                if (i < block->block.stmts.size - 1) {
                    BasicBlock *ifExit = create_block();
                    translate_if(tctx, curr, ifExit, stmt);
                    ifExit->id = tctx.blockCnt++;
                    curr = ifExit;
                } else {
                    translate_if(tctx, curr, exit, stmt);
                    return;
                }
                break;
            }
            case NodeKind::kWhile: {
                if (i < block->block.stmts.size - 1) {
                    BasicBlock *whileExit = create_block();
                    translate_while(tctx, curr, whileExit, stmt);
                    whileExit->id = tctx.blockCnt++;
                    curr = whileExit;
                    break;
                } else {
                    translate_while(tctx, curr, exit, stmt);
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
}

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
    void try_add(size_t val) {
        assert(size < capacity);
        assert(val < valLim);

        if (contains(val)) return;
        dense[size] = val;
        sparse[val] = size++;
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

void insert_phi_nodes(TranslationContext &tctx, BasicBlock *entry) {
    assert(entry && tctx.blockCnt > 0);

    // Fast dominator algorithm by Lengauer and Tarjan
    // Uses the "sophisticated" eval and link methods

    // Allocate memory for data structures
    size_t numNodes = tctx.blockCnt + 1;
    size_t arenaSize = sizeof(BasicBlock *) * numNodes;
    arenaSize += sizeof(size_t) * numNodes * 8;
    arenaSize += sizeof(LList<size_t>) * numNodes * 2;
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
    arena = (void *)&bucket[numNodes];

    // Initialize data structures
    for (size_t i = 0; i < numNodes; i++) {
        semi[i] = 0;
        dfctx.ancestor[i] = 0;
        dfctx.child[i] = 0;
        dfctx.size[i] = 1;
        pred[i] = {};
        bucket[i] = {};
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
    for (size_t i = 0; i < tctx.defBlocks.capacity; i++) {
        // TODO: more efficient iteration of variable name deflists
        if (tctx.defBlocks.table[i].psl) {
            worklist.clear();
            philist.clear();
            LList<BlockId> *deflist = tctx.defBlocks.table[i].val;
            for (size_t k = 0; k < deflist->size; k++) {
                worklist.try_add(deflist->get(k) + 1);
            }
            while (worklist.size) {
                size_t def = worklist.pop();
                for (size_t k = 0; k < domFronts[def].size; k++) {
                    size_t dfy = domFronts[def].dense[k];
                    if (!philist.contains(dfy)) {
                        IrInst *phi = create_inst(IrInstKind::kPhi);
                        phi->phi.dest = tctx.valCnt++;
                        phi->phi.in.init(pred[dfy].size);
                        blocks[dfy]->insts.add(phi);
                        philist.try_add(dfy);
                        // TODO: optimization where a node shouldn't be added if the variable
                        // is already defined there
                        if (dfy != def) worklist.try_add(dfy);
                    }
                }
            }
        }
    }

    // Free data structures
    for (size_t i = 0; i < numNodes; i++) {
        mem::c_free(pred[i].data);
        mem::c_free(bucket[i].data);
    }
    mem::c_free((void *)((int8_t *)arena - arenaSize));
}

BasicBlock *translate_function_body(Node *functionBody) {
    TranslationContext tctx{};
    tctx.defBlocks.init();

    BasicBlock *entry = create_block(tctx);
    BasicBlock *exit = create_block();
    translate_block(tctx, entry, exit, functionBody);
    exit->id = tctx.blockCnt++;
    insert_phi_nodes(tctx, entry);

    mem::c_free(tctx.defBlocks.table);
    return entry;
}

}  // namespace ssa

void r_print_block(BlockId &bid, BasicBlock *basicBlock) {
    printf("b%d\n", basicBlock->id, basicBlock->insts.size);
    for (size_t i = 0; i < basicBlock->insts.size; i++) {
        IrInst *inst = basicBlock->insts.get(i);
        switch (inst->kind) {
            case IrInstKind::kPhi:
                printf("  v%d = phi(", inst->phi.dest);
                for (size_t k = 0; k < inst->phi.in.capacity; k++) {
                    printf("?");
                    if (k < inst->phi.in.capacity - 1) {
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
            default: todo("Unimplemented instruction print\n"); assert(false);
        }
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
