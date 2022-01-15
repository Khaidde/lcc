#include "translate.hpp"

#include <cstdio>

#include "ast.hpp"
#include "print.hpp"

namespace lcc {

namespace {

namespace ssa {

struct TranslationContext {
    BlockId blockCnt{0};
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

/*
void translate_expr(BasicBlock *curr, Node *expr) {
assert(expr && curr);
switch (expr->kind) {
    case NodeKind::kIntLit: {
        IrInst *inst = mem::malloc<IrInst>();
        curr->insts.add(inst);
        break;
    }
    default: assert(false && "TODO: no translation implemented for expression kind\n"); break;
}
}
*/

void translate_block(TranslationContext &tctx, BasicBlock *entry, BasicBlock *exit, Node *block) {
    assert(block->kind == NodeKind::kBlock);

    BasicBlock *curr = entry;
    for (size_t i = 0; i < block->block.stmts.size; i++) {
        curr->insts.size++;
        Node *stmt = block->block.stmts.get(i);
        switch (stmt->kind) {
            case NodeKind::kDecl: {
                if (stmt->decl.isDecl) {
                    // translate_expr(curr, stmt->decl.rval);
                } else {
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
    void try_add(size_t val) {
        assert(size < capacity);
        assert(val < valLim);

        if (contains(val)) return;
        dense[size] = val;
        sparse[val] = size++;
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

void generate_dominance_frontier(BasicBlock *entry, size_t maxBlockId) {
    assert(entry && maxBlockId > 0);

    // Fast dominator algorithm by Lengauer and Tarjan
    // Uses the "sophisticated" eval and link methods

    // Allocate memory for data structures
    size_t numNodes = maxBlockId + 2;
    size_t arenaSize = sizeof(size_t) * numNodes * 8 + sizeof(LList<size_t>) * numNodes * 2;
    void *arena = mem::c_malloc<int8_t>(arenaSize);

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

    SparseSet *domFronts = mem::c_malloc<SparseSet>(numNodes);
    for (size_t i = 0; i < numNodes; i++) {
        domFronts[i].init(numNodes, numNodes);
    }

    // Simple Dominance Frontier algorithm by Cooper et al.
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
    for (size_t i = 0; i < numNodes; i++) {
        printf("%2d:{", i);
        for (size_t k = 0; k < domFronts[i].size; k++) {
            printf("%2d", domFronts[i].dense[k]);
            if (k < domFronts[i].size - 1) {
                printf(",");
            }
        }
        printf("}\n");
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
    BasicBlock *entry = create_block(tctx);
    BasicBlock *exit = create_block();
    translate_block(tctx, entry, exit, functionBody);
    exit->id = tctx.blockCnt++;
    generate_dominance_frontier(entry, tctx.blockCnt);
    return entry;
}

}  // namespace ssa

}  // namespace

void print_block(BlockId &bid, BasicBlock *basicBlock) {
    printf("b%d(%d)\n", basicBlock->id, basicBlock->insts.size);
    bid++;
    for (size_t i = 0; i < basicBlock->exits.size; i++) {
        printf("->%d\n", basicBlock->exits.get(i)->id);
    }
    for (size_t i = 0; i < basicBlock->exits.size; i++) {
        BasicBlock *exit = basicBlock->exits.get(i);
        if (bid == exit->id) print_block(bid, exit);
    }
}

void translate_decl(Node *decl) {
    if (decl->decl.rval->kind == NodeKind::kFunc) {
        if (decl->decl.rval->func.body->kind == NodeKind::kBlock) {
            ssa::translate_function_body(decl->decl.rval->func.body);
        }
    }
}

/*
void translate_global_decl_list(struct Node *declListHead) {
IrContext irctx;
irctx.basicBlocks = {};
while (declListHead) {
    if (declListHead->decl.resolvedTy->kind == TypeKind::kFuncTy) {
        if (declListHead->decl.rval->func.body->kind == NodeKind::kBlock) {
            irctx.basicBlocks.add(ssa::translate_function_body(declListHead->decl.rval));
        }
    }
    declListHead = declListHead->decl.info->nextDecl;
}

BasicBlock bt[14];
for (size_t i = 0; i < 14; i++) {
    bt[i].id = i;
    bt[i].exits = {};
}
bt[1].exits.add(&bt[2]);
bt[1].exits.add(&bt[5]);
bt[1].exits.add(&bt[9]);
bt[2].exits.add(&bt[3]);
bt[3].exits.add(&bt[3]);
bt[3].exits.add(&bt[4]);
bt[4].exits.add(&bt[13]);
bt[5].exits.add(&bt[6]);
bt[5].exits.add(&bt[7]);
bt[6].exits.add(&bt[4]);
bt[6].exits.add(&bt[8]);
bt[7].exits.add(&bt[8]);
bt[7].exits.add(&bt[12]);
bt[8].exits.add(&bt[5]);
bt[8].exits.add(&bt[13]);
bt[9].exits.add(&bt[10]);
bt[9].exits.add(&bt[11]);
bt[10].exits.add(&bt[12]);
bt[11].exits.add(&bt[12]);
bt[12].exits.add(&bt[13]);
ssa::generate_dominance_frontier(&bt[1], 14);
// BlockId bid = 0;
// print_block(bid, &bt[1]);

for (size_t i = 0; i < irctx.basicBlocks.size; i++) {
    BlockId bid = 0;
    print_block(bid, irctx.basicBlocks.get(i));
}
}
*/

}  // namespace lcc
