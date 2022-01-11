#include "translate.hpp"

#include <cstdio>

#include "ast.hpp"
#include "print.hpp"

namespace lcc {

namespace {

// Fast dominator algorithm by Lengauer and Tarjan
// Uses the "sophisticated" eval and link methods
namespace dominator {

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

BasicBlock **generate_idoms(BasicBlock *entry, size_t numBlocks) {
    assert(entry && numBlocks > 0);
    size_t numNodes = numBlocks + 1;

    // Allocate memory for data structures
    BasicBlock **idom = mem::c_malloc<BasicBlock *>(numBlocks);
    idom[0] = nullptr;

    size_t arenaSize = sizeof(BasicBlock *) * numNodes;
    arenaSize += sizeof(size_t) * numNodes * 6;
    arenaSize += sizeof(LList<size_t>) * numNodes * 2;
    void *arena = mem::c_malloc<int8_t>(arenaSize);

    BasicBlock **vertex = (BasicBlock **)arena;
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
    vertex[0] = nullptr;
    dfctx.size[0] = 0;

    // Iterative DFS numbering
    LList<BasicBlock *> stack = {};
    stack.add(entry);
    size_t semiCnt = 0;
    while (stack.size) {
        BasicBlock *top = stack.get(stack.size - 1);
        stack.size--;
        size_t tid = top->id + 1;
        if (!semi[tid]) {
            semi[tid] = ++semiCnt;
            vertex[semiCnt] = top;
            dfctx.label[tid] = tid;
            for (size_t i = 0; i < top->exits.size; i++) {
                BasicBlock *p = top->exits.get(i);
                size_t pn = p->id + 1;
                pred[pn].add(tid);
                if (!semi[pn]) {
                    parent[pn] = tid;
                    stack.add(p);
                }
            }
        }
    }

    for (size_t i = numBlocks; i >= 2; i--) {
        // Compute semi-dominators
        size_t currN = vertex[i]->id + 1;
        for (size_t k = 0; k < pred[currN].size; k++) {
            size_t en = eval(semi, dfctx, pred[currN].get(k));
            if (semi[en] < semi[currN]) {
                semi[currN] = semi[en];
            }
        }
        bucket[vertex[semi[currN]]->id + 1].add(currN);
        size_t parenN = parent[currN];
        link(semi, dfctx, parenN, currN);

        // Implicitly define immediate dominator for each vertex
        LList<size_t> &parentBucket = bucket[parenN];
        for (size_t i = 0; i < parentBucket.size; i++) {
            size_t vn = parentBucket.get(i);
            size_t en = eval(semi, dfctx, vn);
            if (semi[en] < semi[vn]) {
                idom[vn - 1] = vertex[semi[en]];
            } else {
                idom[vn - 1] = vertex[semi[parenN]];
            }
        }
        parentBucket.size = 0;
    }
    // Explicitly define immediate dominators
    for (size_t i = 2; i <= numBlocks; i++) {
        BasicBlock *curr = vertex[i];
        if (idom[curr->id] != vertex[semi[curr->id + 1]]) {
            idom[curr->id] = idom[idom[curr->id + 1]->id + 1];
        }
    }

    // Free data structures
    for (size_t i = 0; i < numBlocks; i++) {
        mem::c_free(pred[i].data);
        mem::c_free(bucket[i].data);
    }
    mem::c_free((void *)((int8_t *)arena - arenaSize));
    return idom;
}

}  // namespace dominator

BasicBlock *create_block() {
    BasicBlock *block = mem::malloc<BasicBlock>();
    block->insts = {};
    block->exits = {};
    return block;
}

BasicBlock *create_block(BlockId &id) {
    BasicBlock *block = create_block();
    block->id = id++;
    return block;
}

void translate_block(BlockId &idCnt, BasicBlock *entry, BasicBlock *exit, Node *block);

void translate_if(BlockId &idCnt, BasicBlock *entry, BasicBlock *exit, Node *ifstmt) {
    for (;;) {
        BasicBlock *then = create_block(idCnt);
        entry->exits.add(then);
        translate_block(idCnt, then, exit, ifstmt->ifstmt.then);

        if (!ifstmt->ifstmt.alt) {
            entry->exits.add(exit);
            return;
        }

        if (ifstmt->ifstmt.alt->kind == NodeKind::kBlock) {
            BasicBlock *alt = create_block(idCnt);
            entry->exits.add(alt);
            translate_block(idCnt, alt, exit, ifstmt->ifstmt.alt);
            return;
        }

        assert(ifstmt->ifstmt.alt->kind == NodeKind::kIf);
        ifstmt = ifstmt->ifstmt.alt;
    }
}

void translate_while(BlockId &idCnt, BasicBlock *entry, BasicBlock *exit, Node *whilestmt) {
    BasicBlock *cond = create_block(idCnt);
    entry->exits.add(cond);

    // Goto targets for break and continue statements
    whilestmt->whilestmt.info->entry = cond;
    whilestmt->whilestmt.info->exit = exit;

    BasicBlock *loop = create_block(idCnt);
    cond->exits.add(loop);
    cond->exits.add(exit);
    translate_block(idCnt, loop, cond, whilestmt->whilestmt.loop);
}

void translate_block(BlockId &idCnt, BasicBlock *entry, BasicBlock *exit, Node *block) {
    assert(block->kind == NodeKind::kBlock);

    BasicBlock *curr = entry;
    for (size_t i = 0; i < block->block.stmts.size; i++) {
        curr->insts.size++;
        Node *stmt = block->block.stmts.get(i);
        switch (stmt->kind) {
            case NodeKind::kIf: {
                if (i < block->block.stmts.size - 1) {
                    BasicBlock *ifExit = create_block();
                    translate_if(idCnt, curr, ifExit, stmt);
                    ifExit->id = idCnt++;
                    curr = ifExit;
                } else {
                    translate_if(idCnt, curr, exit, stmt);
                    return;
                }
                break;
            }
            case NodeKind::kWhile: {
                if (i < block->block.stmts.size - 1) {
                    BasicBlock *whileExit = create_block();
                    translate_while(idCnt, curr, whileExit, stmt);
                    whileExit->id = idCnt++;
                    curr = whileExit;
                    break;
                } else {
                    translate_while(idCnt, curr, exit, stmt);
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

BasicBlock *translate_function_body(BlockId &idCnt, Node *function) {
    BasicBlock *entry = create_block(idCnt);
    BasicBlock *exit = create_block();
    translate_block(idCnt, entry, exit, function->func.body);
    exit->id = idCnt;
    return entry;
}

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

void translate_global_decl_list(struct Node *declListHead) {
    IrContext irctx;
    irctx.basicBlocks = {};
    while (declListHead) {
        if (declListHead->decl.info->resolvedTy->kind == TypeKind::kFuncTy) {
            if (declListHead->decl.rval->func.body->kind == NodeKind::kBlock) {
                BlockId idCnt = 0;  // TODO: move this into some kind of context structure
                irctx.basicBlocks.add(translate_function_body(idCnt, declListHead->decl.rval));
                BasicBlock **idom = dominator::generate_idoms(irctx.basicBlocks.get(0), idCnt + 1);
                for (size_t i = 1; i < idCnt + 1; i++) {
                    if (idom[i]) printf("dom:%d-%d\n", i, idom[i]->id);
                }
            }
        }
        declListHead = declListHead->decl.info->nextDecl;
    }

    for (size_t i = 0; i < irctx.basicBlocks.size; i++) {
        BlockId bid = 0;
        print_block(bid, irctx.basicBlocks.get(i));
    }
}

}  // namespace lcc
