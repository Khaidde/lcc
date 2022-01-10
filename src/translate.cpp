#include "translate.hpp"

#include <cstdio>

#include "ast.hpp"
#include "print.hpp"

namespace lcc {

namespace {

void dom_link(LList<BasicBlock *> &ancestor, BasicBlock *parent, BasicBlock *child) {
    ancestor.get(child->id) = parent;
}

BasicBlock *dom_eval(LList<size_t> &semidom, LList<BasicBlock *> &ancestor, BasicBlock *block) {
    BasicBlock *curr = ancestor.get(block->id);
    while (curr) {
        if (semidom.get(block->id) > semidom.get(curr->id)) {
            block = curr;
        }
        curr = ancestor.get(curr->id);
    }
    return block;
}

// Fast dominator algorithm by Lengauer and Tarjan
// Uses the "simple" eval and link methods
LList<BasicBlock *> generate_immediate_dominators(BasicBlock *entry, size_t numBlocks) {
    assert(entry && numBlocks > 0);

    // Initialize data structures
    // TODO: optimize space by manually allocating arrays instead of using LList
    LList<size_t> semidom;
    LList<BasicBlock *> ancestor, label, parent, vertex, idom;
    LList<LList<BasicBlock *>> predecessor, bucket;
    semidom.init(numBlocks);
    ancestor.init(numBlocks);
    label.init(numBlocks);
    parent.init(numBlocks);
    vertex.init(numBlocks + 1);
    idom.init(numBlocks);
    predecessor.init(numBlocks);
    bucket.init(numBlocks);

    label.size = numBlocks;
    parent.size = numBlocks;
    vertex.add(nullptr);
    idom.add(nullptr);
    idom.size = numBlocks;
    for (size_t i = 0; i < numBlocks; i++) {
        semidom.add(0);
        ancestor.add(nullptr);
        predecessor.add({});
        bucket.add({});
    }

    // Iterative DFS numbering
    LList<BasicBlock *> stack{};
    stack.add(entry);
    size_t semiCnt = 0;
    while (stack.size) {
        BasicBlock *top = stack.get(stack.size - 1);
        stack.size--;
        if (!semidom.get(top->id)) {
            semidom.get(top->id) = ++semiCnt;
            vertex.add(top);
            label.get(top->id) = top;
            for (size_t i = 0; i < top->exits.size; i++) {
                BasicBlock *pred = top->exits.get(i);
                predecessor.get(pred->id).add(top);
                if (!semidom.get(pred->id)) {
                    parent.get(pred->id) = top;
                    stack.add(pred);
                }
            }
        }
    }

    for (size_t i = numBlocks; i >= 2; i--) {
        // Compute semi-dominators
        BasicBlock *curr = vertex.get(i);
        for (size_t k = 0; k < predecessor.get(curr->id).size; k++) {
            BasicBlock *eval = dom_eval(semidom, ancestor, predecessor.get(curr->id).get(k));
            if (semidom.get(eval->id) < semidom.get(curr->id)) {
                semidom.get(curr->id) = semidom.get(eval->id);
            }
        }
        bucket.get(vertex.get(semidom.get(curr->id))->id).add(curr);
        BasicBlock *currParent = parent.get(curr->id);
        dom_link(ancestor, currParent, curr);

        // Implicitly define immediate dominator for each vertex
        LList<BasicBlock *> &parentBucket = bucket.get(currParent->id);
        for (size_t i = 0; i < parentBucket.size; i++) {
            BasicBlock *eval = dom_eval(semidom, ancestor, parentBucket.get(i));
            if (semidom.get(eval->id) < semidom.get(parentBucket.get(i)->id)) {
                idom.get(parentBucket.get(i)->id) = eval;
            } else {
                idom.get(parentBucket.get(i)->id) = currParent;
            }
        }
        parentBucket.size = 0;
    }
    // Explicitly define immediate dominators
    for (size_t i = 2; i <= numBlocks; i++) {
        BasicBlock *curr = vertex.get(i);
        if (idom.get(curr->id) != vertex.get(semidom.get(curr->id))) {
            idom.get(curr->id) = idom.get(idom.get(curr->id)->id);
        }
    }

    // Free data structures
    mem::c_free(semidom.data);
    mem::c_free(ancestor.data);
    mem::c_free(label.data);
    mem::c_free(parent.data);
    mem::c_free(vertex.data);
    for (size_t i = 0; i < predecessor.size; i++) {
        mem::c_free(predecessor.get(i).data);
        mem::c_free(bucket.get(i).data);
    }
    mem::c_free(predecessor.data);
    mem::c_free(bucket.data);

    return idom;
}

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
                LList<BasicBlock *> idom = generate_immediate_dominators(irctx.basicBlocks.get(0), idCnt + 1);
                for (size_t i = 1; i < idom.size; i++) {
                    printf("%d-%d\n", i, idom.get(i)->id);
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
