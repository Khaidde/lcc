#include "translate.hpp"

#include <cstdio>

#include "ast.hpp"
#include "print.hpp"

namespace lcc {

namespace {

BasicBlock *create_block() {
    BasicBlock *block = mem::malloc<BasicBlock>();
    block->instrs = {};
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
    BasicBlock *then = create_block(idCnt);
    entry->exits.add(then);
    translate_block(idCnt, then, exit, ifstmt->ifstmt.then);

    if (ifstmt->ifstmt.alt) {
        if (ifstmt->ifstmt.alt->kind == NodeKind::kIf) {
            // TODO: handle translation of else if
            assert(false);
        } else {
            assert(ifstmt->ifstmt.alt->kind == NodeKind::kBlock);
            BasicBlock *alt = create_block(idCnt);
            entry->exits.add(alt);
            translate_block(idCnt, alt, exit, ifstmt->ifstmt.alt);
        }
    } else {
        entry->exits.add(exit);
    }
}

void translate_block(BlockId &idCnt, BasicBlock *entry, BasicBlock *exit, Node *block) {
    assert(block->kind == NodeKind::kBlock);

    BasicBlock *curr = entry;
    for (size_t i = 0; i < block->block.stmts.size; i++) {
        curr->instrs.size++;
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
            default: break;
        }
    }
    curr->exits.add(exit);
}

BasicBlock *translate_function_body(Node *function) {
    BlockId idCnt = 0;
    BasicBlock *entry = create_block(idCnt);
    BasicBlock *exit = create_block();
    translate_block(idCnt, entry, exit, function->func.body);
    exit->id = idCnt;
    return entry;
}

}  // namespace

void print_block(BlockId &bid, BasicBlock *basicBlock) {
    printf("b%d(%d)\n", basicBlock->id, basicBlock->instrs.size);
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
                irctx.basicBlocks.add(translate_function_body(declListHead->decl.rval));
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
