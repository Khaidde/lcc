#include "gen.hpp"

#include "mem.hpp"
#include "print.hpp"

namespace lcc {

ListIterator<BasicBlock *> rpo_begin(Function &fn) { return {&fn.rpo[0]}; }

ListIterator<BasicBlock *> rpo_end(Function &fn) { return {&fn.rpo[fn.numBlocks]}; }

ListIterator<BasicBlock *> pred_begin(BasicBlock *block) { return {&block->pred[0]}; }

ListIterator<BasicBlock *> pred_end(BasicBlock *block) { return {&block->pred[block->predCnt]}; }

size_t succ_count(BasicBlock *block) {
    switch (block->term.kind) {
        case Terminator::Kind::kCond: return 2;
        case Terminator::Kind::kGoto: return 1;
        case Terminator::Kind::kRet: return 0;
    }
}

ListIterator<BasicBlock *> succ_begin(BasicBlock *block) { return {&block->term.succ[0]}; }

ListIterator<BasicBlock *> succ_end(BasicBlock *block) { return {&block->term.succ[succ_count(block)]}; }

constexpr size_t kAnnotateSpace = 10;

void print_reg(Inst *def) {
    switch (def->type) {
        case Inst::Type::kUnk:  // Stands for "v(alue)" which is unknown
            printf("v");
            break;
        case Inst::Type::kU16:
            set_color(kColorYellow);
            printf("u");
            break;
        case Inst::Type::kPtr:
            set_color(kColorMagenta);
            printf("p");
            break;
    }
    printf("%d", def->dst);
    reset_color();
}

void print_opd(Opd &opd) {
    switch (opd.kind) {
        case Opd::Kind::kImm:
            set_color(kColorCyan);
            printf("%d", opd.intval);
            reset_color();
            break;
        case Opd::Kind::kReg:
            if (!opd.regVal || opd.regVal->dst == 0) {
                printf("v_");
            } else {
                print_reg(opd.regVal);
            }
            break;
    }
}

void print_inst(Inst *inst) {
    const char *annotation =
#if DBG_OPT
        inst->annotation;
#else
        nullptr;
#endif
    printf("[ %*.*s ]   ", kAnnotateSpace, kAnnotateSpace, annotation ? annotation : "");

    if (inst->dst != 0) {
        print_reg(inst);
        if (inst->dst < 10) printf(" ");
        printf(" = ");
    } else {
        printf("      ");
    }
    switch (inst->kind) {
        case Inst::Kind::kAlloc:
            printf("alloc(");
            printf("%d)", inst->alloc.allocSize);
            printf(" // ");
            switch (inst->alloc.kind) {
                case AllocInst::Kind::kRetVal: printf("return value"); break;
                case AllocInst::Kind::kArg: printf("<arg %d> ", inst->alloc.argNo);
#if DBG_OPT
                    lstr_print(inst->alloc.name);
#endif
                    break;
                case AllocInst::Kind::kLocalVar: printf("<local> ");
#if DBG_OPT
                    lstr_print(inst->alloc.name);
#endif
                    break;
            }
            printf("\n");
            break;
        case Inst::Kind::kPhi:
            printf("phi(");
            for (size_t k = 0; k < inst->phi.joins.size; k++) {
                PhiInst::Arg &phiArg = inst->phi.joins[k];
                printf("v%-2d[B%-2d]", phiArg.ref->dst, phiArg.bb->idx);
                if (k < inst->phi.joins.size - 1) {
                    printf(", ");
                }
            }
            printf(")\n");

            break;
        case Inst::Kind::kAssign:
            print_opd(inst->assign.src);
            printf("\n");
            break;
        case Inst::Kind::kBin:
            const char *opstr;
            switch (inst->bin.op) {
                case TokenType::kAdd: opstr = "add"; break;
                case TokenType::kSubNeg: opstr = "sub"; break;
                default: opstr = "?";
            }
            printf("%s(", opstr);
            print_opd(inst->bin.left);
            printf(", ");
            print_opd(inst->bin.right);
            printf(")\n");
            break;
        case Inst::Kind::kCall:
            printf("%s(", lstr_raw_str(inst->call.fn->ident));
            for (size_t i = 0; i < inst->call.numArgs; i++) {
                print_opd(inst->call.args[i]);
                if (i + 1 < inst->call.numArgs) {
                    printf(", ");
                }
            }
            printf(")\n");
            break;
        case Inst::Kind::kLoad:
            printf("load[");
            print_opd(inst->ld.src);
            printf("]\n");
            break;
        case Inst::Kind::kStore:
            printf("store(");
            print_opd(inst->st.src);
            printf(" -> [");
            print_opd(inst->st.addr);
            printf("])\n");
            break;
    }
}

void print_bid(BasicBlock *block) {
    set_color(kColorGreen);
    printf("B%-2d", block->idx);
    reset_color();
}

void print_function(Function &fn) {
    for (auto bb = rpo_begin(fn); bb != rpo_end(fn); ++bb) {
        printf("%*s    ", kAnnotateSpace, "");
        print_bid(*bb);
        printf("\n");

        for (Inst *inst = (*bb)->start; inst; inst = inst->next) {
            print_inst(inst);
        }

        const char *annotation =
#if DBG_OPT
            (*bb)->term.annotation;
#else
            nullptr;
#endif
        printf("[ %*.*s ]", kAnnotateSpace, kAnnotateSpace, annotation ? annotation : "");
        switch ((*bb)->term.kind) {
            case Terminator::Kind::kGoto: {
                BasicBlock *exit = (*bb)->term.tgoto.target;
                printf("      -> ");
                print_bid(exit);
                printf("\n");
                break;
            }
            case Terminator::Kind::kCond: {
                BasicBlock *then = (*bb)->term.cond.then;
                BasicBlock *alt = (*bb)->term.cond.alt;

                printf("      -> ");
                print_bid(then);
                printf(":");
                print_bid(alt);
                printf("[?");
                print_opd((*bb)->term.cond.predicate);
                printf("]\n");
                break;
            }
            case Terminator::Kind::kRet: printf("         ret\n"); break;
        }
    }
}

void start_pass(Opt &opt, const char *passName) {
#if DBG_OPT
    opt.currentPassName = passName;
#else
    (void)opt;
    (void)passName;
#endif
}

Inst *create_inst(Inst::Kind kind) {
    Inst *inst = mem::p_alloc<Inst>();
    inst->kind = kind;
#if DBG_OPT
    inst->annotation = nullptr;
#endif
    inst->block = nullptr;
    inst->prev = nullptr;
    inst->next = nullptr;
    return inst;
}

void push_front_inst(Opt &opt, BasicBlock *block, Inst *inst, bool genNewReg) {
    assert(block);
    assert(inst);
#if DBG_OPT
    inst->annotation = opt.currentPassName;
#else
    (void)opt;
#endif
    inst->block = block;
    inst->next = block->start;
    if (block->end) {
        block->start->prev = inst;
    } else {
        block->end = inst;
    }
    block->start = inst;

    inst->dst = genNewReg ? opt.nextReg++ : 0;
}

void push_back_inst(Opt &opt, BasicBlock *block, Inst *inst, bool genNewReg) {
    assert(block);
    assert(inst);
#if DBG_OPT
    inst->annotation = opt.currentPassName;
#else
    (void)opt;
#endif
    inst->block = block;
    inst->prev = block->end;
    if (block->start) {
        block->end->next = inst;
    } else {
        block->start = inst;
    }
    block->end = inst;

    inst->dst = genNewReg ? opt.nextReg++ : 0;
}

void insert_inst(Opt &opt, BasicBlock *block, Inst *afterThis, Inst *inst, bool genNewReg) {
    assert(block);
    assert(inst);
#if DBG_OPT
    inst->annotation = opt.currentPassName;
#else
    (void)opt;
#endif
    inst->block = block;

    if (afterThis) {
        assert(afterThis->block == block);
        inst->next = afterThis->next;
        if (afterThis->next) {
            afterThis->next->prev = inst;
        } else {
            block->end = inst;
        }

        afterThis->next = inst;
        inst->prev = afterThis;
    } else {
        assert(!inst->next);
        assert(!inst->prev);
        assert(!block->start);
        assert(!block->end);
        block->start = inst;
        block->end = inst;
    }

    inst->dst = genNewReg ? opt.nextReg++ : 0;
}

void remove_inst(Inst *inst) {
    switch (inst->kind) {
        case Inst::Kind::kCall: mem::c_free(inst->call.args); break;
        case Inst::Kind::kPhi: mem::c_free(inst->phi.joins.data); break;
        default: break;
    }
    if (inst->prev) {
        inst->prev->next = inst->next;
    } else if (inst->block) {
        inst->block->start = inst->next;
    }

    if (inst->next) {
        inst->next->prev = inst->prev;
    } else if (inst->block) {
        inst->block->end = inst->prev;
    }
    mem::p_free(inst);
}

namespace translate {

struct Context {
    struct Opt &opt;
    Function &fn;
    BasicBlock *bb{nullptr};

    struct LoopInfo {
        BasicBlock *entry;
        BasicBlock *exit;
    };
    LMap<Node *, LoopInfo> loopMap{};  // Maps ast while to loop entry and exit blocks

    Inst *returnValuePtrInst{nullptr};  // instruction containing pointer to return value on stack
    Inst *lastAllocInst{nullptr};
    LMap<Node *, Inst *> declAllocMap{};  // Maps ast decl to inst defining the pointer
};

BasicBlock *create_empty_block() {
    BasicBlock *block = mem::p_alloc<BasicBlock>();
    block->predCnt = 0;
    block->start = nullptr;
    block->end = nullptr;
#if DBG_OPT
    block->term.annotation = "";
#endif
    return block;
}

BasicBlock *create_block(Context &tc) {
    BasicBlock *block = create_empty_block();
    block->idx = tc.fn.numBlocks++;
    tc.bb = block;
    return block;
}

void split_edge(Context &tc, BasicBlock *&predTermTarget, BasicBlock *succ) {
    BasicBlock *splitEdgeBlock = create_block(tc);
    splitEdgeBlock->term.kind = Terminator::Kind::kGoto;
    splitEdgeBlock->term.tgoto.target = succ;
    predTermTarget = splitEdgeBlock;
}

Inst::Type get_opd_type(Opd &opd) {
    switch (opd.kind) {
        case Opd::Kind::kImm: return Inst::Type::kU16;
        case Opd::Kind::kReg: assert(opd.regVal); return opd.regVal->type;
    }
}

void translate_expr(Context &tc, Opd &dst, Node *exp);

Inst *translate_call(Context &tc, Node *exp, bool genNewReg) {
    Inst *call = create_inst(Inst::Kind::kCall);
    assert(exp->call.callee->kind == NodeKind::kName);
    call->call.fn = tc.opt.fnNameMap[exp->call.callee->name.ident];

    call->call.args = mem::c_alloc<Opd>(exp->call.args.size);
    for (size_t i = 0; i < exp->call.args.size; i++) {
        translate_expr(tc, call->call.args[i], exp->call.args[i]);
    }
    push_back_inst(tc.opt, tc.bb, call, genNewReg);
    return call;
}

Inst *translate_load(Context &tc, Node *nameRef) {
    Inst *ld = create_inst(Inst::Kind::kLoad);
    ld->type = Inst::Type::kUnk;
    ld->ld.src.kind = Opd::Kind::kReg;
    ld->ld.src.regVal = *tc.declAllocMap[nameRef];
    push_back_inst(tc.opt, tc.bb, ld, true);
    return ld;
}

// Returns instruction if generated else nullptr
void translate_expr(Context &tc, Opd &dst, Node *exp) {
    switch (exp->kind) {
        case NodeKind::kIntLit:
            dst.kind = Opd::Kind::kImm;
            dst.intval = exp->intLit.intVal;
            mem::p_free(exp);
            break;
        case NodeKind::kName: {
            if (exp->name.ref->decl.isAssignToGlobal) {
                todo("TODO: Use of global variable '%s'\n", lstr_raw_str(exp->name.ident));
                assert(false);
            }
            Inst *ld = translate_load(tc, exp->name.ref);
            dst.kind = Opd::Kind::kReg;
            dst.regVal = ld;
            mem::p_free(exp);
            break;
        }
        case NodeKind::kPrefix: {
            switch (exp->prefix.op) {
                case TokenType::kPtr:
                    assert(exp->prefix.inner->kind == NodeKind::kName);
                    dst.kind = Opd::Kind::kReg;
                    dst.regVal = *tc.declAllocMap[exp->prefix.inner->name.ref];
                    break;
                case TokenType::kDeref: {
                    Inst *derefLd = create_inst(Inst::Kind::kLoad);
                    derefLd->type = Inst::Type::kUnk;
                    translate_expr(tc, derefLd->ld.src, exp->prefix.inner);

                    push_back_inst(tc.opt, tc.bb, derefLd, true);

                    dst.kind = Opd::Kind::kReg;
                    dst.regVal = derefLd;
                    mem::p_free(exp);
                    break;
                }
                default: assert(!"TODO: Prefix generation not yet implemented");
            }
            break;
        }
        case NodeKind::kInfix: {
            Inst *bin = create_inst(Inst::Kind::kBin);
            bin->bin.op = exp->infix.op;
            translate_expr(tc, bin->bin.left, exp->infix.left);
            translate_expr(tc, bin->bin.right, exp->infix.right);
            dst.kind = Opd::Kind::kReg;
            dst.regVal = bin;

            if (get_opd_type(bin->bin.left) == Inst::Type::kU16 && get_opd_type(bin->bin.right) == Inst::Type::kU16) {
                bin->type = Inst::Type::kU16;
            } else {
                bin->type = Inst::Type::kUnk;
            }
            push_back_inst(tc.opt, tc.bb, bin, true);
            mem::p_free(exp);
            break;
        }
        case NodeKind::kCall: {
            Inst *call = translate_call(tc, exp, true);
            dst.kind = Opd::Kind::kReg;
            dst.regVal = call;
            break;
        }
        default: assert(!"TODO: no translation implemented for expression kind"); unreachable();
    }
}

void translate_decl(Context &tc, Node *decl) {
    switch (decl->decl.lval->kind) {
        case NodeKind::kName:
            if (decl->decl.rval) {
                Inst *st = create_inst(Inst::Kind::kStore);
                st->st.addr.kind = Opd::Kind::kReg;

                Inst *varAlloc = create_inst(Inst::Kind::kAlloc);
                if (Inst **allocPtr = tc.declAllocMap.try_put(decl->decl.lval->name.ref, varAlloc)) {
                    mem::p_free(varAlloc);
                    st->st.addr.regVal = *allocPtr;
                } else {
                    varAlloc->type = Inst::Type::kPtr;
#if DBG_OPT
                    varAlloc->alloc.name = decl->decl.lval->name.ident;
#endif
                    varAlloc->alloc.kind = AllocInst::Kind::kLocalVar;
                    varAlloc->alloc.allocSize = get_byte_size(decl->decl.resolvedTy);
                    st->st.addr.regVal = varAlloc;

                    insert_inst(tc.opt, tc.fn.entry, tc.lastAllocInst, varAlloc, true);
                    tc.lastAllocInst = varAlloc;
                }
                translate_expr(tc, st->st.src, decl->decl.rval);
                push_back_inst(tc.opt, tc.bb, st, false);
            }
            break;
        case NodeKind::kPrefix: {
            assert(!decl->decl.isDecl);
            assert(decl->decl.lval->prefix.op == TokenType::kDeref);
            assert(decl->decl.rval);

            Inst *st = create_inst(Inst::Kind::kStore);
            st->st.addr.kind = Opd::Kind::kReg;
            translate_expr(tc, st->st.src, decl->decl.rval);
            translate_expr(tc, st->st.addr, decl->decl.lval->prefix.inner);
            push_back_inst(tc.opt, tc.bb, st, false);
            break;
        }
        default: assert(!"Invalid lvalue node cannot be translated to IR");
    }
    mem::p_free(decl->decl.lval);
    mem::p_free(decl);
}

BasicBlock *translate_block(Context &tc, BasicBlock *exit, Node *block);

BasicBlock *translate_if(Context &tc, BasicBlock *exit, Node *ifstmt) {
    BasicBlock *entry = tc.bb;
    for (;;) {
        translate_expr(tc, entry->term.cond.predicate, ifstmt->ifstmt.cond);

        Node *thenstmt = ifstmt->ifstmt.then;
        Node *altstmt = ifstmt->ifstmt.alt;
        mem::p_free(ifstmt);

        bool isAlwaysFalse = false;
        if (entry->term.cond.predicate.kind == Opd::Kind::kImm) {
            if (entry->term.cond.predicate.intval == 0) {
                isAlwaysFalse = true;
            } else {
                // "Then" of ifstmt always executes so treat this call
                // to translate_if as if it were translate_block
                return translate_block(tc, exit, ifstmt->ifstmt.then);
            }
        } else {
            entry->term.kind = Terminator::Kind::kCond;
        }

        if (isAlwaysFalse) {
            entry->term.kind = Terminator::Kind::kGoto;
            if (!altstmt) {
                entry->term.tgoto.target = exit;
                return exit;
            }
        } else {
            BasicBlock *then = create_block(tc);
            entry->term.cond.then = then;
            translate_block(tc, exit, thenstmt);

            if (!altstmt) {
                // Remove critical edge in if statement with no else block
                // (Critical edge would exist from condition block to exit block)
                split_edge(tc, entry->term.cond.alt, exit);
                return exit;
            }

            BasicBlock *alt = create_block(tc);
            entry->term.cond.alt = alt;
            entry = alt;
        }
        if (altstmt->kind == NodeKind::kBlock) {
            return translate_block(tc, exit, ifstmt->ifstmt.alt);
        }
        assert(altstmt->kind == NodeKind::kIf);
        ifstmt = altstmt;
    }
}

BasicBlock *translate_while(Context &tc, BasicBlock *exit, Node *whilestmt) {
    BasicBlock *entry = tc.bb;

    translate_expr(tc, entry->term.cond.predicate, whilestmt->whilestmt.cond);

    // Early CFG Optimization where while statement of condition 0/false is culled
    if (entry->term.cond.predicate.kind == Opd::Kind::kImm) {
        if (entry->term.cond.predicate.intval == 0) {
            entry->term.kind = Terminator::Kind::kGoto;
            entry->term.tgoto.target = exit;
            return exit;
        }
    }
    entry->term.kind = Terminator::Kind::kCond;

    // Goto targets for break and continue statements
    tc.loopMap.try_put(whilestmt, {entry, exit});

    BasicBlock *loop = create_block(tc);
    entry->term.cond.then = loop;
    translate_block(tc, entry, whilestmt->whilestmt.loop);

    // Potential for critical edge from entry to exit
    // so preemptively split it. Later CFG optimization
    // may cull the new block if it is later found to
    // be unnecessary
    split_edge(tc, entry->term.cond.alt, exit);

    mem::p_free(whilestmt);
    return exit;
}

BasicBlock *translate_block(Context &tc, BasicBlock *exit, Node *block) {
    assert(block->kind == NodeKind::kBlock);
    for (StatementListNode *stmtNode = block->block.start; stmtNode; stmtNode = stmtNode->next) {
        Node *stmt = stmtNode->stmt;
        switch (stmt->kind) {
            case NodeKind::kDecl: translate_decl(tc, stmt); break;
            case NodeKind::kCall: translate_call(tc, stmt, false); break;
            case NodeKind::kIf: {
                if (stmtNode->next) {
                    BasicBlock *ifExit = translate_if(tc, create_empty_block(), stmt);
                    ifExit->idx = tc.fn.numBlocks++;
                    tc.bb = ifExit;
                    break;
                } else {
                    translate_if(tc, exit, stmt);
                    mem::p_free(block);
                    return exit;
                }
                break;
            }
            case NodeKind::kWhile: {
                BasicBlock *prev = tc.bb;
                prev->term.kind = Terminator::Kind::kGoto;
                BasicBlock *cond = create_block(tc);
                prev->term.tgoto.target = cond;

                BasicBlock *whileExit = translate_while(tc, create_empty_block(), stmt);
                whileExit->idx = tc.fn.numBlocks++;
                tc.bb = whileExit;
                break;
            }
            case NodeKind::kLoopBr: {
                tc.bb->term.kind = Terminator::Kind::kGoto;
                if (stmt->loopbr.isBreak) {
                    tc.bb->term.tgoto.target = tc.loopMap[stmt->loopbr.ref]->exit;
                } else {
                    // loop branch is continue
                    tc.bb->term.tgoto.target = tc.loopMap[stmt->loopbr.ref]->entry;
                }
                mem::p_free(block);
                return exit;
            }
            case NodeKind::kRet: {
                if (stmt->ret.value) {
                    Inst *st = create_inst(Inst::Kind::kStore);
                    assert(tc.returnValuePtrInst);
                    st->st.addr.kind = Opd::Kind::kReg;
                    st->st.addr.regVal = tc.returnValuePtrInst;
                    translate_expr(tc, st->st.src, stmt->ret.value);
                    push_back_inst(tc.opt, tc.bb, st, false);
                }

                tc.bb->term.kind = Terminator::Kind::kGoto;
                tc.bb->term.tgoto.target = tc.fn.exit;
                mem::p_free(block);
                return exit;
            }
            default: assert(false && "TODO: no translation implemented for statement kind"); unreachable();
        }
    }
    tc.bb->term.kind = Terminator::Kind::kGoto;
    tc.bb->term.tgoto.target = exit;
    mem::p_free(block);
    return exit;
}

Function &translate_function(Opt &opt, Node *functionDecl) {
    assert(functionDecl->kind == NodeKind::kDecl);

    start_pass(opt, "");

    opt.fnNameMap.try_put(functionDecl->decl.lval->name.ident, {});
    Function *fnPtr = opt.fnNameMap[functionDecl->decl.lval->name.ident];
    assert(fnPtr);

    Function &fn = *fnPtr;
    fn.ident = functionDecl->decl.lval->name.ident;
    fn.dom = nullptr;

    Context tc{opt, fn};
    tc.loopMap.init();
    tc.declAllocMap.init();

    fn.entry = create_block(tc);

    // Assign vreg value to address of return value on stack frame
    if (functionDecl->decl.resolvedTy->funcTy.retTy->kind != TypeKind::kNone) {
        Inst *retAlloc = create_inst(Inst::Kind::kAlloc);
        retAlloc->type = Inst::Type::kPtr;
        retAlloc->alloc.kind = AllocInst::Kind::kRetVal;
        assert(functionDecl->decl.resolvedTy->kind == TypeKind::kFuncTy);
        retAlloc->alloc.allocSize = get_byte_size(functionDecl->decl.resolvedTy->funcTy.retTy);

        insert_inst(tc.opt, tc.fn.entry, tc.lastAllocInst, retAlloc, true);
        tc.lastAllocInst = retAlloc;

        tc.returnValuePtrInst = retAlloc;
    }

    // Assign vreg values to function arguments in entry block
    for (size_t i = 0; i < functionDecl->decl.rval->func.params.size; i++) {
        Node *param = functionDecl->decl.rval->func.params[i];
        Inst *argAlloc = create_inst(Inst::Kind::kAlloc);
        argAlloc->type = Inst::Type::kPtr;
#if DBG_OPT
        argAlloc->alloc.name = param->decl.lval->name.ident;
#endif
        argAlloc->alloc.kind = AllocInst::Kind::kArg;
        argAlloc->alloc.allocSize = get_byte_size(param->decl.resolvedTy);
        argAlloc->alloc.argNo = i;

        insert_inst(tc.opt, tc.fn.entry, tc.lastAllocInst, argAlloc, true);
        tc.lastAllocInst = argAlloc;

        tc.declAllocMap.try_put(functionDecl->decl.rval->func.params[i], argAlloc);
    }

    // Translate AST into CFG
    fn.exit = create_empty_block();
    translate_block(tc, fn.exit, functionDecl->decl.rval->func.body);
    fn.exit->term.kind = Terminator::Kind::kRet;
    fn.exit->idx = fn.numBlocks++;

    tc.loopMap.destroy();
    tc.declAllocMap.destroy();

    return fn;
}

}  // namespace translate

Function &gen_function(Opt &opt, Node *functionDecl) { return translate::translate_function(opt, functionDecl); }

}  // namespace lcc
