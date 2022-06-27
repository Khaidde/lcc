#include "ir.hpp"

#include "ast.hpp"
#include "print.hpp"

namespace lcc {

#ifndef NDEBUG
template <>
const char *mem::PoolAllocator<Inst>::get_type_name() {
    return "Inst";
}

template <>
const char *mem::PoolAllocator<BasicBlock>::get_type_name() {
    return "BasicBlock";
}
#endif

ListIterator<BasicBlock *> po_begin(CFG &cfg) { return {&cfg.po[0]}; }

ListIterator<BasicBlock *> po_end(CFG &cfg) { return {&cfg.po[cfg.numBlocks]}; }

ListIterator<BasicBlock *> rpo_begin(CFG &cfg) { return {&cfg.rpo[0]}; }

ListIterator<BasicBlock *> rpo_end(CFG &cfg) { return {&cfg.rpo[cfg.numBlocks]}; }

ListIterator<BasicBlock *> pred_begin(BasicBlock *block) { return {&block->pred.data[0]}; }

ListIterator<BasicBlock *> pred_end(BasicBlock *block) { return {&block->pred.data[block->pred.size]}; }

size_t succ_count(BasicBlock *block) {
    switch (block->term.kind) {
        case Terminator::Kind::kCond: return 2;
        case Terminator::Kind::kGoto: return 1;
        case Terminator::Kind::kRet: return 0;
    }
}

ListIterator<BasicBlock *> succ_begin(BasicBlock *block) { return {&block->term.succ[0]}; }

ListIterator<BasicBlock *> succ_end(BasicBlock *block) { return {&block->term.succ[succ_count(block)]}; }

namespace {

struct RenameVar {
    enum Kind {
        kUse,
        kDef,
    } kind;
    VarId varid;
    union {
        Inst *def;
        Inst **use;
    };

    RenameVar *next;
};

struct RenameVarLL {
    RenameVar *start;
    RenameVar *end;
};

struct GenIRContext {
    CFG &cfg;

    BasicBlock *currBB{nullptr};

    struct LoopInfo {
        BasicBlock *entry;
        BasicBlock *exit;
    };
    LPtrMap<Node, LoopInfo> loopMap{};  // Maps ast while to loop entry and exit blocks

    LPtrMap<Node, VarInfo *> declMap{};  // Maps ast decl to variable info
    LList<VarInfo *> varInfoTab{};
    LList<RenameVarLL> useDefTab{};  // Keep track of uses and defs for each block in ssa variable renaming
};

RenameVar *create_use_def(GenIRContext &irc) {
    while (irc.useDefTab.size <= irc.currBB->id) {
        irc.useDefTab.add({nullptr, nullptr});
    }

    RenameVarLL &usedefList = irc.useDefTab[irc.currBB->id];

    RenameVar *newUseDef = mem::p_malloc<RenameVar>();
    newUseDef->next = nullptr;
    if (usedefList.start) {
        usedefList.end->next = newUseDef;
    } else {
        usedefList.start = newUseDef;
    }
    usedefList.end = newUseDef;
    return newUseDef;
}

void create_use(GenIRContext &irc, VarInfo *varInfo, Inst **ref) {
    RenameVar *use = create_use_def(irc);
    use->kind = RenameVar::Kind::kUse;
    use->varid = varInfo->varid;
    use->use = ref;
}

void create_def(GenIRContext &irc, VarInfo *varInfo, Inst *ref) {
    RenameVar *def = create_use_def(irc);
    def->kind = RenameVar::Kind::kDef;
    def->varid = varInfo->varid;
    def->def = ref;
}

VarInfo *init_new_variable(GenIRContext &irc, Node *decl) {
    VarInfo *info = mem::p_malloc<VarInfo>();
    info->varid = irc.varInfoTab.size;

    info->defSites = {};
    info->defStack = {};
    info->defSites.add(irc.currBB->id);

    irc.declMap.try_put(decl, info);
    irc.varInfoTab.add(info);

    return info;
}

void init_terminator(Terminator::Kind kind, BasicBlock *block) {
    block->term.kind = kind;
#ifndef NDEBUG
    block->term.annotation = "";
#endif
}

BasicBlock *create_empty_block() {
    BasicBlock *block = mem::p_malloc<BasicBlock>();
    block->start = nullptr;
    block->end = nullptr;
    return block;
}

BasicBlock *create_block(CFG &cfg) {
    BasicBlock *block = create_empty_block();
    block->id = cfg.numBlocks++;
    return block;
}

void translate_expr(GenIRContext &irc, Opd &dstOpd, Node *expr);

Inst *translate_call(GenIRContext &irc, Node *call) {
    assert(call->kind == NodeKind::kCall);
    assert(call->call.callee->kind == NodeKind::kName);

    Inst *callInst = create_inst(Inst::Kind::kCall);
    // TODO: handle complex callee cases
    callInst->call.cfg = irc.cfg.globalIR->funcMap[call->call.callee->name.ident];
    callInst->call.args.init(call->call.args.size);
    for (size_t i = 0; i < call->call.args.size; i++) {
        Opd &arg = callInst->call.args[i];
        translate_expr(irc, arg, call->call.args[i]);
    }
    add_end_inst(irc.currBB, callInst);
    mem::p_free(call);
    return callInst;
}

void translate_expr(GenIRContext &irc, Opd &dstOpd, Node *expr) {
    switch (expr->kind) {
        case NodeKind::kIntLit: {
            dstOpd.kind = Opd::Kind::kImm;
            dstOpd.intval = expr->intLit.intVal;
            mem::p_free(expr);
            break;
        }
        case NodeKind::kName: {
            if (expr->name.ref->decl.isAssignToGlobal) {
                todo("TODO: Use of global variable '%s'\n", lstr_raw_str(expr->name.ident));
                assert(false);
            }
            dstOpd.kind = Opd::Kind::kReg;
            dstOpd.regVal = nullptr;
            VarInfo *info = (*irc.declMap[expr->name.ref]);
            create_use(irc, info, &dstOpd.regVal);
            mem::p_free(expr);
            break;
        }
        case NodeKind::kInfix: {
            Inst *bin = create_inst(Inst::Kind::kBin);
            bin->bin.op = expr->infix.op;
            translate_expr(irc, bin->bin.left, expr->infix.left);
            translate_expr(irc, bin->bin.right, expr->infix.right);
            dstOpd.kind = Opd::Kind::kReg;
            dstOpd.regVal = bin;
            bin->dst = irc.cfg.nextReg++;
            add_end_inst(irc.currBB, bin);
            mem::p_free(expr);
            break;
        }
        case NodeKind::kCall: {
            Inst *call = translate_call(irc, expr);
            call->dst = irc.cfg.nextReg++;
            dstOpd.kind = Opd::Kind::kReg;
            dstOpd.regVal = call;
            break;
        }
        default: assert(false && "TODO: no translation implemented for expression kind"); unreachable();
    }
}

void translate_decl(GenIRContext &irc, Node *decl) {
    if (decl->decl.lval->kind == NodeKind::kName) {
        // Keep track of definitions
        VarInfo *info;
        if (VarInfo **infoPtr = irc.declMap[decl->decl.lval->name.ref]) {
            // Ensure no duplicated before adding to definition sites
            // Assumes that we iterate blocks in order and never revisit
            info = *infoPtr;
            if (info->defSites.last() != irc.currBB->id) {
                info->defSites.add(irc.currBB->id);
            }
        } else {
            info = init_new_variable(irc, decl);
        }

        if (decl->decl.rval) {
            Inst *declInst = create_inst(Inst::Kind::kAssign);
            // Must calculate before translate_expr because decl->decl.rval is freed
            bool isInfixOrCall = decl->decl.rval->kind == NodeKind::kInfix;
            isInfixOrCall |= decl->decl.rval->kind == NodeKind::kCall;
            translate_expr(irc, declInst->assign.src, decl->decl.rval);

            // Minimize copies of temporaries when expression was infix node
            if (isInfixOrCall && declInst->assign.src.kind == Opd::Kind::kReg) {
                Inst *srcInst = declInst->assign.src.regVal;
                mem::p_free(declInst);
                declInst = srcInst;
            } else {
                declInst->dst = irc.cfg.nextReg++;
                add_end_inst(irc.currBB, declInst);
            }
            create_def(irc, info, declInst);
        }
    }
    mem::p_free(decl->decl.lval);
    mem::p_free(decl);
}

void translate_block(GenIRContext &irc, BasicBlock *exit, Node *block);

void translate_if(GenIRContext &irc, BasicBlock *entry, BasicBlock *exit, Node *ifstmt) {
    for (;;) {
        init_terminator(Terminator::Kind::kCond, entry);

        translate_expr(irc, entry->term.cond.predicate, ifstmt->ifstmt.cond);
        if (entry->term.cond.predicate.kind == Opd::Kind::kImm) {
            // TODO optimization where if statement of constant expression is culled
            // assert(predicate.kind == Opd::Kind::kReg);
            assert(false && "TODO: Const value if statement predicate opt");
        }

        BasicBlock *then = irc.currBB = create_block(irc.cfg);
        entry->term.cond.then = then;
        translate_block(irc, exit, ifstmt->ifstmt.then);

        if (!ifstmt->ifstmt.alt) {
            entry->term.cond.alt = exit;
            mem::p_free(ifstmt);
            return;
        }
        BasicBlock *alt = irc.currBB = create_block(irc.cfg);
        entry->term.cond.alt = alt;
        if (ifstmt->ifstmt.alt->kind == NodeKind::kBlock) {
            translate_block(irc, exit, ifstmt->ifstmt.alt);
            mem::p_free(ifstmt);
            return;
        }
        assert(ifstmt->ifstmt.alt->kind == NodeKind::kIf);
        entry = alt;

        Node *next = ifstmt->ifstmt.alt;
        mem::p_free(ifstmt);
        ifstmt = next;
    }
}

void translate_while(GenIRContext &irc, BasicBlock *entry, BasicBlock *exit, Node *whilestmt) {
    init_terminator(Terminator::Kind::kCond, entry);
    entry->term.cond.alt = exit;

    // TODO optimization where while statement of condition 0/false is culled
    // assert(predicate.kind == Opd::Kind::kReg);
    translate_expr(irc, entry->term.cond.predicate, whilestmt->whilestmt.cond);

    // Goto targets for break and continue statements
    irc.loopMap.try_put(whilestmt, {entry, exit});

    BasicBlock *loop = irc.currBB = create_block(irc.cfg);
    entry->term.cond.then = loop;

    init_terminator(Terminator::Kind::kGoto, loop);
    loop->term.tgoto.target = entry;

    translate_block(irc, entry, whilestmt->whilestmt.loop);

    mem::p_free(whilestmt);
}

void translate_block(GenIRContext &irc, BasicBlock *exit, Node *block) {
    assert(block->kind == NodeKind::kBlock);
    for (StatementListNode *stmtNode = block->block.start; stmtNode; stmtNode = stmtNode->next) {
        Node *stmt = stmtNode->stmt;
        switch (stmt->kind) {
            case NodeKind::kDecl: {
                translate_decl(irc, stmt);
                break;
            }
            case NodeKind::kCall: {
                Inst *call = translate_call(irc, stmt);
                call->dst = 0;
                break;
            }
            case NodeKind::kIf: {
                if (stmtNode->next) {
                    BasicBlock *ifExit = create_empty_block();
                    translate_if(irc, irc.currBB, ifExit, stmt);
                    ifExit->id = irc.cfg.numBlocks++;

                    irc.currBB = ifExit;
                    break;
                } else {
                    translate_if(irc, irc.currBB, exit, stmt);
                    mem::p_free(block);
                    return;
                }
                break;
            }
            case NodeKind::kWhile: {
                if (irc.currBB->start) {
                    BasicBlock *cond = create_block(irc.cfg);
                    init_terminator(Terminator::Kind::kGoto, irc.currBB);
                    irc.currBB->term.tgoto.target = cond;
                    irc.currBB = cond;
                }
                if (stmtNode->next) {
                    BasicBlock *whileExit = create_empty_block();
                    translate_while(irc, irc.currBB, whileExit, stmt);
                    whileExit->id = irc.cfg.numBlocks++;
                    irc.currBB = whileExit;
                    break;
                } else {
                    translate_while(irc, irc.currBB, exit, stmt);
                    mem::p_free(block);
                    return;
                }
                break;
            }
            case NodeKind::kLoopBr: {
                init_terminator(Terminator::Kind::kGoto, irc.currBB);
                if (stmt->loopbr.isBreak) {
                    irc.currBB->term.tgoto.target = irc.loopMap[stmt->loopbr.ref]->exit;
                } else {
                    irc.currBB->term.tgoto.target = irc.loopMap[stmt->loopbr.ref]->entry;
                }
                mem::p_free(block);
                return;
            }
            case NodeKind::kRet: {
                if (stmt->ret.value) {
                    irc.cfg.exit->term.ret.args.add({});
                    irc.cfg.exit->term.ret.args.last().bb = irc.currBB;
                    translate_expr(irc, irc.cfg.exit->term.ret.args.last().opd, stmt->ret.value);
                }

                init_terminator(Terminator::Kind::kGoto, irc.currBB);
                irc.currBB->term.tgoto.target = irc.cfg.exit;
                mem::p_free(block);
                return;
            }
            default: assert(false && "TODO: no translation implemented for statement kind"); unreachable();
        }
    }
    init_terminator(Terminator::Kind::kGoto, irc.currBB);
    irc.currBB->term.tgoto.target = exit;
    mem::p_free(block);
}

// Simple, Fast Dominance Algorithm
// by Cooper et al.
BasicBlock **dom_tree(CFG &cfg) {
    // post order numberings
    size_t *pnum = mem::c_malloc<size_t>(cfg.numBlocks);
    for (size_t p = 0; p < cfg.numBlocks; p++) {
        pnum[cfg.rpo[p]->id] = p;
    }

    BasicBlock **idom = mem::c_malloc<BasicBlock *>(cfg.numBlocks);
    for (size_t i = 0; i < cfg.numBlocks; i++) {
        idom[i] = nullptr;
    }
    BasicBlock *entry = cfg.rpo[0];
    idom[entry->id] = entry;
    bool changed = true;
    while (changed) {
        changed = false;

        // For each basic block excluding entry node
        for (size_t i = 1; i < cfg.numBlocks; i++) {
            BasicBlock *bb = cfg.rpo[i];

            BasicBlock *newIdom = nullptr;
            for (auto pred = pred_begin(bb), end = pred_end(bb); pred != end; ++pred) {
                if (idom[(*pred)->id] != nullptr) {
                    if (newIdom) {
                        // Calculate dominator set intersection between predecessor
                        // and existing dominator set
                        BasicBlock *b1 = *pred;
                        BasicBlock *b2 = newIdom;
                        while (b1 != b2) {
                            while (pnum[b1->id] > pnum[b2->id]) {
                                b1 = idom[b1->id];
                            }
                            while (pnum[b1->id] < pnum[b2->id]) {
                                b2 = idom[b2->id];
                            }
                        }
                        newIdom = b1;
                    } else {
                        newIdom = *pred;
                    }
                }
            }
            if (idom[bb->id] != newIdom) {
                idom[bb->id] = newIdom;
                changed = true;
            }
        }
    }
    return idom;
}

void rename_block(GenIRContext &irc, LList<BasicBlock *> *&dominates, BasicBlock *curr) {
    size_t *numDefs = mem::c_malloc<size_t>(irc.varInfoTab.size);
    for (size_t i = 0; i < irc.varInfoTab.size; i++) {
        numDefs[i] = 0;
    }

    Inst *phi = curr->start;
    while (phi && phi->kind == Inst::Kind::kPhi) {
        VarInfo *info = phi->phi.varInfo;
        info->defStack.add(phi);
        numDefs[info->varid]++;
        phi = phi->next;
    }

    if (curr->id < irc.useDefTab.size) {
        RenameVar *currUseDef = irc.useDefTab[curr->id].start;
        while (currUseDef) {
            VarInfo *info = irc.varInfoTab[currUseDef->varid];
            if (currUseDef->kind == RenameVar::Kind::kDef) {
                info->defStack.add(currUseDef->def);
                numDefs[info->varid]++;
            } else {
                *currUseDef->use = info->defStack.last();
            }
            RenameVar *next = currUseDef->next;
            mem::p_free(currUseDef);
            currUseDef = next;
        }
    }

    // Add joins to phi nodes
    for (auto succ = succ_begin(curr), end = succ_end(curr); succ != end; ++succ) {
        Inst *phi = (*succ)->start;
        while (phi && phi->kind == Inst::Kind::kPhi) {
            VarInfo *info = phi->phi.varInfo;

            if (!info->defStack.size) {
                /*
                 * Phi node may be mistakenly placed before any definition of
                 * the variable it merges. Detect if variable has not been defined
                 * and if so, remove the phi node.
                 *
                 * Example scenario:
                 * while 1 {
                 *   b := 3
                 * }
                 * TODO prove that this can always be done
                 */
                free_inst(phi);
            } else {
                phi->phi.joins.add({info->defStack.last(), curr});
            }
            phi = phi->next;
        }
    }

    // Translate dominator children
    for (size_t i = 0; i < dominates[curr->id].size; i++) {
        rename_block(irc, dominates, dominates[curr->id][i]);
    }

    // Pop id stack for each variable
    for (size_t i = 0; i < irc.varInfoTab.size; i++) {
        irc.varInfoTab[i]->defStack.size -= numDefs[i];
    }
    mem::c_free(numDefs);
}

}  // namespace

const char *currentPassAnnotation = "";

Inst *create_inst(Inst::Kind kind) {
    Inst *inst = mem::p_malloc<Inst>();
    inst->kind = kind;
#ifndef NDEBUG
    inst->annotation = nullptr;
#endif
    inst->block = nullptr;
    inst->prev = nullptr;
    inst->next = nullptr;
    return inst;
}

void add_start_inst(BasicBlock *block, Inst *inst) {
#ifndef NDEBUG
    inst->annotation = currentPassAnnotation;
#endif
    inst->block = block;
    inst->next = block->start;
    if (block->end) {
        block->start->prev = inst;
    } else {
        block->end = inst;
    }
    block->start = inst;
}

void add_end_inst(BasicBlock *block, Inst *inst) {
#ifndef NDEBUG
    inst->annotation = currentPassAnnotation;
#endif
    inst->block = block;
    inst->prev = block->end;
    if (block->start) {
        block->end->next = inst;
    } else {
        block->start = inst;
    }
    block->end = inst;
}

void free_inst(Inst *inst) {
    switch (inst->kind) {
        case Inst::Kind::kCall: mem::c_free(inst->call.args.data); break;
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

namespace {

constexpr size_t kPrintWhitespace = 10;

void print_opd(Opd &opd) {
    switch (opd.kind) {
        case Opd::Kind::kReg: printf("v%d", opd.regVal->dst); break;
        case Opd::Kind::kImm: printf("%d", opd.intval); break;
    }
}

}  // namespace

void print_inst(Inst *inst) {
#ifndef NDEBUG
    printf("[ %*.*s ]   ", kPrintWhitespace, kPrintWhitespace, inst->annotation ? inst->annotation : "");
#endif

    if (inst->dst) printf("v%-2d = ", inst->dst);
    switch (inst->kind) {
        case Inst::Kind::kPhi:
            printf("phi(");
            for (size_t k = 0; k < inst->phi.joins.size; k++) {
                PhiInst::Arg &phiArg = inst->phi.joins[k];
                printf("v%-2d[B%-2d]", phiArg.ref->dst, phiArg.bb->id);
                if (k < inst->phi.joins.size - 1) {
                    printf(", ");
                }
            }
            printf(")");

            break;
        case Inst::Kind::kArg: printf("<arg %d>", inst->arg.argNo); break;
        case Inst::Kind::kAssign: print_opd(inst->assign.src); break;
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
            printf(")");
            break;
        case Inst::Kind::kCall:
            printf("%s(", lstr_raw_str(inst->call.cfg->ident));
            for (size_t i = 0; i < inst->call.args.size; i++) {
                print_opd(inst->call.args[i]);
                if (i + 1 < inst->call.args.size) {
                    printf(", ");
                }
            }
            printf(")");
            break;
    }
}

void start_pass(const char *annotation) {
#ifndef NDEBUG
    currentPassAnnotation = annotation;
#endif
}

void annotate_inst(Inst *inst) {
#ifndef NDEBUG
    inst->annotation = currentPassAnnotation;
#endif
}

void annotate_term(Terminator &term) {
#ifndef NDEBUG
    term.annotation = currentPassAnnotation;
#endif
}

void print_cfg(CFG &cfg) {
    for (auto bb = rpo_begin(cfg), end = rpo_end(cfg); bb != end; ++bb) {
        printf("%*s    B%d\n", kPrintWhitespace, "", (*bb)->id);

        Inst *inst = (*bb)->start;
        while (inst) {
            print_inst(inst);
            printf("\n");
            inst = inst->next;
        }

#ifndef NDEBUG
        const char *annotation = (*bb)->term.annotation;
        printf("[ %*.*s ]   ", kPrintWhitespace, kPrintWhitespace, annotation ? annotation : "");
#endif
        switch ((*bb)->term.kind) {
            case Terminator::Kind::kGoto: {
                BasicBlock *exit = (*bb)->term.tgoto.target;
                printf("=> B%d\n", exit->id);
                break;
            }
            case Terminator::Kind::kCond: {
                BasicBlock *then = (*bb)->term.cond.then;
                BasicBlock *alt = (*bb)->term.cond.alt;

                printf("=> B%d:B%d [?", then->id, alt->id);
                print_opd((*bb)->term.cond.predicate);
                printf("]\n");
                break;
            }
            case Terminator::Kind::kRet:
                printf("ret");
                if ((*bb)->term.ret.args.size) {
                    printf(" phi(");
                    for (size_t i = 0; i < (*bb)->term.ret.args.size; i++) {
                        RetTerminator::Arg &retArg = (*bb)->term.ret.args[i];
                        print_opd(retArg.opd);
                        printf("[B%d]", retArg.bb->id);
                        if (i < (*bb)->term.ret.args.size - 1) {
                            printf(", ");
                        }
                    }
                    printf(")\n");
                } else {
                    printf("\n");
                }
                break;
        }
    }
}

CFG &generate_cfg(IR &globalIR, Node *functionDecl) {
    assert(functionDecl->kind == NodeKind::kDecl);

    start_pass("");

    globalIR.funcMap.try_put(functionDecl->decl.lval->name.ident, {});
    CFG *cfgPtr = globalIR.funcMap[functionDecl->decl.lval->name.ident];
    CFG &cfg = *cfgPtr;
    cfg.ident = functionDecl->decl.lval->name.ident;
    cfg.globalIR = &globalIR;

    GenIRContext irc{cfg};
    irc.loopMap.init();
    irc.declMap.init();
    irc.varInfoTab = {};
    irc.useDefTab = {};

    // Translate function body into control flow graph
    cfg.entry = irc.currBB = create_block(cfg);

    // Assign vreg values to function arguments
    for (size_t i = 0; i < functionDecl->decl.rval->func.params.size; i++) {
        Node *argNode = functionDecl->decl.rval->func.params[i];

        Inst *arg = create_inst(Inst::Kind::kArg);
        arg->dst = cfg.nextReg++;
        arg->arg.argNo = i;
        add_end_inst(irc.currBB, arg);

        create_def(irc, init_new_variable(irc, argNode), arg);
    }

    cfg.exit = create_empty_block();
    cfg.exit->term.ret.args = {};

    translate_block(irc, cfg.exit, functionDecl->decl.rval->func.body);
    cfg.exit->id = cfg.numBlocks++;
    init_terminator(Terminator::Kind::kRet, cfg.exit);

    // Sets up id to block map, initial rpo and predecessors
    LList<BasicBlock *> bbMap;
    bbMap.init(cfg.numBlocks);
    bbMap.size = cfg.numBlocks;
    FixedBitField visited;
    visited.init(cfg.numBlocks);
    BasicBlock **dfsStack = mem::c_malloc<BasicBlock *>(cfg.numBlocks - 1);
    dfsStack[0] = cfg.entry;
    cfg.entry->pred = {};
    size_t stackSize = 1;
    while (stackSize) {
        BasicBlock *curr = dfsStack[--stackSize];
        bbMap[curr->id] = curr;

        for (auto succ = succ_begin(curr), end = succ_end(curr); succ != end; ++succ) {
            if (!visited[(*succ)->id]) {
                (*succ)->pred = {};
                dfsStack[stackSize++] = *succ;
                visited.set((*succ)->id);
            }
            (*succ)->pred.add(curr);
        }
    }

    // Split critical edges, and initialize rpo/po
    LList<BasicBlock *> rpo;
    rpo.init(cfg.numBlocks);
    for (size_t i = 0, numBlocks = cfg.numBlocks; i < numBlocks; i++) {
        BasicBlock *src = bbMap[i];
        rpo.add(src);

        size_t succCnt = succ_count(src);
        if (succCnt > 1) {
            for (size_t i = 0; i < succCnt; i++) {
                BasicBlock *tgt = src->term.succ[i];
                if (tgt->pred.size > 1) {
                    BasicBlock *newBlock = create_block(cfg);
                    init_terminator(Terminator::Kind::kGoto, newBlock);
                    newBlock->term.tgoto.target = tgt;
                    newBlock->pred.init(1);
                    newBlock->pred.add(src);

                    // Replace predecessor in target with newBlock
                    size_t k = 0;
                    for (; k < tgt->pred.size; k++) {
                        if (tgt->pred[k] == src) {
                            tgt->pred[k] = newBlock;
                            break;
                        }
                    }
                    assert(k != tgt->pred.size);

                    src->term.succ[i] = newBlock;

                    bbMap.add(newBlock);
                    rpo.add(newBlock);
                }
            }
        }
    }
    // Set up po to be the exact opposite order of rpo
    cfg.po = mem::c_malloc<BasicBlock *>(cfg.numBlocks);
    for (size_t i = 0; i < cfg.numBlocks; i++) {
        cfg.po[i] = rpo[cfg.numBlocks - i - 1];
    }
    cfg.rpo = rpo.data;

    cfg.idom = dom_tree(cfg);

    // Simple Dominance Frontier algorithm by Cooper et al.
    SparseSet *domf = mem::c_malloc<SparseSet>(cfg.numBlocks);
    for (size_t i = 0; i < cfg.numBlocks; i++) {
        domf[i].init(cfg.numBlocks);
    }
    for (auto bb = rpo_begin(cfg), end = rpo_end(cfg); bb != end; ++bb) {
        if ((*bb)->pred.size >= 2) {
            for (auto pred = pred_begin(*bb), end = pred_end(*bb); pred != end; ++pred) {
                BasicBlock *runner = *pred;
                while (runner != cfg.idom[(*bb)->id]) {
                    domf[runner->id].try_add((*bb)->id);
                    runner = cfg.idom[runner->id];
                }
            }
        }
    }

    // Iterated dominance-frontier and phi node insertion
    SparseSet philist;
    philist.init(cfg.numBlocks);
    SparseSet worklist;
    worklist.init(cfg.numBlocks);
    for (size_t i = 0; i < irc.varInfoTab.size; i++) {
        worklist.clear();
        philist.clear();
        VarInfo *var = irc.varInfoTab[i];
        for (size_t k = 0; k < var->defSites.size; k++) {
            worklist.try_add(var->defSites[k]);
        }
        while (worklist.size) {
            BlockId def = worklist.pop();
            for (size_t k = 0; k < domf[def].size; k++) {
                BlockId dfb = domf[def].dense[k];
                BasicBlock *bb = bbMap[dfb];
                if (!philist.contains(dfb)) {
                    Inst *phi = create_inst(Inst::Kind::kPhi);
                    phi->phi.varInfo = var;
                    phi->dst = cfg.nextReg++;
                    phi->phi.joins.init(bb->pred.size);

                    add_start_inst(bb, phi);

                    philist.try_add(dfb);

                    if (dfb != def) worklist.try_add(dfb);
                }
            }
        }
    }
    mem::c_free(philist.dense);
    mem::c_free(philist.sparse);
    mem::c_free(worklist.dense);
    mem::c_free(worklist.sparse);
    for (size_t i = 0; i < cfg.numBlocks; i++) {
        mem::c_free(domf[i].dense);
        mem::c_free(domf[i].sparse);
    }

    mem::c_free(bbMap.data);

    // Variable renaming
    LList<BasicBlock *> *dominates = mem::c_malloc<LList<BasicBlock *>>(cfg.numBlocks);
    for (size_t i = 0; i < cfg.numBlocks; i++) {
        dominates[i].init(cfg.numBlocks);
    }
    for (auto bb = ++rpo_begin(cfg), end = rpo_end(cfg); bb != end; ++bb) {
        dominates[cfg.idom[(*bb)->id]->id].add(*bb);
    }
    rename_block(irc, dominates, cfg.entry);
    for (size_t i = 0; i < cfg.numBlocks; i++) {
        mem::c_free(dominates[i].data);
    }
    mem::c_free(irc.useDefTab.data);

    mem::c_free(irc.declMap.table);
    for (size_t i = 0; i < irc.varInfoTab.size; i++) {
        mem::p_free(irc.varInfoTab[i]);
    }
    mem::c_free(irc.varInfoTab.data);

    mem::p_free(functionDecl->decl.rval);

    return cfg;
}

}  // namespace lcc
