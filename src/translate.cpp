#include "translate.hpp"

#include <cstdio>

#include "ast.hpp"
#include "print.hpp"

namespace lcc {

ListIterator<BasicBlock *> rpo_begin(CFG &cfg) { return {&cfg.rpo[0]}; }

ListIterator<BasicBlock *> rpo_end(CFG &cfg) { return {&cfg.rpo[cfg.numBlocks]}; }

ListIterator<BasicBlock *> pred_begin(BasicBlock *block) { return {&block->pred.data[0]}; }

ListIterator<BasicBlock *> pred_end(BasicBlock *block) { return {&block->pred.data[block->pred.size]}; }

ListIterator<BasicBlock *> succ_begin(BasicBlock *block) { return {&block->term.succ[0]}; }

ListIterator<BasicBlock *> succ_end(BasicBlock *block) {
    switch (block->term.kind) {
        case TerminatorKind::kCond: return {&block->term.succ[2]};
        case TerminatorKind::kGoto: return {&block->term.succ[1]};
        case TerminatorKind::kRet: return {&block->term.succ[0]};
    }
}

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

struct TranslationContext {
    CFG &cfg;

    BasicBlock *currBB{nullptr};

    VReg regCnt{0};

    LMap<Node *, VarInfo *, ptr_hash<Node>, ptr_equal<Node>> declMap{};  // Maps ast decl to variable info
    LList<VarInfo *> varInfoTab{};
    LList<RenameVarLL> useDefTab{};  // Keep track of uses and defs for each block in ssa variable renaming
};

Inst *create_inst(Inst::Kind kind) {
    Inst *inst = mem::p_malloc<Inst>();
    inst->kind = kind;
    inst->next = nullptr;
    return inst;
}

void add_inst(TranslationContext &tctx, Inst *inst) {
    if (tctx.currBB->start) {
        tctx.currBB->end->next = inst;
    } else {
        tctx.currBB->start = inst;
    }
    tctx.currBB->end = inst;
}

RenameVar *create_use_def(TranslationContext &tctx) {
    while (tctx.useDefTab.size <= tctx.currBB->id) {
        tctx.useDefTab.add({nullptr, nullptr});
    }

    RenameVarLL &usedefList = tctx.useDefTab[tctx.currBB->id];

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

void create_use(TranslationContext &tctx, VarInfo *varInfo, Inst **ref) {
    RenameVar *use = create_use_def(tctx);
    use->kind = RenameVar::Kind::kUse;
    use->varid = varInfo->varid;
    use->use = ref;
}

void create_def(TranslationContext &tctx, VarInfo *varInfo, Inst *ref) {
    RenameVar *def = create_use_def(tctx);
    def->kind = RenameVar::Kind::kDef;
    def->varid = varInfo->varid;
    def->def = ref;
}

VarInfo *init_new_variable(TranslationContext &tctx, Node *decl) {
    VarInfo *info = mem::p_malloc<VarInfo>();
    info->varid = tctx.varInfoTab.size;
    info->defSites = {};
    info->defStack = {};
    info->defSites.add(tctx.currBB->id);

    tctx.declMap.try_put(decl, info);
    tctx.varInfoTab.add(info);

    return info;
}

BasicBlock *create_empty_block() {
    BasicBlock *block = mem::malloc<BasicBlock>();
    block->start = nullptr;
    block->end = nullptr;
    return block;
}

BasicBlock *create_block(CFG &cfg) {
    BasicBlock *block = create_empty_block();
    block->id = cfg.numBlocks++;
    return block;
}

void translate_block(TranslationContext &tctx, BasicBlock *exit, Node *block);

void translate_expr(TranslationContext &tctx, Opd &out, Node *expr) {
    switch (expr->kind) {
        case NodeKind::kIntLit: {
            out.kind = Opd::Kind::kConst;
            out.intval = expr->intLit.intVal;
            mem::p_free(expr);
            break;
        }
        case NodeKind::kName: {
            if (expr->name.ref->decl.isAssignToGlobal) {
                todo("Use of global variable '%s'\n", lstr_raw_str(expr->name.ident));
                assert(false);
            }
            out.kind = Opd::Kind::kReg;
            out.regVal = nullptr;
            VarInfo *info = (*tctx.declMap[expr->name.ref]);
            create_use(tctx, info, &out.regVal);
            mem::p_free(expr);
            break;
        }
        case NodeKind::kInfix: {
            Inst *bin = create_inst(Inst::Kind::kBin);
            bin->bin.op = expr->infix.op;
            translate_expr(tctx, bin->bin.left, expr->infix.left);
            translate_expr(tctx, bin->bin.right, expr->infix.right);
            mem::p_free(expr);
            if (bin->bin.left.kind == Opd::Kind::kConst && bin->bin.right.kind == Opd::Kind::kConst) {
                out.kind = Opd::Kind::kConst;
                switch (expr->infix.op) {
                    case TokenType::kAdd: out.intval = bin->bin.left.intval + bin->bin.right.intval; break;
                    default: assert(false && "TODO: unimplemented constant folding for infix operation");
                }
            } else {
                out.kind = Opd::Kind::kReg;
                out.regVal = bin;
                bin->dst = tctx.regCnt++;
                add_inst(tctx, bin);
            }
            break;
        }
        default: assert(false && "TODO: no translation implemented for expression kind"); unreachable();
    }
}

void translate_decl(TranslationContext &tctx, Node *decl) {
    if (decl->decl.lval->kind == NodeKind::kName && decl->decl.rval) {
        Inst *declInst = create_inst(Inst::Kind::kAssign);
        // Must calculate before translate_expr because decl->decl.rval is freed
        bool isInfix = decl->decl.rval->kind == NodeKind::kInfix;
        translate_expr(tctx, declInst->assign.src, decl->decl.rval);

        // Minimize copies of temporaries when expression was infix node
        if (isInfix && declInst->assign.src.kind == Opd::Kind::kReg) {
            Inst *srcInst = declInst->assign.src.regVal;
            mem::p_free(declInst);
            declInst = srcInst;
        } else {
            declInst->dst = tctx.regCnt++;
            add_inst(tctx, declInst);
        }

        // Keep track of definitions
        VarInfo *info;
        if (VarInfo **infoPtr = tctx.declMap[decl->decl.lval->name.ref]) {
            // Ensure no duplicated before adding to definition sites
            // Assumes that we iterate blocks in order and never revisit
            info = *infoPtr;
            if (info->defSites.last() != tctx.currBB->id) {
                info->defSites.add(tctx.currBB->id);
            }
        } else {
            info = init_new_variable(tctx, decl);
        }
        create_def(tctx, info, declInst);
    }
    mem::p_free(decl->decl.lval);
    mem::p_free(decl);
}

void translate_if(TranslationContext &tctx, BasicBlock *entry, BasicBlock *exit, Node *ifstmt) {
    for (;;) {
        entry->term.kind = TerminatorKind::kCond;

        translate_expr(tctx, entry->term.cond.predicate, ifstmt->ifstmt.cond);
        if (entry->term.cond.predicate.kind == Opd::Kind::kConst) {
            // TODO optimization where if statement of constant expression is culled
            // assert(predicate.kind == Opd::Kind::kReg);
            assert(false && "TODO: Const value if statement predicate opt");
        }

        BasicBlock *then = tctx.currBB = create_block(tctx.cfg);
        entry->term.cond.then = then;
        translate_block(tctx, exit, ifstmt->ifstmt.then);

        if (!ifstmt->ifstmt.alt) {
            entry->term.cond.alt = exit;
            mem::p_free(ifstmt);
            return;
        }
        BasicBlock *alt = tctx.currBB = create_block(tctx.cfg);
        entry->term.cond.alt = alt;
        if (ifstmt->ifstmt.alt->kind == NodeKind::kBlock) {
            translate_block(tctx, exit, ifstmt->ifstmt.alt);
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

void translate_while(TranslationContext &tctx, BasicBlock *entry, BasicBlock *exit, Node *whilestmt) {
    entry->term.kind = TerminatorKind::kCond;
    entry->term.cond.alt = exit;

    // TODO optimization where while statement of condition 0/false is culled
    // assert(predicate.kind == Opd::Kind::kReg);
    translate_expr(tctx, entry->term.cond.predicate, whilestmt->whilestmt.cond);

    // Goto targets for break and continue statements
    whilestmt->whilestmt.info->entry = entry;
    whilestmt->whilestmt.info->exit = exit;

    BasicBlock *loop = tctx.currBB = create_block(tctx.cfg);
    entry->term.cond.then = loop;

    loop->term.kind = TerminatorKind::kGoto;
    loop->term.tgoto.target = entry;

    translate_block(tctx, entry, whilestmt->whilestmt.loop);

    mem::p_free(whilestmt);
}

void translate_block(TranslationContext &tctx, BasicBlock *exit, Node *block) {
    assert(block->kind == NodeKind::kBlock);
    for (StatementListNode *stmtNode = block->block.start; stmtNode; stmtNode = stmtNode->next) {
        Node *stmt = stmtNode->stmt;
        switch (stmt->kind) {
            case NodeKind::kDecl: {
                translate_decl(tctx, stmt);
                break;
            }
            case NodeKind::kIf: {
                if (stmtNode->next) {
                    BasicBlock *ifExit = create_empty_block();
                    translate_if(tctx, tctx.currBB, ifExit, stmt);
                    ifExit->id = tctx.cfg.numBlocks++;

                    tctx.currBB = ifExit;
                    break;
                } else {
                    translate_if(tctx, tctx.currBB, exit, stmt);
                    mem::p_free(block);
                    return;
                }
                break;
            }
            case NodeKind::kWhile: {
                if (tctx.currBB->start) {
                    BasicBlock *cond = create_block(tctx.cfg);
                    tctx.currBB->term.kind = TerminatorKind::kGoto;
                    tctx.currBB->term.tgoto.target = cond;
                    tctx.currBB = cond;
                }
                if (stmtNode->next) {
                    BasicBlock *whileExit = create_empty_block();
                    translate_while(tctx, tctx.currBB, whileExit, stmt);
                    whileExit->id = tctx.cfg.numBlocks++;
                    tctx.currBB = whileExit;
                    break;
                } else {
                    translate_while(tctx, tctx.currBB, exit, stmt);
                    mem::p_free(block);
                    return;
                }
                break;
            }
            case NodeKind::kLoopBr: {
                tctx.currBB->term.kind = TerminatorKind::kGoto;
                if (stmt->loopbr.isBreak) {
                    tctx.currBB->term.tgoto.target = stmt->loopbr.ref->whilestmt.info->exit;
                } else {
                    tctx.currBB->term.tgoto.target = stmt->loopbr.ref->whilestmt.info->entry;
                }
                mem::p_free(block);
                return;
            }
            case NodeKind::kRet: {
                tctx.currBB->term.kind = TerminatorKind::kGoto;
                tctx.currBB->term.tgoto.target = tctx.cfg.exit;
                mem::p_free(block);
                return;
            }
            default: assert(false && "TODO: no translation implemented for statement kind"); unreachable();
        }
    }
    tctx.currBB->term.kind = TerminatorKind::kGoto;
    tctx.currBB->term.tgoto.target = exit;
    mem::p_free(block);
}  // namespace

// Simple, Fast Dominance Algorithm
// by Cooper et al.
void init_dominator_tree(CFG &cfg) {
    // post order numberings
    size_t *po = mem::c_malloc<size_t>(cfg.numBlocks);
    for (size_t p = 0; p < cfg.numBlocks; p++) {
        po[cfg.rpo[p]->id] = p;
    }

    cfg.idom = mem::c_malloc<BasicBlock *>(cfg.numBlocks);
    for (size_t i = 0; i < cfg.numBlocks; i++) {
        cfg.idom[i] = nullptr;
    }
    cfg.idom[0] = cfg.entry;
    bool changed = true;
    while (changed) {
        changed = false;

        // For each basic block excluding entry node
        for (auto bb = ++rpo_begin(cfg), end = rpo_end(cfg); bb != end; ++bb) {
            BasicBlock *newIdom = nullptr;
            for (auto pred = pred_begin(*bb), end = pred_end(*bb); pred != end; ++pred) {
                if (cfg.idom[(*pred)->id] != nullptr) {
                    if (newIdom) {
                        // Calculate dominator set intersection between predecessor
                        // and existing dominator set
                        BasicBlock *b1 = *pred;
                        BasicBlock *b2 = newIdom;
                        while (b1 != b2) {
                            while (po[b1->id] > po[b2->id]) {
                                b1 = cfg.idom[b1->id];
                            }
                            while (po[b1->id] < po[b2->id]) {
                                b2 = cfg.idom[b2->id];
                            }
                        }
                        newIdom = b1;
                    } else {
                        newIdom = *pred;
                    }
                }
            }
            if (cfg.idom[(*bb)->id] != newIdom) {
                cfg.idom[(*bb)->id] = newIdom;
                changed = true;
            }
        }
    }
}

void dfs(CFG &cfg, BasicBlock *curr, size_t &toVisit, FixedBitField &visited) {
    cfg.map[curr->id] = curr;
    curr->pred = {};
    for (auto succ = succ_begin(curr), end = succ_end(curr); succ != end; ++succ) {
        if (!visited[(*succ)->id]) {
            visited.set((*succ)->id);
            dfs(cfg, *succ, toVisit, visited);
        }
        (*succ)->pred.add(curr);
    }
    cfg.rpo[--toVisit] = curr;
}

void init_cfg(CFG &cfg) {
    cfg.map = mem::c_malloc<BasicBlock *>(cfg.numBlocks);
    cfg.rpo = mem::c_malloc<BasicBlock *>(cfg.numBlocks);

    // Note that entry block always reaches every other block
    // so only one dfs call is needed
    // Sets up id to block map, predecessor and rpo
    FixedBitField visited;
    visited.init(cfg.numBlocks);
    size_t toVisit = cfg.numBlocks;
    dfs(cfg, cfg.entry, toVisit, visited);
    visited.destroy();

    init_dominator_tree(cfg);
}

void rename_block(TranslationContext &tctx, LList<BasicBlock *> *&dominates, BasicBlock *curr) {
    size_t *numDefs = mem::c_malloc<size_t>(tctx.varInfoTab.size);
    memset(numDefs, 0, tctx.varInfoTab.size * sizeof(size_t));

    Inst *phi = curr->start;
    while (phi && phi->kind == Inst::Kind::kPhi) {
        VarInfo *info = phi->phi.varInfo;
        info->defStack.add(phi);
        numDefs[info->varid]++;
        phi = phi->next;
    }

    if (curr->id < tctx.useDefTab.size) {
        RenameVar *currUseDef = tctx.useDefTab[curr->id].start;
        while (currUseDef) {
            VarInfo *info = tctx.varInfoTab[currUseDef->varid];
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
            phi->phi.joins.add({info->defStack.last(), curr});
            phi = phi->next;
        }
    }

    // Translate dominator children
    // TODO: figure out if this can be done using rpo iteration
    for (size_t i = 0; i < dominates[curr->id].size; i++) {
        rename_block(tctx, dominates, dominates[curr->id][i]);
    }

    // Pop id stack for each variable
    for (size_t i = 0; i < tctx.varInfoTab.size; i++) {
        tctx.varInfoTab[i]->defStack.size -= numDefs[i];
    }
    mem::c_free(numDefs);
}

void print_opd(Opd &opd) {
    switch (opd.kind) {
        case Opd::Kind::kReg: printf("v%d", opd.regVal->dst); break;
        case Opd::Kind::kConst: printf("%d", opd.intval); break;
    }
}

void r_print_block(BlockId &bid, BasicBlock *basicBlock) {
    printf("b%d\n", basicBlock->id);

    Inst *inst = basicBlock->start;
    while (inst) {
        printf("  ");
        print_inst(inst);
        inst = inst->next;
    }
    bid++;
    switch (basicBlock->term.kind) {
        case TerminatorKind::kGoto: {
            BasicBlock *exit = basicBlock->term.tgoto.target;
            printf("  goto b%d\n", exit->id);
            if (bid == exit->id) r_print_block(bid, exit);
            break;
        }
        case TerminatorKind::kCond: {
            BasicBlock *then = basicBlock->term.cond.then;
            BasicBlock *alt = basicBlock->term.cond.alt;

            printf("  if ");
            print_opd(basicBlock->term.cond.predicate);
            printf(" -> b%d b%d\n", then->id, alt->id);

            if (bid == then->id) r_print_block(bid, then);
            if (bid == alt->id) r_print_block(bid, alt);
            break;
        }
        case TerminatorKind::kRet: printf("  ret\n"); break;
    }
}

}  // namespace

void print_inst(Inst *inst) {
    printf("v%d = ", inst->dst);
    switch (inst->kind) {
        case Inst::Kind::kPhi:
            printf("phi(");
            for (size_t k = 0; k < inst->phi.joins.size; k++) {
                PhiInst::Arg &phiArg = inst->phi.joins[k];
                printf("v%d:%d", phiArg.ref->dst, phiArg.bb->id);
                if (k < inst->phi.joins.size - 1) {
                    printf(", ");
                }
            }
            printf(")\n");
            break;
        case Inst::Kind::kArg: printf("<arg %d>\n", inst->arg.argNo); break;
        case Inst::Kind::kAssign:
            print_opd(inst->assign.src);
            printf("\n");
            break;
        case Inst::Kind::kBin:
            const char *opstr;
            switch (inst->bin.op) {
                case TokenType::kAdd: opstr = "add"; break;
                default: opstr = "?";
            }
            printf("%s(", opstr);
            print_opd(inst->bin.left);
            printf(", ");
            print_opd(inst->bin.right);
            printf(")\n");
            break;
    }
}

void print_cfg(CFG &cfg) {
    BlockId id{0};
    r_print_block(id, cfg.entry);
}

void translate_function(CFG &cfg, Node *function) {
    TranslationContext tctx{cfg};
    tctx.declMap.init();
    tctx.varInfoTab = {};
    tctx.useDefTab = {};

    // Translate function body into control flow graph
    cfg.entry = tctx.currBB = create_block(cfg);

    // Assign vreg values to function arguments
    for (size_t i = 0; i < function->func.params.size; i++) {
        Node *argNode = function->func.params[i];

        Inst *arg = create_inst(Inst::Kind::kArg);
        arg->dst = tctx.regCnt++;
        arg->arg.argNo = i;
        add_inst(tctx, arg);

        create_def(tctx, init_new_variable(tctx, argNode), arg);
    }

    cfg.exit = create_empty_block();
    translate_block(tctx, cfg.exit, function->func.body);
    cfg.exit->id = cfg.numBlocks++;
    cfg.exit->term.kind = TerminatorKind::kRet;

    init_cfg(cfg);

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
    for (size_t i = 0; i < tctx.varInfoTab.size; i++) {
        worklist.clear();
        philist.clear();
        VarInfo *var = tctx.varInfoTab[i];
        for (size_t k = 0; k < var->defSites.size; k++) {
            worklist.try_add(var->defSites[k]);
        }
        while (worklist.size) {
            BlockId def = worklist.pop();
            for (size_t k = 0; k < domf[def].size; k++) {
                BlockId dfb = domf[def].dense[k];
                BasicBlock *bb = cfg.map[dfb];
                if (!philist.contains(dfb)) {
                    Inst *phi = create_inst(Inst::Kind::kPhi);
                    phi->phi.varInfo = var;
                    phi->dst = tctx.regCnt++;
                    phi->phi.joins.init(bb->pred.size);

                    phi->next = bb->start;
                    bb->start = phi;

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

    // Variable renaming
    LList<BasicBlock *> *dominates = mem::c_malloc<LList<BasicBlock *>>(cfg.numBlocks);
    for (size_t i = 0; i < cfg.numBlocks; i++) {
        dominates[i].init(cfg.numBlocks);
    }
    for (auto bb = ++rpo_begin(cfg), end = rpo_end(cfg); bb != end; ++bb) {
        dominates[cfg.idom[(*bb)->id]->id].add(*bb);
    }
    rename_block(tctx, dominates, cfg.entry);
    for (size_t i = 0; i < cfg.numBlocks; i++) {
        mem::c_free(dominates[i].data);
    }
    mem::c_free(tctx.useDefTab.data);

    mem::c_free(tctx.declMap.table);
    for (size_t i = 0; i < tctx.varInfoTab.size; i++) {
        mem::p_free(tctx.varInfoTab[i]);
    }
    mem::c_free(tctx.varInfoTab.data);

    mem::p_free(function);
}

}  // namespace lcc
