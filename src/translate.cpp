#include "translate.hpp"

#include <cstdio>

#include "ast.hpp"
#include "print.hpp"

namespace lcc {

namespace {

namespace ssa {

struct VariableRef {
    size_t varId;
    ValId *valLoc;
    bool isDef;
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

struct CFG {
    size_t numBlocks{0};
    BasicBlock *entry;
    BasicBlock **map;

    LList<BasicBlock *> *pred;

    BasicBlock **rpo;
    BasicBlock **idom;
};

ListIterator<BasicBlock *> rpo_begin(CFG &cfg) { return {&cfg.rpo[0]}; }

ListIterator<BasicBlock *> rpo_end(CFG &cfg) { return {&cfg.rpo[cfg.numBlocks]}; }

ListIterator<BasicBlock *> pred_begin(CFG &cfg, BasicBlock *block) { return {&cfg.pred[block->id].data[0]}; }

ListIterator<BasicBlock *> pred_end(CFG &cfg, BasicBlock *block) {
    return {&cfg.pred[block->id].data[cfg.pred[block->id].size]};
}

ListIterator<BasicBlock *> succ_begin(BasicBlock *block) { return {&block->term.succ[0]}; }

ListIterator<BasicBlock *> succ_end(BasicBlock *block) {
    switch (block->term.kind) {
        case TerminatorKind::kCond: return {&block->term.succ[2]};
        case TerminatorKind::kGoto: return {&block->term.succ[1]};
        case TerminatorKind::kRet: return {&block->term.succ[0]};
    }
}

struct TranslationContext {
    CFG cfg;

    ValId valCnt{0};

    LMap<LStringView, VariableInfo *, lstr_hash, lstr_equal> varMap;
    LList<VariableInfo *> varInfos;

    LList<VariableRefList> refs;  // Keep track of uses and definitions for ssa variable renaming
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

BasicBlock *create_block() {
    BasicBlock *block = mem::malloc<BasicBlock>();
    block->start = nullptr;
    block->end = nullptr;
    return block;
}

BasicBlock *create_block(TranslationContext &tctx) {
    BasicBlock *block = create_block();
    block->id = tctx.cfg.numBlocks++;
    return block;
}

void translate_block(TranslationContext &tctx, BasicBlock *entry, BasicBlock *exit, Node *block);

ValId *translate_expr(TranslationContext &tctx, ValId *dest, BasicBlock *curr, Node *expr) {
    switch (expr->kind) {
        case NodeKind::kIntLit: {
            Inst *aconst = create_inst(InstKind::kConst);
            aconst->aconst.intVal = expr->intLit.intVal;
            add_inst(curr, aconst);
            mem::p_free(expr);
            *dest = aconst->aconst.dest = tctx.valCnt++;
            return &aconst->aconst.dest;
        }
        case NodeKind::kName: {
            if (expr->name.ref->decl.isAssignToGlobal) {
                todo("Use of global variable '%s'\n", lstr_raw_str(expr->name.ident));
                assert(false);
            }
            VariableRef *varRef = create_variable_reference(tctx, curr->id);
            varRef->varId = (*tctx.varMap.get(expr->name.ident))->varId;
            varRef->valLoc = dest;
            varRef->isDef = false;
            mem::p_free(expr);
            return nullptr;
        }
        case NodeKind::kInfix: {
            Inst *bin = create_inst(InstKind::kBin);
            bin->bin.op = expr->infix.op;
            translate_expr(tctx, &bin->bin.left, curr, expr->infix.left);
            translate_expr(tctx, &bin->bin.right, curr, expr->infix.right);
            add_inst(curr, bin);
            mem::p_free(expr);
            *dest = bin->bin.dest = tctx.valCnt++;
            return &bin->bin.dest;
        }
        default: assert(false && "TODO: no translation implemented for expression kind"); unreachable();
    }
}

void translate_decl(TranslationContext &tctx, BasicBlock *curr, Node *decl) {
    if (decl->decl.lval->kind == NodeKind::kName && decl->decl.rval) {
        VariableInfo *varInfo;
        if (VariableInfo **varInfoRef = tctx.varMap.get(decl->decl.lval->name.ident)) {
            varInfo = *varInfoRef;
            LList<BlockId> &defSites = varInfo->defSites;
            // Ensure no duplicates before adding to definition sites
            if (!defSites.size || defSites.last() != curr->id) {
                defSites.add(curr->id);
            }
        } else {
            varInfo = mem::malloc<VariableInfo>();
            varInfo->varId = tctx.varMap.size;
            varInfo->defSites = {};
            varInfo->idStack = {};
            varInfo->defSites.add(curr->id);
            tctx.varMap.try_put(decl->decl.lval->name.ident, varInfo);
            tctx.varInfos.add(varInfo);
        }
        ValId *valLoc;
        if (decl->decl.rval->kind == NodeKind::kName) {
            Inst *assign = create_inst(InstKind::kAssign);
            add_inst(curr, assign);
            translate_expr(tctx, &assign->assign.assignId, curr, decl->decl.rval);
            assign->assign.dest = tctx.valCnt++;
            valLoc = &assign->assign.dest;
        } else {
            ValId temp;
            valLoc = translate_expr(tctx, &temp, curr, decl->decl.rval);
        }
        VariableRef *varRef = create_variable_reference(tctx, curr->id);
        varRef->varId = varInfo->varId;
        varRef->valLoc = valLoc;
        varRef->isDef = true;
    }
}

void translate_if(TranslationContext &tctx, BasicBlock *entry, BasicBlock *exit, Node *ifstmt) {
    for (;;) {
        entry->term.kind = TerminatorKind::kCond;
        translate_expr(tctx, &entry->term.predicate, entry, ifstmt->ifstmt.cond);

        BasicBlock *then = create_block(tctx);
        entry->term.cond.then = then;
        translate_block(tctx, then, exit, ifstmt->ifstmt.then);

        if (!ifstmt->ifstmt.alt) {
            entry->term.cond.alt = exit;
            return;
        }
        BasicBlock *alt = create_block(tctx);
        entry->term.cond.alt = alt;
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
    entry->term.kind = TerminatorKind::kCond;
    entry->term.cond.alt = exit;
    translate_expr(tctx, &entry->term.predicate, entry, whilestmt->whilestmt.cond);

    // Goto targets for break and continue statements
    whilestmt->whilestmt.info->entry = entry;
    whilestmt->whilestmt.info->exit = exit;

    BasicBlock *loop = create_block(tctx);
    entry->term.cond.then = loop;

    loop->term.kind = TerminatorKind::kGoto;
    loop->term.tgoto.target = entry;

    translate_block(tctx, loop, entry, whilestmt->whilestmt.loop);
}

void translate_block(TranslationContext &tctx, BasicBlock *entry, BasicBlock *exit, Node *block) {
    assert(block->kind == NodeKind::kBlock);
    BasicBlock *curr = entry;
    for (StatementListNode *stmtNode = block->block.start; stmtNode; stmtNode = stmtNode->next) {
        Node *stmt = stmtNode->stmt;
        switch (stmt->kind) {
            case NodeKind::kDecl: {
                translate_decl(tctx, curr, stmt);
                break;
            }
            case NodeKind::kIf: {
                if (stmtNode->next) {
                    BasicBlock *ifExit = create_block();
                    translate_if(tctx, curr, ifExit, stmt);
                    ifExit->id = tctx.cfg.numBlocks++;
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
                    curr->term.kind = TerminatorKind::kGoto;
                    curr->term.tgoto.target = cond;
                    curr = cond;
                }
                if (stmtNode->next) {
                    BasicBlock *whileExit = create_block();
                    translate_while(tctx, curr, whileExit, stmt);
                    whileExit->id = tctx.cfg.numBlocks++;
                    curr = whileExit;
                    break;
                } else {
                    translate_while(tctx, curr, exit, stmt);
                    return;
                }
                break;
            }
            case NodeKind::kLoopBr: {
                curr->term.kind = TerminatorKind::kGoto;
                if (stmt->loopbr.isBreak) {
                    curr->term.tgoto.target = stmt->loopbr.ref->whilestmt.info->exit;
                } else {
                    curr->term.tgoto.target = stmt->loopbr.ref->whilestmt.info->entry;
                }
                return;
            }
            default: break;
        }
    }
    curr->term.kind = TerminatorKind::kGoto;
    curr->term.tgoto.target = exit;
    mem::p_free(block);
}

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
            for (auto pred = pred_begin(cfg, *bb), end = pred_end(cfg, *bb); pred != end; ++pred) {
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
    for (auto succ = succ_begin(curr), end = succ_end(curr); succ != end; ++succ) {
        if (!visited.get((*succ)->id)) {
            visited.set((*succ)->id);
            dfs(cfg, *succ, toVisit, visited);
        }
        cfg.pred[(*succ)->id].add(curr);
    }
    cfg.rpo[--toVisit] = curr;
}

void init_cfg(CFG &cfg) {
    cfg.map = mem::c_malloc<BasicBlock *>(cfg.numBlocks);
    cfg.pred = mem::c_malloc<LList<BasicBlock *>>(cfg.numBlocks);
    for (size_t i = 0; i < cfg.numBlocks; i++) {
        cfg.pred[i] = {};
    }
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
            if (currRef->isDef) {
                varInfo->idStack.add(*currRef->valLoc);
                numDefs[varInfo->varId]++;
            } else {
                *currRef->valLoc = varInfo->idStack.last();
            }
            VariableRef *next = currRef->nextRef;
            mem::p_free(currRef);
            currRef = next;
        }
    }

    // Add joins to phi nodes
    for (auto succ = succ_begin(curr), end = succ_end(curr); succ != end; ++succ) {
        Inst *phi = (*succ)->start;
        while (phi && phi->kind == InstKind::kPhi) {
            VariableInfo *varInfo = tctx.varInfos.get(phi->phi.varId);
            phi->phi.joins.add(varInfo->idStack.last());
            phi = phi->next;
        }
    }

    // Translate dominator children
    // TODO: figure out if this can be done using rpo iteration
    for (size_t i = 0; i < dominates[curr->id].size; i++) {
        rename_block(tctx, dominates, dominates[curr->id].get(i));
    }

    // Pop id stack for each variable
    for (size_t i = 0; i < tctx.varMap.size; i++) {
        tctx.varInfos.get(i)->idStack.size -= numDefs[i];
    }
    mem::c_free(numDefs);
}

BasicBlock *translate_function_body(Node *functionBody) {
    TranslationContext tctx{};
    CFG &cfg = tctx.cfg;
    tctx.varMap.init();

    // Translate function body into control flow graph
    cfg.entry = create_block(tctx);
    BasicBlock *exit = create_block();
    translate_block(tctx, cfg.entry, exit, functionBody);
    exit->id = cfg.numBlocks++;
    exit->term.kind = TerminatorKind::kRet;

    init_cfg(cfg);

    // Simple Dominance Frontier algorithm by Cooper et al.
    SparseSet *domf = mem::c_malloc<SparseSet>(cfg.numBlocks);
    for (size_t i = 0; i < cfg.numBlocks; i++) {
        domf[i].init(cfg.numBlocks);
    }
    for (auto bb = rpo_begin(cfg), end = rpo_end(cfg); bb != end; ++bb) {
        if (cfg.pred[(*bb)->id].size >= 2) {
            for (auto pred = pred_begin(cfg, *bb), end = pred_end(cfg, *bb); pred != end; ++pred) {
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
    for (size_t i = 0; i < tctx.varInfos.size; i++) {
        worklist.clear();
        philist.clear();
        VariableInfo *var = tctx.varInfos.get(i);
        for (size_t k = 0; k < var->defSites.size; k++) {
            worklist.try_add(var->defSites.get(k));
        }
        while (worklist.size) {
            BlockId def = worklist.pop();
            for (size_t k = 0; k < domf[def].size; k++) {
                BlockId dfb = domf[def].dense[k];
                if (!philist.contains(dfb)) {
                    Inst *phi = create_inst(InstKind::kPhi);
                    phi->phi.varId = var->varId;
                    phi->phi.dest = tctx.valCnt++;
                    phi->phi.joins.init(cfg.pred[dfb].size);

                    phi->next = cfg.map[dfb]->start;
                    cfg.map[dfb]->start = phi;

                    philist.try_add(dfb);

                    // TODO: optimization where a node shouldn't be added if
                    // the variable is already defined there
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
    mem::c_free(tctx.refs.data);

    // TODO: free all the VariableInfo * inside the map
    mem::c_free(tctx.varMap.table);
    mem::p_free(functionBody);
    return cfg.entry;
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
            case InstKind::kAssign: printf("  v%d = v%d\n", inst->assign.dest, inst->assign.assignId); break;
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
            printf("  if v%d -> b%d b%d\n", basicBlock->term.predicate, then->id, alt->id);

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
