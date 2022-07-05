#include "optimize.hpp"

#include "map.hpp"
#include "print.hpp"
#include "util.hpp"

namespace lcc {

namespace cfg_opt {

/*
 * Handles several tasks related to canonicalizing a functions cfg:
 * - rpo data structure for traversal
 * - initializing predecessor list
 * - numbering basic block id in rpo order
 * - simple edge merging optimization (if block's successor has 1 predecessor, merge)
 * - assert that there are no critical edges
 */
void canonicalize(Function &fn) {
    mem::ArenaAllocator<512> localAlloc;

    BasicBlock **stack = mem::alloc<BasicBlock *>(localAlloc, fn.numBlocks - 1);
    BasicBlock **firstRpo = mem::alloc<BasicBlock *>(localAlloc, fn.numBlocks);
    size_t *numPredAdded = mem::alloc<size_t>(localAlloc, fn.numBlocks);

    Bitset visited;
    visited.init(localAlloc, fn.numBlocks);
    stack[0] = fn.entry;
    fn.entry->predCnt = 0;
    numPredAdded[fn.entry->idx] = 0;

    // DFS to initialize first rpo ordering without merging optimizations
    // Also keep track of the number of predecessors and initialize pred data structure
    size_t stackSize = 1;
    while (stackSize) {
        BasicBlock *curr = stack[--stackSize];
        curr->pred = nullptr;

        firstRpo[curr->idx] = curr;
        for (auto succ = succ_begin(curr); succ != succ_end(curr); ++succ) {
            if (!visited[(*succ)->idx]) {
                (*succ)->predCnt = 0;
                numPredAdded[(*succ)->idx] = 0;

                stack[stackSize++] = *succ;
                visited.set((*succ)->idx);
            }
            (*succ)->predCnt++;
        }
    }

    // Initialize predecessor lists
    fn.rpo = firstRpo;
    for (auto bb = rpo_begin(fn); bb != rpo_end(fn); ++bb) {
        for (auto succ = succ_begin(*bb); succ != succ_end(*bb); ++succ) {
            if (!(*succ)->pred) {
                (*succ)->pred = mem::c_alloc<BasicBlock *>((*succ)->predCnt);
            }
            (*succ)->pred[numPredAdded[(*succ)->idx]++] = *bb;
        }
    }

    // Merge blocks whose single successor has one predecessor
    size_t numMerged = 0;
    for (size_t i = 0; i < fn.numBlocks; i++) {
        // Traverse from exit to entry to ensure merges
        // are propagated backwards correctly
        BasicBlock *curr = firstRpo[fn.numBlocks - i - 1];
        if (succ_count(curr) == 1) {
            BasicBlock *succ = *succ_begin(curr);
            if (succ->predCnt == 1) {
                for (auto ss = succ_begin(succ); ss != succ_end(succ); ++ss) {
                    for (size_t ii = 0; ii < (*ss)->predCnt; ii++) {
                        BasicBlock *succsuccpred = (*ss)->pred[ii];
                        if (succsuccpred == succ) {
                            (*ss)->pred[ii] = curr;
                        }
                    }
                }
                if (curr->end) {
                    curr->end->next = succ->start;
                } else {
                    curr->start = succ->start;
                }
                curr->end = succ->end;
                if (succ->start) {
                    succ->start->prev = curr->end;
                }
                curr->term = succ->term;
                if (succ == fn.exit) fn.exit = curr;
                mem::c_free(succ->pred);
                succ->predCnt = 0;
                numMerged++;
            }
        }
    }

    // Initialize rpo
    fn.rpo = mem::c_alloc<BasicBlock *>(fn.numBlocks - numMerged);
    size_t rpoIdx = 0;
    for (size_t i = 0; i < fn.numBlocks; i++) {
        if (firstRpo[i] == fn.entry || firstRpo[i]->predCnt) {
            fn.rpo[rpoIdx] = firstRpo[i];
            fn.rpo[rpoIdx]->idx = rpoIdx;
            rpoIdx++;
        } else {
            mem::p_free(firstRpo[i]);
        }
    }
    fn.numBlocks -= numMerged;

#if DBG
    printf("Merged %d edge(s) on first pass:\n", numMerged);
#endif

    // Ensure critical edge doesn't exist
    for (auto bb = rpo_begin(fn); bb != rpo_end(fn); ++bb) {
        if (succ_count(*bb) > 1) {
            for (auto succ = succ_begin(*bb); succ != succ_end(*bb); ++succ) {
                if ((*succ)->predCnt > 1) {
                    printf("CriticalEdge: %d->%d\n", (*bb)->idx, (*succ)->idx);
                    assert(false);
                }
            }
        }
    }

    dominator::invalidate_domtree(fn);
    dominator::invalidate_pdomtree(fn);
    dominator::invalidate_domfrontier(fn);

    return;
}

}  // namespace cfg_opt

namespace dominator {

namespace generic {

// Simple, Fast Dominance Algorithm
// by Cooper et al.
template <typename D>
void compute_dominance(Function &fn) {
    using DomTraversal = D;

    mem::ArenaAllocator<256> localAlloc;

    // post order numberings
    size_t *pnum = mem::alloc<size_t>(localAlloc, fn.numBlocks);
    for (size_t p = 0; p < fn.numBlocks; p++) {
        pnum[DomTraversal::get_block(fn, p)->idx] = p;
    }

    BasicBlock **&idom = DomTraversal::idom(fn);
    idom = mem::c_alloc<BasicBlock *>(fn.numBlocks);
    for (size_t i = 0; i < fn.numBlocks; i++) {
        idom[i] = nullptr;
    }
    BasicBlock *entry = DomTraversal::get_block(fn, 0);
    idom[entry->idx] = entry;
    bool changed = true;
    while (changed) {
        changed = false;

        // For each basic block excluding entry
        for (size_t i = 1; i < fn.numBlocks; i++) {
            BasicBlock *bb = DomTraversal::get_block(fn, i);

            BasicBlock *newIdom = nullptr;
            for (auto pred = DomTraversal::pred_begin(bb); pred != DomTraversal::pred_end(bb); ++pred) {
                if (idom[(*pred)->idx] != nullptr) {
                    if (newIdom) {
                        // Calculate dominator set intersection between predecessor
                        // and existing dominator set
                        BasicBlock *b1 = *pred;
                        BasicBlock *b2 = newIdom;
                        while (b1 != b2) {
                            while (pnum[b1->idx] > pnum[b2->idx]) {
                                b1 = idom[b1->idx];
                            }
                            while (pnum[b1->idx] < pnum[b2->idx]) {
                                b2 = idom[b2->idx];
                            }
                        }
                        newIdom = b1;
                    } else {
                        newIdom = *pred;
                    }
                }
            }
            if (idom[bb->idx] != newIdom) {
                idom[bb->idx] = newIdom;
                changed = true;
            }
        }
    }
}

}  // namespace generic

bool is_dom_valid(Function &fn) { return fn.dom != nullptr; }

bool is_pdom_valid(Function &fn) { return fn.pdom != nullptr; }

bool is_domfrontier_valid(Function &fn) { return fn.domf != nullptr; }

void invalidate_domtree(Function &fn) {
    if (is_dom_valid(fn)) {
        mem::c_free(fn.dom);
        fn.dom = nullptr;
    }
    fn.dom = nullptr;
}

void invalidate_pdomtree(Function &fn) {
    if (is_pdom_valid(fn)) {
        mem::c_free(fn.pdom);
        fn.pdom = nullptr;
    }
}

void invalidate_domfrontier(Function &fn) {
    if (is_domfrontier_valid(fn)) {
        for (size_t i = 0; i < fn.numBlocks; i++) {
            fn.domf[i].destroy(mem::cAlloc);
        }
        mem::c_free(fn.domf);
        fn.domf = nullptr;
    }
}

struct DomInfo {
    static BasicBlock **&idom(Function &fn) { return fn.dom; }
    static BasicBlock *get_block(Function &fn, size_t idx) { return fn.rpo[idx]; }
    static ListIterator<BasicBlock *> pred_begin(BasicBlock *block) { return lcc::pred_begin(block); }
    static ListIterator<BasicBlock *> pred_end(BasicBlock *block) { return lcc::pred_end(block); };
};

void compute_domtree(Function &fn) {
    if (is_dom_valid(fn)) return;
    generic::compute_dominance<DomInfo>(fn);
}

struct PDomInfo {
    static BasicBlock **&idom(Function &fn) { return fn.pdom; }
    static BasicBlock *get_block(Function &fn, size_t idx) { return fn.po[idx]; }
    static ListIterator<BasicBlock *> pred_begin(BasicBlock *block) { return lcc::succ_begin(block); }
    static ListIterator<BasicBlock *> pred_end(BasicBlock *block) { return lcc::succ_end(block); };
};

void compute_pdomtree(Function &fn) {
    assert(!"TODO: implement pdom and post-ordering for pdom calculation");
    if (is_pdom_valid(fn)) return;
    generic::compute_dominance<PDomInfo>(fn);
}

// Simple Dominance Frontier algorithm
// by Cooper et al.
void compute_domfrontier(Function &fn) {
    if (is_domfrontier_valid(fn)) return;

    compute_domtree(fn);
    assert(is_dom_valid(fn));

    fn.domf = mem::c_alloc<Bitset>(fn.numBlocks);
    for (size_t i = 0; i < fn.numBlocks; i++) {
        fn.domf[i].init(mem::cAlloc, fn.numBlocks);
    }
    for (auto bb = rpo_begin(fn); bb != rpo_end(fn); ++bb) {
        if ((*bb)->predCnt >= 2) {
            for (auto pred = pred_begin(*bb); pred != pred_end(*bb); ++pred) {
                BasicBlock *runner = *pred;
                while (runner != fn.dom[(*bb)->idx]) {
                    fn.domf[runner->idx].set((*bb)->idx);
                    runner = fn.dom[runner->idx];
                }
            }
        }
    }
}

void print_domtree(Function &fn) {
    assert(is_dom_valid(fn));

    printf("Domtree for \"");
    lstr_print(fn.ident);
    printf("\":\n");
    for (auto bb = rpo_begin(fn); bb != rpo_end(fn); ++bb) {
        printf("\tdom[B%d] = B%d\n", (*bb)->idx, fn.dom[(*bb)->idx]->idx);
    }
}

void print_dominates(Function &fn) {
    assert(is_dom_valid(fn));

    printf("Domination Sets for \"");
    lstr_print(fn.ident);
    printf("\":\n");

    mem::ArenaAllocator<512> localAlloc;
    LList<BlockIdx> *dominates = mem::alloc<LList<BlockIdx>>(localAlloc, fn.numBlocks);
    for (auto bb = rpo_begin(fn); bb != rpo_end(fn); ++bb) {
        dominates[(*bb)->idx] = {};
        dominates[(*bb)->idx].add((*bb)->idx);

        BasicBlock *dom = fn.dom[(*bb)->idx];
        while (dom->idx) {
            dominates[dom->idx].add((*bb)->idx);
            dom = fn.dom[dom->idx];
        }
    }

    for (auto bb = rpo_begin(fn); bb != rpo_end(fn); ++bb) {
        printf("\tdominates[B%d] = ", (*bb)->idx);

        LList<BlockIdx> &domSet = dominates[(*bb)->idx];
        for (size_t i = 0; i < domSet.size; i++) {
            printf("B%d", domSet[i]);
            if (i + 1 < domSet.size) {
                printf(", ");
            }
        }
        printf("\n");
    }
}

void print_domfrontier(Function &fn) {
    assert(is_domfrontier_valid(fn));

    printf("Dominance Frontier Sets for \"");
    lstr_print(fn.ident);
    printf("\":\n");

    for (auto bb = rpo_begin(fn); bb != rpo_end(fn); ++bb) {
        printf("\tdf[B%d] = ", (*bb)->idx);

        Bitset &dfSet = fn.domf[(*bb)->idx];
        auto bid = dfSet.begin();
        while (bid != dfSet.end()) {
            printf("B%d", *bid);
            ++bid;
            if (bid != dfSet.end()) {
                printf(", ");
            } else {
                break;
            }
        }
        printf("\n");
    }
}

}  // namespace dominator

namespace mem_elimination {

using VariableIdx = size_t;

struct VariableInfo {
    // TODO: add valuable information here
    VariableIdx idx;  // TODO: delete this
};

void pass(Opt &opt, Function &fn) {
    dominator::compute_domfrontier(fn);
    assert(dominator::is_domfrontier_valid(fn));

    dominator::print_domtree(fn);
    dominator::print_dominates(fn);
    dominator::print_domfrontier(fn);

    mem::ArenaAllocator<512> localAlloc;

    Bitset invalidVarIdx;
    size_t numAllocs = 0;
    LList<VariableInfo> allocInsts;
    LMap<Inst *, VariableIdx> allocInstsMap;
    allocInstsMap.init();

    // Determine local variables that can be optimized
    for (auto bb = rpo_begin(fn); bb != rpo_end(fn); ++bb) {
        for (Inst *inst = (*bb)->start; inst; inst = inst->next) {
            switch (inst->kind) {
                case Inst::Kind::kAlloc:
                    VariableInfo vInfo;
                    vInfo.idx = numAllocs++;
                    allocInsts.add(vInfo);
                    allocInstsMap.try_put(inst, vInfo.idx);
                    if (!inst->next || inst->next->kind != Inst::Kind::kAlloc) {
                        invalidVarIdx.init(localAlloc, numAllocs);
                    }
                    break;
                case Inst::Kind::kStore:
                    if (inst->st.src.kind == Opd::Kind::kReg) {
                        if (inst->st.src.regVal->kind == Inst::Kind::kAlloc) {
                            VariableIdx *vidx = allocInstsMap[inst->st.src.regVal];
                            assert(vidx);
                            invalidVarIdx.set(*vidx);
                        }
                    }
                    break;
                default: break;
            }
        }
    }

    // No allocations so there is nothing to optimize
    if (numAllocs == 0) return;

    for (auto bit = invalidVarIdx.begin(); bit != invalidVarIdx.end(); ++bit) {
        printf("::var idx=%d is invalidated\n", *bit);
    }

    // Collect all uses and defs of allocated pointers (arguments, local variables, etc.)
    LList<BasicBlock *> blocksWithDefinition;
    blocksWithDefinition.init(fn.numBlocks);
    Bitset *defSets = mem::alloc<Bitset>(localAlloc, fn.numBlocks);
    for (auto bb = rpo_begin(fn); bb != rpo_end(fn); ++bb) {
        defSets[(*bb)->idx].init(localAlloc, numAllocs);
        for (Inst *inst = (*bb)->start; inst; inst = inst->next) {
            switch (inst->kind) {
                case Inst::Kind::kStore:
                    assert(inst->st.addr.kind == Opd::Kind::kReg);
                    if (VariableIdx *vidx = allocInstsMap[inst->st.addr.regVal]) {
                        defSets[(*bb)->idx].set(*vidx);
                        break;
                    }
                default: break;
            }
        }
        defSets[(*bb)->idx].and_not(invalidVarIdx);
        if (!defSets[(*bb)->idx].all_zero()) {
            blocksWithDefinition.add(*bb);
        }
    }

    Bitset *phiSets = mem::alloc<Bitset>(localAlloc, numAllocs);
    for (size_t i = 0; i < numAllocs; i++) {
        phiSets[i].init(localAlloc, fn.numBlocks);
    }

    Bitset domFUnion, finalPhiSet;
    domFUnion.init(localAlloc, fn.numBlocks);
    finalPhiSet.init(localAlloc, fn.numBlocks);

    LMap<Inst *, VariableIdx> phiInstVarIdxMap;
    phiInstVarIdxMap.init();

    LList<BlockIdx> worklist;

    for (size_t i = 0; i < blocksWithDefinition.size; i++) {
        BasicBlock *bb = blocksWithDefinition[i];

        // Calculate union of dominance frontiers for current block
        domFUnion.reset();
        worklist.size = 0;
        worklist.add(bb->idx);
        while (worklist.size) {
            Bitset &domf = fn.domf[worklist.pop_back()];

            for (auto bidx = domf.begin(); bidx != domf.end(); ++bidx) {
                if (domFUnion.set(*bidx)) {
                    worklist.add(*bidx);
                }
            }
        }

        if (domFUnion.all_zero()) continue;

        // For each variable with a definition in this block
        // insert phi nodes where appropriate
        for (auto vidx = defSets[bb->idx].begin(); vidx != defSets[bb->idx].end(); ++vidx) {
            // Do not add phi node if phi node already exists for vidx
            finalPhiSet.and_not(domFUnion, phiSets[*vidx]);

            for (auto bidx = finalPhiSet.begin(); bidx != finalPhiSet.end(); ++bidx) {
                BasicBlock *curr = fn.rpo[*bidx];

                Inst *phi = create_inst(Inst::Kind::kPhi);
                phi->phi.joins.init(curr->predCnt);
                push_front_inst(opt, curr, phi, true);
                phiInstVarIdxMap.try_put(phi, *vidx);

                // Mark block as already having a phi node for the variable
                phiSets[*vidx].set(*bidx);
            }
        }
    }

    allocInsts.destroy();
    allocInstsMap.destroy();
    useDefInsts.destroy();

    blocksWithDefinition.destroy();

    worklist.destroy();
    phiInstVarIdxMap.destroy();
}

}  // namespace mem_elimination

#if 0

namespace opt {

bool is_commutative(TokenType infixOp) {
    switch (infixOp) {
        case TokenType::kAdd: return true;
        default: return false;
    }
}

bool const_prop_opd(Opd &opd) {
    if (opd.kind == Opd::Kind::kReg) {
        Inst *def = opd.regVal;
        if (def->kind == Inst::Kind::kAssign) {
            if (def->assign.src.kind == Opd::Kind::kImm) {
                opd.kind = Opd::Kind::kImm;
                opd.intval = def->assign.src.intval;
                return true;
            }
        }
    }
    return false;
}

void canon_inst(Inst *inst) {
    switch (inst->kind) {
        case Inst::Kind::kAssign:
            if (const_prop_opd(inst->assign.src)) {
                annotate_inst(inst);
            };
            break;
        case Inst::Kind::kBin: {
            bool lchange = const_prop_opd(inst->bin.left);
            bool rchange = const_prop_opd(inst->bin.right);
            if (lchange || rchange) {
                annotate_inst(inst);
            }
            if (inst->bin.left.kind == Opd::Kind::kImm && inst->bin.right.kind == Opd::Kind::kImm) {
                inst->kind = Inst::Kind::kAssign;

                switch (inst->bin.op) {
                    case TokenType::kAdd: {
                        inst->assign.src.kind = Opd::Kind::kImm;
                        inst->assign.src.intval = inst->bin.left.intval + inst->bin.right.intval;
                        break;
                    }
                    default: assert(!"TODO: unimplemented constant folding for infix operation");
                }
                annotate_inst(inst);
            }
            break;
        }
        default: break;
    }
}

void global_canon(CFG &cfg) {
    for (auto bb = rpo_begin(cfg), end = rpo_end(cfg); bb != end; ++bb) {
        for (Inst *inst = (*bb)->start; inst; inst = inst->next) {
            canon_inst(inst);
        }
    }
}

}  // namespace opt

// Dead Code Elimination
namespace dce {

struct DCE {
    LPtrMap<Inst, size_t> liveMap;
    LList<Inst *> usedInstsList;
};

void mark_opd(DCE &dce, Opd &opd) {
    if (opd.kind == Opd::Kind::kReg) {
        if (!dce.liveMap.try_put(opd.regVal, dce.usedInstsList.size)) {
            dce.usedInstsList.add(opd.regVal);
        }
    }
}

/*
 * Assumes all instructions are initially dead. Finds starting live instructions with arguments and
 * instructions with side-effects (calls and returns) and marks them as live. Iteratively mark the
 * definitions used by live instructions as also alive. Work backwards until convergence.
 * Final pass removes all instructions not marked as alive
 */
void pass_mark_pessismistic(CFG &cfg) {
    DCE dce;
    dce.liveMap.init();
    dce.usedInstsList = {};

    for (auto bb = rpo_begin(cfg), end = rpo_end(cfg); bb != end; ++bb) {
        Inst *curr = (*bb)->start;
        while (curr) {
            switch (curr->kind) {
                case Inst::Kind::kArg:
                case Inst::Kind::kCall:
                    assert(!dce.liveMap.try_put(curr, dce.usedInstsList.size));
                    dce.usedInstsList.add(curr);
                    break;
                default: break;
            }
            curr = curr->next;
        }
        if ((*bb)->term.kind == Terminator::Kind::kCond) {
            mark_opd(dce, (*bb)->term.cond.predicate);
        }
    }
    for (size_t i = 0; i < cfg.exit->term.ret.args.size; i++) {
        mark_opd(dce, cfg.exit->term.ret.args[i].opd);
    }

    for (size_t i = 0; i < dce.usedInstsList.size; i++) {
        Inst *marked = dce.usedInstsList[i];
        switch (marked->kind) {
            case Inst::Kind::kPhi:
                for (size_t i = 0; i < marked->phi.joins.size; i++) {
                    Inst *ref = marked->phi.joins[i].ref;
                    if (!dce.liveMap.try_put(ref, dce.usedInstsList.size)) {
                        dce.usedInstsList.add(ref);
                    }
                }
                break;
            case Inst::Kind::kArg: break;
            case Inst::Kind::kAssign: mark_opd(dce, marked->assign.src); break;
            case Inst::Kind::kBin:
                mark_opd(dce, marked->bin.left);
                mark_opd(dce, marked->bin.right);
                break;
            case Inst::Kind::kCall:
                for (size_t i = 0; i < marked->call.args.size; i++) {
                    mark_opd(dce, marked->call.args[i]);
                }
                break;
            case Inst::Kind::kLoad: mark_opd(dce, marked->ld.src); break;
        }
    }
    for (auto bb = rpo_begin(cfg), end = rpo_end(cfg); bb != end; ++bb) {
        Inst *curr = (*bb)->start;
        while (curr) {
            if (!dce.liveMap[curr]) {
                free_inst(curr);
            }
            curr = curr->next;
        }
    }
}

}  // namespace dce

namespace gvnpre {

#define DBG_GVN 0

using Value = size_t;
Value kNone{0};

uint32_t imm_hash(uint16_t &imm) { return imm; }

bool imm_equal(uint16_t &im1, uint16_t &im2) { return im1 == im2; }

struct BinValue {
    TokenType op;
    Value lval;
    Value rval;
};

uint32_t binval_hash(BinValue &bin) {
#define HASH(hash, val) (hash) = (((hash) << 5) - (hash)) + (val)
    size_t hash = bin.lval;
    HASH(hash, bin.op);
    HASH(hash, bin.rval);
    return hash;
#undef HASH
}

bool binval_equal(BinValue &b1, BinValue &b2) {
    if (b1.lval != b2.lval) return false;
    if (b1.op != b2.op) return false;
    if (b1.rval != b2.rval) return false;
    return true;
}

struct Expression {
    enum Kind {
        kImm,
        kTmp,
        kBin,
    } kind;

    union {
        uint16_t imm;
        Inst *tmp;
        BinValue bin;
    };
};

Expression create_temp(Inst *tmp) {
    Expression exp;
    exp.kind = Expression::Kind::kTmp;
    exp.tmp = tmp;
    return exp;
}

Expression create_imm(uint16_t imm) {
    Expression exp;
    exp.kind = Expression::Kind::kImm;
    exp.imm = imm;
    return exp;
}

Expression create_bin(TokenType op, Value lval, Value rval) {
    Expression exp;
    exp.kind = Expression::Kind::kBin;
    exp.bin.op = op;
    if (opt::is_commutative(op) && lval > rval) {
        exp.bin.lval = rval;
        exp.bin.rval = lval;
    } else {
        exp.bin.lval = lval;
        exp.bin.rval = rval;
    }
    return exp;
}

uint32_t exp_hash(Expression &exp) {
#define HASH(hash, val) (hash) = (((hash) << 5) - (hash)) + (val)
    size_t hash = exp.kind;
    switch (exp.kind) {
        case Expression::Kind::kImm: HASH(hash, exp.imm); break;
        case Expression::Kind::kTmp: HASH(hash, exp.tmp->dst); break;
        case Expression::Kind::kBin: HASH(hash, binval_hash(exp.bin)); break;
    }
    return hash;
#undef HASH
}

bool exp_equal(Expression &e1, Expression &e2) {
    if (e1.kind != e2.kind) return false;
    switch (e1.kind) {
        case Expression::Kind::kImm:
            if (e1.imm != e2.imm) return false;
            break;
        case Expression::Kind::kTmp:
            if (e1.tmp != e2.tmp) return false;
            break;
        case Expression::Kind::kBin:
            if (!binval_equal(e1.bin, e2.bin)) return false;
            break;
    }
    return true;
}

void print_exp(Expression &exp) {
    switch (exp.kind) {
        case Expression::Kind::kImm: printf("c%d", exp.imm); break;
        case Expression::Kind::kTmp: printf("t%d", exp.tmp->dst); break;
        case Expression::Kind::kBin:
            printf("V%d%sV%d", exp.bin.lval, token_type_string(exp.bin.op), exp.bin.rval);
            break;
    }
}

uint32_t val_hash(Value &val) { return val; }

bool val_equal(Value &val1, Value &val2) { return val1 == val2; }

template <typename T>
struct ValueSet {
    void init() {
        valMap.init();
        set = {};
    }

    void destroy() {
        mem::c_free(valMap.table);
        mem::c_free(set.data);
    }

    void insert(Value val, T &item) {
        valMap.try_put(val, set.size);
        set.add(item);
    }

    // Returns whether or not a new value was inserted
    bool val_insert(Value val, T &item) {
        if (!valMap.try_put(val, set.size)) {
            set.add(item);
            return true;
        }
        return false;
    }

    LMap<Value, size_t, val_hash, val_equal> valMap;
    LList<T> set{};
};

using ExpValueSet = ValueSet<Expression>;
using TempValueSet = ValueSet<Inst *>;

struct AvailOutSet {
    void init(AvailOutSet *domSet) {
        domTmpSet = domSet;
        tmpSet.init();
    }

    void val_replace(Value val, Inst *tmp) {
        if (!r_val_replace(val, tmp)) {
            tmpSet.insert(val, tmp);
        }
    }

    bool r_val_replace(Value val, Inst *tmp) {
        if (size_t *idx = tmpSet.valMap[val]) {
            tmpSet.set[*idx] = tmp;
            return true;
        }
        if (this == domTmpSet) return false;
        return domTmpSet->r_val_replace(val, tmp);
    }

    void val_insert(Value val, Inst *tmp) {
        if (!find_leader(val)) {
            tmpSet.insert(val, tmp);
        }
    }

    Inst *find_leader(Value val) {
        if (this != domTmpSet) {
            if (Inst *inst = domTmpSet->find_leader(val)) return inst;
        }
        if (size_t *idx = tmpSet.valMap[val]) {
            return tmpSet.set[*idx];
        }
        return nullptr;
    }

    AvailOutSet *domTmpSet;
    TempValueSet tmpSet;
};

struct GVNPREContext {
    CFG *cfg;

    Value nextVal{1};
    LMap<Value, uint16_t, val_hash, val_equal> valueToImmediateTable;
    LMap<uint16_t, Value, imm_hash, imm_equal> immediatesValTable;
    LPtrMap<Inst, Value> tempValTable;
    LMap<BinValue, Value, binval_hash, binval_equal> binValTable;

    ExpValueSet *expGen;
    TempValueSet *phiGen;
    TempValueSet *tmpGen;
    AvailOutSet *availOut;
    ExpValueSet *anticIn;
};

Value *lookup(GVNPREContext &gvn, Expression &exp) {
    switch (exp.kind) {
        case Expression::Kind::kImm: return gvn.immediatesValTable[exp.imm];
        case Expression::Kind::kTmp: return gvn.tempValTable[exp.tmp];
        case Expression::Kind::kBin: return gvn.binValTable[exp.bin];
    }
}

void dump_value_table(GVNPREContext &gvn) {
#if DBG_GVN
    LList<Expression> *valTab = mem::c_malloc<LList<Expression>>(gvn.nextVal);
    for (size_t i = 0; i < gvn.nextVal; i++) {
        valTab[i] = {};
    }
    for (size_t i = 0; i < gvn.tempValTable.capacity; i++) {
        if (gvn.tempValTable.table[i].psl) {
            Expression exp;
            exp.kind = Expression::Kind::kTmp;
            exp.tmp = gvn.tempValTable.table[i].key;
            valTab[gvn.tempValTable.table[i].val].add(exp);
        }
    }
    for (size_t i = 0; i < gvn.immediatesValTable.capacity; i++) {
        if (gvn.immediatesValTable.table[i].psl) {
            Expression exp;
            exp.kind = Expression::Kind::kImm;
            exp.imm = gvn.immediatesValTable.table[i].key;
            valTab[gvn.immediatesValTable.table[i].val].add(exp);
        }
    }
    for (size_t i = 0; i < gvn.binValTable.capacity; i++) {
        if (gvn.binValTable.table[i].psl) {
            Expression exp;
            exp.kind = Expression::Kind::kBin;
            exp.bin = gvn.binValTable.table[i].key;
            valTab[gvn.binValTable.table[i].val].add(exp);
        }
    }
    for (size_t i = 0; i < gvn.nextVal; i++) {
        printf("V%-2d:: ", i);
        for (size_t ii = 0; ii < valTab[i].size; ii++) {
            print_exp(valTab[i][ii]);
            printf(", ");
        }
        printf("\n");
    }
    mem::c_free(valTab);
#else
    (void)gvn;
#endif
}

void dump_analysis_sets(GVNPREContext &gvn) {
#if DBG_GVN
    for (size_t i = 0; i < gvn.cfg->numBlocks; i++) {
        printf("b%d: \n", i);
        printf("\tavail: ");
        for (size_t ii = 0; ii < gvn.availOut[i].tmpSet.set.size; ii++) {
            Inst *tmp = gvn.availOut[i].tmpSet.set[ii];
            printf("V%d: t%d, ", *gvn.tempValTable[tmp], tmp->dst);
        }
        printf("\n");
        printf("\tantic: ");
        for (size_t ii = 0; ii < gvn.anticIn[i].set.size; ii++) {
            Expression &exp = gvn.anticIn[i].set[ii];
            printf("V%d: ", *lookup(gvn, exp));
            print_exp(exp);
            printf(", ");
        }
        printf("\n");
        printf("\ttmp: ");
        for (size_t ii = 0; ii < gvn.tmpGen[i].set.size; ii++) {
            Inst *tmp = gvn.tmpGen[i].set[ii];
            printf("t%d, ", tmp->dst);
        }
        printf("\n");
        printf("\texp: ");
        for (size_t ii = 0; ii < gvn.expGen[i].set.size; ii++) {
            Expression &exp = gvn.expGen[i].set[ii];
            print_exp(exp);
            printf(", ");
        }
        printf("\n");
    }
#else
    (void)gvn;
#endif
}

Value lookup_or_add_opd(GVNPREContext &gvn, ExpValueSet &currExpGen, Opd &opd) {
    Value val;
    Expression exp;
    switch (opd.kind) {
        case Opd::Kind::kReg: {
            Value *valptr = gvn.tempValTable[opd.regVal];
            assert(valptr);
            val = *valptr;

            exp.kind = Expression::Kind::kTmp;
            exp.tmp = opd.regVal;
            break;
        }
        case Opd::Kind::kImm:
            if (Value *valptr = gvn.immediatesValTable.try_put(opd.intval, gvn.nextVal)) {
                val = *valptr;
            } else {
                val = gvn.nextVal++;
                gvn.valueToImmediateTable.try_put(val, opd.intval);
            }

            exp.kind = Expression::Kind::kImm;
            exp.imm = opd.intval;
            break;
    }
    currExpGen.val_insert(val, exp);
    return val;
}

void buildset_avail(GVNPREContext &gvn) {
    for (size_t i = 0; i < gvn.cfg->numBlocks; i++) {
        gvn.expGen[i].init();
        gvn.phiGen[i].init();
        gvn.tmpGen[i].init();
    }

    for (auto bb = rpo_begin(*gvn.cfg), end = rpo_end(*gvn.cfg); bb != end; ++bb) {
        ExpValueSet &currExpGen = gvn.expGen[(*bb)->id];
        TempValueSet &currPhiGen = gvn.phiGen[(*bb)->id];
        TempValueSet &currTmpGen = gvn.tmpGen[(*bb)->id];

        AvailOutSet &domAvailOut = gvn.availOut[gvn.cfg->idom[(*bb)->id]->id];
        AvailOutSet &currAvailOut = gvn.availOut[(*bb)->id];
        currAvailOut.init(&domAvailOut);

        Inst *currInst = (*bb)->start;
        while (currInst) {
            switch (currInst->kind) {
                case Inst::Kind::kPhi: {
                    Value val = gvn.nextVal++;
                    gvn.tempValTable.try_put(currInst, val);
                    currPhiGen.insert(val, currInst);
                    currAvailOut.val_insert(val, currInst);
                    break;
                }
                case Inst::Kind::kArg: {
                    Value val = gvn.nextVal++;
                    gvn.tempValTable.try_put(currInst, val);
                    currTmpGen.insert(val, currInst);
                    currAvailOut.val_insert(val, currInst);
                    break;
                }
                case Inst::Kind::kAssign: {
                    Value val = lookup_or_add_opd(gvn, currExpGen, currInst->assign.src);
                    gvn.tempValTable.try_put(currInst, val);
                    currTmpGen.insert(val, currInst);
                    currAvailOut.val_insert(val, currInst);
                    break;
                }
                case Inst::Kind::kBin: {
                    Value v1 = lookup_or_add_opd(gvn, currExpGen, currInst->bin.left);
                    Value v2 = lookup_or_add_opd(gvn, currExpGen, currInst->bin.right);
                    Expression binExp = create_bin(currInst->bin.op, v1, v2);
                    Value val;
                    if (Value *valptr = gvn.binValTable.try_put(binExp.bin, gvn.nextVal)) {
                        val = *valptr;
                    } else {
                        val = gvn.nextVal++;
                    }
                    currExpGen.val_insert(val, binExp);
                    gvn.tempValTable.try_put(currInst, val);
                    currTmpGen.insert(val, currInst);
                    currAvailOut.val_insert(val, currInst);
                    break;
                }
                case Inst::Kind::kCall: {
                    if (!currInst->dst) break;

                    // Take conservative approach (always generate new val)
                    // because call may have side-effects
                    // TODO: perform purity analysis on function
                    Value val = gvn.nextVal++;
                    gvn.tempValTable.try_put(currInst, val);
                    currTmpGen.insert(val, currInst);
                    currAvailOut.val_insert(val, currInst);
                    break;
                }
                case Inst::Kind::kLoad: {
                    // Treat loads conservatively
                    Value val = gvn.nextVal++;
                    gvn.tempValTable.try_put(currInst, val);
                    currTmpGen.insert(val, currInst);
                    currAvailOut.val_insert(val, currInst);
                    break;
                }
            }
            currInst = currInst->next;
        }
    }
}

Expression phi_translate_inst(GVNPREContext &gvn, Inst *inst, BasicBlock *pred, BasicBlock *succ);

Expression phi_translate_opd(GVNPREContext &gvn, Opd &opd, BasicBlock *pred, BasicBlock *succ) {
    switch (opd.kind) {
        case Opd::Kind::kImm: return create_imm(opd.intval);
        case Opd::Kind::kReg:
            Value *valptr = gvn.tempValTable[opd.regVal];
            assert(valptr);
            Inst *avail = gvn.availOut[succ->id].find_leader(*valptr);

            // If avail is not in the successor then it must be from a block
            // which dominates the successor. If avail dominates successor
            // then avail must also dominate pred. Otherwise, there would
            // exist a path "entry -> pred -> succ" which does not include
            // avail but this is a contradiction (avail dominates succ).
            // This also means that avail must be available in pred
            if (avail->block != succ) return create_temp(avail);

            return phi_translate_inst(gvn, avail, pred, succ);
    }
}

Expression phi_translate_inst(GVNPREContext &gvn, Inst *inst, BasicBlock *pred, BasicBlock *succ) {
    if (inst->block != succ) return create_temp(inst);
    switch (inst->kind) {
        case Inst::Kind::kPhi:
            for (size_t i = 0; i < inst->phi.joins.size; i++) {
                if (inst->phi.joins[i].bb == pred) {
                    return create_temp(inst->phi.joins[i].ref);
                }
            }
            assert(!"phi node must contain a relevant inst copy for the given pred");
        case Inst::Kind::kAssign:
            assert(inst->assign.src.kind == Opd::Kind::kImm);
            return create_imm(inst->assign.src.intval);
        case Inst::Kind::kBin: {
            Expression lExp = phi_translate_opd(gvn, inst->bin.left, pred, succ);
            Expression rExp = phi_translate_opd(gvn, inst->bin.right, pred, succ);
            Value v1 = *lookup(gvn, lExp);
            Value v2 = *lookup(gvn, rExp);

            Expression binExp = create_bin(inst->bin.op, v1, v2);
            if (!gvn.binValTable.try_put(binExp.bin, gvn.nextVal)) {
                gvn.nextVal++;
            }
            return binExp;
        }
        case Inst::Kind::kCall: return create_temp(inst);
        default: assert(!"Unimplemented phi_translate for inst kind"); unreachable();
    }
}

Expression phi_translate(GVNPREContext &gvn, Expression &exp, BasicBlock *pred, BasicBlock *succ) {
    switch (exp.kind) {
        case Expression::Kind::kImm: return exp;
        case Expression::Kind::kTmp: return phi_translate_inst(gvn, exp.tmp, pred, succ);
        case Expression::Kind::kBin: {
            Inst *lAvail = gvn.availOut[succ->id].find_leader(exp.bin.lval);
            Value v1 = exp.bin.lval;
            if (lAvail) {
                Expression exp = phi_translate_inst(gvn, lAvail, pred, succ);
                v1 = *lookup(gvn, exp);
            }
            Inst *rAvail = gvn.availOut[succ->id].find_leader(exp.bin.rval);
            Value v2 = exp.bin.rval;
            if (rAvail) {
                Expression exp = phi_translate_inst(gvn, rAvail, pred, succ);
                v2 = *lookup(gvn, exp);
            }

            Expression binExp = create_bin(exp.bin.op, v1, v2);
            if (!gvn.binValTable.try_put(binExp.bin, gvn.nextVal)) {
                gvn.nextVal++;
            }
            return binExp;
        }
    }
}

bool try_add_antic_expression(GVNPREContext &gvn, BasicBlock *block, Expression &exp) {
    Value *valPtr = lookup(gvn, exp);
    assert(valPtr);
    Value expVal = *valPtr;
    if (exp.kind == Expression::Kind::kTmp) {
        TempValueSet &currTmpGen = gvn.tmpGen[block->id];
        if (size_t *idx = currTmpGen.valMap[expVal]) {
            if (currTmpGen.set[*idx] == exp.tmp) {
                // Note: we only skip the temporary if the rval
                // of the assignment has no value. This typically
                // doesn't happen except for cases like "v0 = <arg 0>"
                return false;
            }
        }
    }
    ExpValueSet &currAnticIn = gvn.anticIn[block->id];
    if (exp.kind == Expression::Kind::kBin) {
        bool isKilled = !currAnticIn.valMap[exp.bin.lval];
        isKilled |= !currAnticIn.valMap[exp.bin.rval];
        if (isKilled) return false;
    }
    return currAnticIn.val_insert(expVal, exp);
}

void buildset_antic(GVNPREContext &gvn) {
    for (auto bb = po_begin(*gvn.cfg), end = po_end(*gvn.cfg); bb != end; ++bb) {
        gvn.anticIn[(*bb)->id].init();
    }

    bool changed = true;
    while (changed) {
        changed = false;
        for (auto bb = po_begin(*gvn.cfg), end = po_end(*gvn.cfg); bb != end; ++bb) {
            ExpValueSet &currExpGen = gvn.expGen[(*bb)->id];

            for (size_t i = 0; i < currExpGen.set.size; i++) {
                if (try_add_antic_expression(gvn, *bb, currExpGen.set[i])) {
                    changed = true;
                }
            }
            if (succ_count(*bb) == 1) {
                BasicBlock *succ = *succ_begin(*bb);
                ExpValueSet &anticOut = gvn.anticIn[succ->id];

                // Iterate over all values in anticOut excluding temps generated in this block
                for (size_t i = 0; i < anticOut.set.size; i++) {
                    Expression exp = phi_translate(gvn, anticOut.set[i], *bb, succ);
                    if (try_add_antic_expression(gvn, *bb, exp)) {
                        changed = true;
                    }
                }
            } else if (succ_count(*bb) > 1) {
                auto begin = succ_begin(*bb);
                auto end = succ_end(*bb);
                ExpValueSet &firstAnticIn = gvn.anticIn[(*begin)->id];
                ++begin;
                for (size_t i = 0; i < firstAnticIn.set.size; i++) {
                    Expression &exp = firstAnticIn.set[i];
                    Value *valPtr = lookup(gvn, exp);
                    assert(valPtr);
                    Value expVal = *valPtr;

                    auto succ = begin;
                    while (succ != end) {
                        ExpValueSet &succAnticIn = gvn.anticIn[(*succ)->id];
                        if (!succAnticIn.valMap[expVal]) break;
                        ++succ;
                    }
                    if (succ == end && try_add_antic_expression(gvn, *bb, exp)) {
                        changed = true;
                    }
                }
            }
        }
    }
}

void insert(GVNPREContext &gvn) {
    LList<Inst *> *newPhis = mem::c_malloc<LList<Inst *>>(gvn.cfg->numBlocks);
    for (auto bb = po_begin(*gvn.cfg), end = po_end(*gvn.cfg); bb != end; ++bb) {
        newPhis[(*bb)->id] = {};
    }

    bool changed = true;
    while (changed) {
        changed = false;
        for (auto bb = po_begin(*gvn.cfg), end = po_end(*gvn.cfg); bb != end; ++bb) {
            LList<Inst *> &currNewPhis = newPhis[(*bb)->id];
            currNewPhis.size = 0;

            LList<Inst *> &domNewPhis = newPhis[gvn.cfg->idom[(*bb)->id]->id];
            for (size_t i = 0; i < currNewPhis.size; i++) {
                Inst *domTmp = domNewPhis[i];
                Value *valPtr = gvn.tempValTable[domTmp];
                assert(valPtr);
                currNewPhis.add(domTmp);
                gvn.availOut[(*bb)->id].val_replace(*valPtr, domTmp);
            }
            if ((*bb)->pred.size > 1) {
                ExpValueSet &currAnticIn = gvn.anticIn[(*bb)->id];
                for (size_t i = 0; i < currAnticIn.set.size; i++) {
                    Expression anticExp = currAnticIn.set[i];
                    if (anticExp.kind == Expression::Kind::kBin) {
                        Value *anticValPtr = lookup(gvn, anticExp);
                        assert(anticValPtr);
                        AvailOutSet &domAvailOut = gvn.availOut[gvn.cfg->idom[(*bb)->id]->id];
                        if (domAvailOut.find_leader(*anticValPtr)) {
                            continue;
                        }
                        size_t numAvail = 0;
                        Expression *avail = mem::c_malloc<Expression>((*bb)->pred.size);

                        bool hasSome = false;
                        bool allSame = true;
                        for (auto pred = pred_begin(*bb), pend = pred_end(*bb); pred != pend; ++pred) {
                            AvailOutSet &predAvailOut = gvn.availOut[(*pred)->id];
                            Expression phiTransExp = phi_translate(gvn, anticExp, *pred, *bb);
                            Value *phiTransValPtr = lookup(gvn, phiTransExp);
                            if (Inst *predTmp = predAvailOut.find_leader(*phiTransValPtr)) {
                                avail[numAvail++] = create_temp(predTmp);
                                hasSome = true;
                            } else {
                                avail[numAvail++] = phiTransExp;
                                allSame = false;
                            }
                        }
                        if (!allSame && hasSome) {
                            Inst *newPhiInst = create_inst(Inst::Kind::kPhi);
                            newPhiInst->dst = gvn.cfg->nextReg++;
                            newPhiInst->phi.joins.init((*bb)->pred.size);

                            size_t availIdx = 0;
                            for (auto pred = pred_begin(*bb), pend = pred_end(*bb); pred != pend; ++pred) {
                                Inst *hoistedTmp;
                                Expression &hoistExp = avail[availIdx];
                                if (hoistExp.kind == Expression::Kind::kBin) {
                                    BinValue &hoistBin = hoistExp.bin;

                                    AvailOutSet &predAvailOut = gvn.availOut[(*pred)->id];

                                    Inst *newBinInst = create_inst(Inst::Kind::kBin);
                                    newBinInst->dst = gvn.cfg->nextReg++;
                                    newBinInst->bin.op = hoistBin.op;

                                    if (uint16_t *imm = gvn.valueToImmediateTable[hoistBin.lval]) {
                                        newBinInst->bin.left.kind = Opd::Kind::kImm;
                                        newBinInst->bin.left.intval = *imm;
                                    } else {
                                        newBinInst->bin.left.kind = Opd::Kind::kReg;
                                        newBinInst->bin.left.regVal = predAvailOut.find_leader(hoistBin.lval);
                                    }
                                    if (uint16_t *imm = gvn.valueToImmediateTable[hoistBin.rval]) {
                                        newBinInst->bin.right.kind = Opd::Kind::kImm;
                                        newBinInst->bin.right.intval = *imm;
                                    } else {
                                        newBinInst->bin.right.kind = Opd::Kind::kReg;
                                        newBinInst->bin.right.regVal = predAvailOut.find_leader(hoistBin.rval);
                                    }
                                    add_end_inst(*pred, newBinInst);
                                    opt::canon_inst(newBinInst);

                                    Value binVal;
                                    if (Value *valptr = gvn.binValTable.try_put(hoistBin, gvn.nextVal)) {
                                        binVal = *valptr;
                                    } else {
                                        binVal = gvn.nextVal++;
                                    }
                                    gvn.tempValTable.try_put(newBinInst, binVal);
                                    predAvailOut.tmpSet.insert(binVal, newBinInst);
                                    hoistedTmp = newBinInst;
                                } else {
                                    assert(hoistExp.kind == Expression::Kind::kTmp);
                                    hoistedTmp = hoistExp.tmp;
                                }
                                PhiInst::Arg phiArg;
                                phiArg.bb = *pred;
                                phiArg.ref = hoistedTmp;
                                newPhiInst->phi.joins.add(phiArg);
                                availIdx++;
                            }
                            gvn.tempValTable.try_put(newPhiInst, *anticValPtr);
                            gvn.availOut[(*bb)->id].val_replace(*anticValPtr, newPhiInst);

                            add_start_inst(*bb, newPhiInst);
                            currNewPhis.add(newPhiInst);
                            changed = true;
                        }

                        mem::c_free(avail);
                    }
                }
            }
        }
    }
}

bool copy_prop_opd(GVNPREContext &gvn, BasicBlock *block, Opd &opd) {
    if (opd.kind == Opd::Kind::kReg) {
        Value *valPtr = gvn.tempValTable[opd.regVal];
        assert(valPtr);
        Inst *prev = opd.regVal;
        opd.regVal = gvn.availOut[block->id].find_leader(*valPtr);
        return prev != opd.regVal;
    }
    return false;
}

void eliminate(GVNPREContext &gvn) {
    for (auto bb = rpo_begin(*gvn.cfg), end = rpo_end(*gvn.cfg); bb != end; ++bb) {
        Inst *currInst = (*bb)->start;
        while (currInst) {
            switch (currInst->kind) {
                case Inst::Kind::kPhi:
                    // Copy propagation on phi joins
                    for (size_t i = 0; i < currInst->phi.joins.size; i++) {
                        PhiInst::Arg &phiArg = currInst->phi.joins[i];
                        Value *valPtr = gvn.tempValTable[phiArg.ref];
                        assert(valPtr);
                        Inst *avail = gvn.availOut[(*bb)->id].find_leader(*valPtr);
                        if (avail && avail != phiArg.ref) {
                            phiArg.ref = avail;
                            annotate_inst(currInst);
                        }
                    }
                    break;
                case Inst::Kind::kBin: {
                    Value *valptr = gvn.tempValTable[currInst];
                    assert(valptr);
                    Inst *leadTmp = gvn.availOut[(*bb)->id].find_leader(*valptr);
                    if (leadTmp->dst != currInst->dst) {
                        currInst->kind = Inst::Kind::kAssign;
                        currInst->assign.src.kind = Opd::Kind::kReg;
                        currInst->assign.src.regVal = leadTmp;
                        annotate_inst(currInst);
                    } else {
                        // Do value-based copy propagation on bin operands
                        bool lchange = copy_prop_opd(gvn, *bb, currInst->bin.left);
                        bool rchange = copy_prop_opd(gvn, *bb, currInst->bin.right);
                        if (lchange || rchange) annotate_inst(currInst);
                    }
                    break;
                }
                case Inst::Kind::kCall:
                    // Copy propagation on call argument opds
                    for (size_t i = 0; i < currInst->call.args.size; i++) {
                        if (copy_prop_opd(gvn, *bb, currInst->call.args[i])) {
                            annotate_inst(currInst);
                        }
                    }
                    break;
                default: break;
            }
            currInst = currInst->next;
        }
        if ((*bb)->term.kind == Terminator::Kind::kCond) {
            if (copy_prop_opd(gvn, *bb, (*bb)->term.cond.predicate)) {
                annotate_term((*bb)->term);
            }
        }
    }
    for (size_t i = 0; i < gvn.cfg->exit->term.ret.args.size; i++) {
        if (copy_prop_opd(gvn, gvn.cfg->exit, gvn.cfg->exit->term.ret.args[i].opd)) {
            annotate_term(gvn.cfg->exit->term);
        }
    }
}

void pass(CFG &cfg) {
    GVNPREContext gvn = {};
    gvn.cfg = &cfg;
    gvn.nextVal = 1;
    gvn.valueToImmediateTable.init();
    gvn.immediatesValTable.init();
    gvn.tempValTable.init();
    gvn.binValTable.init();

    gvn.expGen = mem::c_malloc<ExpValueSet>(gvn.cfg->numBlocks);
    gvn.phiGen = mem::c_malloc<TempValueSet>(gvn.cfg->numBlocks);
    gvn.tmpGen = mem::c_malloc<TempValueSet>(gvn.cfg->numBlocks);
    gvn.availOut = mem::c_malloc<AvailOutSet>(cfg.numBlocks);
    gvn.anticIn = mem::c_malloc<ExpValueSet>(cfg.numBlocks);

    buildset_avail(gvn);
    buildset_antic(gvn);
    insert(gvn);
    eliminate(gvn);

    dump_analysis_sets(gvn);
    dump_value_table(gvn);

    mem::c_free(gvn.valueToImmediateTable.table);
    mem::c_free(gvn.immediatesValTable.table);
    mem::c_free(gvn.tempValTable.table);
    mem::c_free(gvn.binValTable.table);

    for (size_t i = 0; i < gvn.cfg->numBlocks; i++) {
        gvn.expGen[i].destroy();
        gvn.phiGen[i].destroy();
        gvn.tmpGen[i].destroy();
    }
    mem::c_free(gvn.expGen);
    mem::c_free(gvn.phiGen);
    mem::c_free(gvn.tmpGen);

    for (size_t i = 0; i < gvn.cfg->numBlocks; i++) {
        gvn.availOut[i].tmpSet.destroy();
        gvn.anticIn[i].destroy();
    }
    mem::c_free(gvn.availOut);
    mem::c_free(gvn.anticIn);
}

}  // namespace gvnpre
#endif

#define PASS(name, call, ...)                                              \
    do {                                                                   \
        printf("\n==================== " #name " ====================\n"); \
        start_pass(opt, #name);                                            \
        call(__VA_ARGS__);                                                 \
        print_function(fn);                                                \
    } while (0)

void optimize(Opt &opt, Function &fn) {
    // PASS(Init, (void), nullptr);
    PASS(CFGCanon, cfg_opt::canonicalize, fn);
    PASS(MemElim, mem_elimination::pass, opt, fn);
    /*
    PASS(Canon, opt::global_canon, cfg);
    PASS(GvnPre, gvnpre::pass, cfg);
    PASS(Dce, dce::pass_mark_pessismistic, cfg);
    */
}

}  // namespace lcc
