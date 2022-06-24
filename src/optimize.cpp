#include "optimize.hpp"

#include "map.hpp"
#include "util.hpp"

namespace lcc {

// Dead Code Elimination
namespace dce {

/*
 * Assumes all instructions are initially dead. Finds starting live instructions with arguments and
 * instructions with side-effects (calls and returns) and marks them as live. Iteratively mark the
 * definitions used by live instructions as also alive. Work backwards until convergence.
 * Final pass removes all instructions not marked as alive
 */
void pass_mark_pessismistic(CFG &cfg) {
    LPtrMap<Inst, size_t> liveMap;
    liveMap.init();
    LList<Inst *> usedInstsList;
    usedInstsList = {};

    for (auto bb = rpo_begin(cfg), end = rpo_end(cfg); bb != end; ++bb) {
        Inst *curr = (*bb)->start;
        while (curr) {
            switch (curr->kind) {
                case Inst::Kind::kArg:
                case Inst::Kind::kCall:
                    assert(!liveMap.try_put(curr, usedInstsList.size));
                    usedInstsList.add(curr);
                    break;
                default: break;
            }
            curr = curr->next;
        }
        if ((*bb)->term.kind == Terminator::Kind::kCond) {
            Opd &predicate = (*bb)->term.cond.predicate;
            if (predicate.kind == Opd::Kind::kReg) {
                if (!liveMap.try_put(predicate.regVal, usedInstsList.size)) {
                    usedInstsList.add(predicate.regVal);
                }
            }
        }
    }
    for (size_t i = 0; i < usedInstsList.size; i++) {
        Inst *marked = usedInstsList[i];
        switch (marked->kind) {
            case Inst::Kind::kPhi:
                for (size_t i = 0; i < marked->phi.joins.size; i++) {
                    Inst *ref = marked->phi.joins[i].ref;
                    if (!liveMap.try_put(ref, usedInstsList.size)) {
                        usedInstsList.add(ref);
                    }
                }
                break;
            case Inst::Kind::kArg: break;
            case Inst::Kind::kAssign:
                if (marked->assign.src.kind == Opd::Kind::kReg) {
                    if (!liveMap.try_put(marked->assign.src.regVal, usedInstsList.size)) {
                        usedInstsList.add(marked->assign.src.regVal);
                    }
                }
                break;
            case Inst::Kind::kBin:
                if (marked->bin.left.kind == Opd::Kind::kReg) {
                    if (!liveMap.try_put(marked->bin.left.regVal, usedInstsList.size)) {
                        usedInstsList.add(marked->bin.left.regVal);
                    }
                }
                if (marked->bin.right.kind == Opd::Kind::kReg) {
                    if (!liveMap.try_put(marked->bin.right.regVal, usedInstsList.size)) {
                        usedInstsList.add(marked->bin.right.regVal);
                    }
                }
                break;
            case Inst::Kind::kCall:
                for (size_t i = 0; i < marked->call.args.size; i++) {
                    if (marked->call.args[i].kind == Opd::Kind::kReg) {
                        if (!liveMap.try_put(marked->call.args[i].regVal, usedInstsList.size)) {
                            usedInstsList.add(marked->call.args[i].regVal);
                        }
                    }
                }
                break;
        }
    }
    for (auto bb = rpo_begin(cfg), end = rpo_end(cfg); bb != end; ++bb) {
        Inst *curr = (*bb)->start;
        while (curr) {
            if (!liveMap[curr]) {
                remove_inst(*bb, curr);
            }
            curr = curr->next;
        }
    }
}

/*
 * Initially assume all instructions are alive. First pass counts all uses of a definition.
 * Second pass uses worklist algorithm to remove definitions with 0 uses and decrement use
 * count for all definitions which use the newly removed definition. Iterate until convergence.
 */
void pass_simple_optimistic(CFG &cfg) {
    LPtrMap<Inst, size_t> useCnt;
    useCnt.init();
    for (auto bb = rpo_begin(cfg), end = rpo_end(cfg); bb != end; ++bb) {
        Inst *curr = (*bb)->start;
        while (curr) {
            useCnt.try_put(curr, 0);
            switch (curr->kind) {
                case Inst::Kind::kPhi:
                    for (size_t i = 0; i < curr->phi.joins.size; i++) {
                        Inst *ref = curr->phi.joins[i].ref;
                        if (size_t *uses = useCnt.try_put(ref, 1)) {
                            (*uses)++;
                        }
                    }
                    break;
                case Inst::Kind::kArg: break;
                case Inst::Kind::kAssign:
                    if (curr->assign.src.kind == Opd::Kind::kReg) {
                        (*useCnt[curr->assign.src.regVal])++;
                    }
                    break;
                case Inst::Kind::kBin:
                    if (curr->bin.left.kind == Opd::Kind::kReg) {
                        (*useCnt[curr->bin.left.regVal])++;
                    }
                    if (curr->bin.right.kind == Opd::Kind::kReg) {
                        (*useCnt[curr->bin.right.regVal])++;
                    }
                    break;
                case Inst::Kind::kCall:
                    for (size_t i = 0; i < curr->call.args.size; i++) {
                        if (curr->call.args[i].kind == Opd::Kind::kReg) {
                            (*useCnt[curr->call.args[i].regVal])++;
                        }
                    }
                    break;
            }
            curr = curr->next;
        }
        if ((*bb)->term.kind == Terminator::Kind::kCond) {
            Opd &predicate = (*bb)->term.cond.predicate;
            if (predicate.kind == Opd::Kind::kReg) {
                (*useCnt[predicate.regVal])++;
            }
        }
    }

    bool changed = true;
    while (changed) {
        changed = false;

        for (auto bb = rpo_begin(cfg), end = rpo_end(cfg); bb != end; ++bb) {
            Inst *curr = (*bb)->start;
            while (curr) {
                Inst *next = curr->next;
                if (*useCnt[curr] == 0) {
                    switch (curr->kind) {
                        case Inst::Kind::kPhi:
                            for (size_t i = 0; i < curr->phi.joins.size; i++) {
                                Inst *ref = curr->phi.joins[i].ref;
                                (*useCnt[ref])--;
                            }
                            remove_inst(*bb, curr);
                            changed = true;
                            break;
                        case Inst::Kind::kArg:
                            // TODO: This is possible if an argument value is reassigned a new value
                            // test := :(a: u16) -> u16 {
                            //   a = 5
                            //   ret a
                            // }
                            assert(!"Function argument cannot be dead code eliminated");
                            break;
                        case Inst::Kind::kAssign:
                            if (curr->assign.src.kind == Opd::Kind::kReg) {
                                (*useCnt[curr->assign.src.regVal])--;
                            }
                            remove_inst(*bb, curr);
                            changed = true;
                            break;
                        case Inst::Kind::kBin:
                            if (curr->bin.left.kind == Opd::Kind::kReg) {
                                (*useCnt[curr->bin.left.regVal])--;
                            }
                            if (curr->bin.right.kind == Opd::Kind::kReg) {
                                (*useCnt[curr->bin.right.regVal])--;
                            }
                            remove_inst(*bb, curr);
                            changed = true;
                            break;
                        case Inst::Kind::kCall: break;
                    }
                }
                curr = next;
            }
        }
    }

    mem::c_free(useCnt.table);
}

}  // namespace dce

namespace gvnpre {

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

    void val_insert(Value val, T &item) {
        if (!valMap.try_put(val, set.size)) {
            set.add(item);
        }
    }

    T find_leader(Value val) {
        if (size_t *idx = valMap[val]) return set[*idx];
        return nullptr;
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

    void val_insert(Value val, Inst *tmp) {
        if (!find_leader(val)) {
            tmpSet.insert(val, tmp);
        }
    }

    Inst *find_leader(Value val) {
        if (this != domTmpSet) {
            if (Inst *inst = domTmpSet->find_leader(val)) return inst;
        }
        return tmpSet.find_leader(val);
    }

    AvailOutSet *domTmpSet;
    TempValueSet tmpSet;
};

struct GVNPREContext {
    CFG *cfg;

    Value valCnt{1};
    LMap<uint16_t, Value, imm_hash, imm_equal> immediatesValTable;
    LMap<Inst *, Value, ptr_hash, ptr_equal> tempValTable;
    LMap<BinValue, Value, binval_hash, binval_equal> binValTable;

    AvailOutSet *availOut;
    ExpValueSet *expGen;
    TempValueSet *phiGen;
    TempValueSet *tmpGen;

    ExpValueSet *anticIn;
};

Value lookup_process_opd(GVNPREContext &gvn, ExpValueSet &currExpGen, Opd &opd) {
    Value val;
    Expression exp;
    switch (opd.kind) {
        case Opd::Kind::kReg: {
            Value *valptr = gvn.tempValTable[opd.regVal];
            assert(valptr);
            val = *valptr;
            break;
        }
        case Opd::Kind::kImm:
            if (Value *valptr = gvn.immediatesValTable.try_put(opd.intval, gvn.valCnt)) {
                val = *valptr;
            } else {
                val = gvn.valCnt++;
            }
            break;
    }
    currExpGen.val_insert(val, exp);
    return val;
}

bool is_commutative(TokenType infixOp) {
    switch (infixOp) {
        case TokenType::kAdd: return true;
        default: return false;
    }
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
                    Value val = gvn.valCnt++;
                    gvn.tempValTable.try_put(currInst, val);
                    currPhiGen.insert(val, currInst);
                    currAvailOut.val_insert(val, currInst);
                    break;
                }
                case Inst::Kind::kArg: {
                    Value val = gvn.valCnt++;
                    gvn.tempValTable.try_put(currInst, val);
                    currTmpGen.insert(val, currInst);
                    currAvailOut.val_insert(val, currInst);
                    break;
                }
                case Inst::Kind::kAssign: {
                    Value val = lookup_process_opd(gvn, currExpGen, currInst->assign.src);
                    gvn.tempValTable.try_put(currInst, val);
                    currTmpGen.insert(val, currInst);
                    currAvailOut.val_insert(val, currInst);
                    break;
                }
                case Inst::Kind::kBin: {
                    Value v1 = lookup_process_opd(gvn, currExpGen, currInst->bin.left);
                    Value v2 = lookup_process_opd(gvn, currExpGen, currInst->bin.right);

                    Expression binExp;
                    binExp.kind = Expression::Kind::kBin;
                    binExp.bin.op = currInst->bin.op;
                    if (is_commutative(currInst->bin.op) && v1 > v2) {
                        binExp.bin.lval = v2;
                        binExp.bin.rval = v1;
                    } else {
                        binExp.bin.lval = v1;
                        binExp.bin.rval = v2;
                    }

                    Value val;
                    if (Value *valptr = gvn.binValTable.try_put(binExp.bin, gvn.valCnt)) {
                        val = *valptr;
                    } else {
                        val = gvn.valCnt++;
                    }
                    currExpGen.val_insert(val, binExp);
                    gvn.tempValTable.try_put(currInst, val);
                    currTmpGen.insert(val, currInst);
                    currAvailOut.val_insert(val, currInst);
                    break;
                }
                case Inst::Kind::kCall: break;
            }

            currInst = currInst->next;
        }
    }
}

Inst *phi_translate(Inst *inst, BasicBlock *pred, BasicBlock *succ) {
    switch (inst->kind) {
        case Inst::Kind::kPhi:
            // TODO ensure for sure that inst is in succ
            (void)succ;
            for (size_t i = 0; i < inst->phi.joins.size; i++) {
                if (inst->phi.joins[i].bb == pred) {
                    return inst->phi.joins[i].ref;
                }
            }
            assert(!"phi node must contain a relevant inst copy for the given pred");
        default: assert(!"Unimplemented phi_translate for inst kind");
    }
    unreachable();
}

/*
void phi_translate(ValuedExpSet &currAnticIn, BasicBlock *pred, BasicBlock *succ) {
    for (size_t i = 0; i < currAnticIn.set.size; i++) {
        Expression &exp = currAnticIn.set[i];
        switch (exp.kind) {
            case Expression::Kind::kImm: break;
            case Expression::Kind::kTmp: break;
            case Expression::Kind::kBin: break;
        }
    }
    (void)currAnticIn;
    (void)pred;
    (void)succ;
}
*/

void buildset_antic(GVNPREContext &gvn) {
    bool changed = true;
    while (changed) {
        changed = false;

        (void)gvn;
        /*
            for (auto bb = po_begin(*gvn.cfg), end = po_end(*gvn.cfg); bb != end; ++bb) {
                ValuedTmpSet &currTmpGen = gvn.tmpGen[(*bb)->id];
                ValuedExpSet &currAnticIn = gvn.anticIn[(*bb)->id];

                if (succ_count(*bb) == 0) {
                } else if (succ_count(*bb) == 1) {
                } else {
                }

                // Iterate over all values in anticOut excluding tmpGen
                for (size_t i = 0; i < currAnticIn.set.size; i++) {
                    Value expVal;
                    Expression &exp = currAnticOut.set[i];
                    switch (exp.kind) {
                        case Expression::Kind::kTmp: {
                            expVal = *gvn.tmpValueMap[exp.tmp];
                            if (currTmpGen.map[expVal]) {
                                printf("Temp: V%d\n", tmpVal);
                                break;
                            }
                        }
                        case Expression::Kind::kImm: expVal = *gvn.immValueMap[exp.imm];
                        case Expression::Kind::kBin:
                            expVal = *gvn.expValueMap[exp];
                            currAnticIn.map.try_put();
                            break;
                    }
                }
                break;  // TODO remove this
            }
        */
    }
}

void eliminate(GVNPREContext &gvn) {
    for (auto bb = rpo_begin(*gvn.cfg), end = rpo_end(*gvn.cfg); bb != end; ++bb) {
        Inst *currInst = (*bb)->start;
        while (currInst) {
            switch (currInst->kind) {
                case Inst::Kind::kBin: {
                    Value *valptr = gvn.tempValTable[currInst];
                    assert(valptr);
                    Inst *leadTmp = gvn.availOut[(*bb)->id].find_leader(*valptr);
                    if (leadTmp->dst != currInst->dst) {
                        currInst->kind = Inst::Kind::kAssign;
                        currInst->assign.src.kind = Opd::Kind::kReg;
                        currInst->assign.src.regVal = leadTmp;
                    } else {
                        // Do copy propagation on bin operands
                        if (currInst->bin.left.kind == Opd::Kind::kReg) {
                            Value *lvalPtr = gvn.tempValTable[currInst->bin.left.regVal];
                            assert(lvalPtr);
                            currInst->bin.left.regVal = gvn.availOut[(*bb)->id].find_leader(*lvalPtr);
                        }
                        if (currInst->bin.right.kind == Opd::Kind::kReg) {
                            Value *rvalPtr = gvn.tempValTable[currInst->bin.right.regVal];
                            assert(rvalPtr);
                            currInst->bin.right.regVal = gvn.availOut[(*bb)->id].find_leader(*rvalPtr);
                        }
                    }
                    break;
                }
                case Inst::Kind::kCall:
                    // Copy propagation on call argument opds
                    for (size_t i = 0; i < currInst->call.args.size; i++) {
                        Opd &opd = currInst->call.args[i];
                        if (opd.kind == Opd::Kind::kReg) {
                            Value *valPtr = gvn.tempValTable[opd.regVal];
                            assert(valPtr);
                            opd.regVal = gvn.availOut[(*bb)->id].find_leader(*valPtr);
                        }
                    }
                    break;
                default: break;
            }
            currInst = currInst->next;
        }
    }
}

void pass(CFG &cfg) {
    GVNPREContext gvn = {};
    gvn.cfg = &cfg;
    gvn.valCnt = 1;
    gvn.immediatesValTable.init();
    gvn.tempValTable.init();
    gvn.binValTable.init();

    gvn.expGen = mem::c_malloc<ExpValueSet>(gvn.cfg->numBlocks);
    gvn.phiGen = mem::c_malloc<TempValueSet>(gvn.cfg->numBlocks);
    gvn.tmpGen = mem::c_malloc<TempValueSet>(gvn.cfg->numBlocks);
    gvn.availOut = mem::c_malloc<AvailOutSet>(cfg.numBlocks);
    buildset_avail(gvn);

    gvn.anticIn = mem::c_malloc<ExpValueSet>(cfg.numBlocks);
    buildset_antic(gvn);

    for (size_t i = 0; i < gvn.cfg->numBlocks; i++) {
        gvn.expGen[i].destroy();
        gvn.phiGen[i].destroy();
        gvn.tmpGen[i].destroy();
    }
    mem::c_free(gvn.expGen);
    mem::c_free(gvn.phiGen);
    mem::c_free(gvn.tmpGen);

    eliminate(gvn);

    for (size_t i = 0; i < cfg.numBlocks; i++) {
        printf("b%d: ", i);
        for (size_t ii = 0; ii < gvn.availOut[i].tmpSet.set.size; ii++) {
            Inst *tmp = gvn.availOut[i].tmpSet.set[ii];
            printf("V%d: t%d, ", *gvn.tempValTable[tmp], tmp->dst);
        }
        printf("\n");
    }

    // TODO: delete. dumps all values and corresponding values
    LList<Expression> *valTab = mem::c_malloc<LList<Expression>>(gvn.valCnt);
    for (size_t i = 0; i < gvn.valCnt; i++) {
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
    for (size_t i = 0; i < gvn.valCnt; i++) {
        printf("V%d:: ", i);
        for (size_t ii = 0; ii < valTab[i].size; ii++) {
            print_exp(valTab[i][ii]);
            printf(", ");
        }
        printf("\n");
    }
    mem::c_free(valTab);

    mem::c_free(gvn.immediatesValTable.table);
    mem::c_free(gvn.tempValTable.table);
    mem::c_free(gvn.binValTable.table);

    // TODO: free Valuedsets inside anticIn and availOut
    mem::c_free(gvn.anticIn);
    mem::c_free(gvn.availOut);
}

}  // namespace gvnpre

void optimize(CFG &cfg) {
    printf("========================= After SSA CONSTRUCTION =>\n");
    print_cfg(cfg);

    printf("========================= After GVNPRE =>\n");
    gvnpre::pass(cfg);
    print_cfg(cfg);

    printf("========================= After DCE =>\n");
    dce::pass_mark_pessismistic(cfg);
    print_cfg(cfg);
}

}  // namespace lcc
