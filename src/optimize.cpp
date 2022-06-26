#include "optimize.hpp"

#include "map.hpp"
#include "util.hpp"

namespace lcc {

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
                annotate(inst);
            };
            break;
        case Inst::Kind::kBin: {
            bool lchange = const_prop_opd(inst->bin.left);
            bool rchange = const_prop_opd(inst->bin.right);
            if (lchange || rchange) {
                annotate(inst);
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
                annotate(inst);
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
    for (size_t i = 0; i < cfg.exit->term.ret.args.size; i++) {
        Opd &retOpd = cfg.exit->term.ret.args[i].opd;
        if (retOpd.kind == Opd::Kind::kReg) {
            if (!liveMap.try_put(retOpd.regVal, usedInstsList.size)) {
                usedInstsList.add(retOpd.regVal);
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

    bool val_replace(Value val, Inst *tmp) {
        if (size_t *idx = tmpSet.valMap[val]) {
            tmpSet.set[*idx] = tmp;
            return true;
        }
        if (this == domTmpSet) return false;
        return domTmpSet->val_replace(val, tmp);
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

    Value valCnt{1};
    LMap<Value, uint16_t, val_hash, val_equal> valueToImmediateTable;
    LMap<uint16_t, Value, imm_hash, imm_equal> immediatesValTable;
    LPtrMap<Inst, Value> tempValTable;
    LMap<BinValue, Value, binval_hash, binval_equal> binValTable;

    AvailOutSet *availOut;
    ExpValueSet *expGen;
    TempValueSet *phiGen;
    TempValueSet *tmpGen;

    ExpValueSet *anticIn;
};

Value *lookup(GVNPREContext &gvn, Expression &exp) {
    switch (exp.kind) {
        case Expression::Kind::kImm: return gvn.immediatesValTable[exp.imm];
        case Expression::Kind::kTmp: return gvn.tempValTable[exp.tmp];
        case Expression::Kind::kBin: return gvn.binValTable[exp.bin];
    }
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
            if (Value *valptr = gvn.immediatesValTable.try_put(opd.intval, gvn.valCnt)) {
                val = *valptr;
            } else {
                val = gvn.valCnt++;
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
                    Value val = lookup_or_add_opd(gvn, currExpGen, currInst->assign.src);
                    gvn.tempValTable.try_put(currInst, val);
                    currTmpGen.insert(val, currInst);
                    currAvailOut.val_insert(val, currInst);
                    break;
                }
                case Inst::Kind::kBin: {
                    Value v1 = lookup_or_add_opd(gvn, currExpGen, currInst->bin.left);
                    Value v2 = lookup_or_add_opd(gvn, currExpGen, currInst->bin.right);

                    Expression binExp;
                    binExp.kind = Expression::Kind::kBin;
                    binExp.bin.op = currInst->bin.op;
                    if (opt::is_commutative(currInst->bin.op) && v1 > v2) {
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
    for (auto bb = po_begin(*gvn.cfg), end = po_end(*gvn.cfg); bb != end; ++bb) {
        gvn.anticIn[(*bb)->id].init();
    }

    bool changed = true;
    while (changed) {
        changed = false;
        for (auto bb = po_begin(*gvn.cfg), end = po_end(*gvn.cfg); bb != end; ++bb) {
            ExpValueSet &currExpGen = gvn.expGen[(*bb)->id];
            TempValueSet &currTmpGen = gvn.tmpGen[(*bb)->id];
            ExpValueSet &currAnticIn = gvn.anticIn[(*bb)->id];

            for (size_t i = 0; i < currExpGen.set.size; i++) {
                Expression &exp = currExpGen.set[i];
                Value *valPtr = lookup(gvn, exp);
                assert(valPtr);
                Value expVal = *valPtr;
                if (exp.kind == Expression::Kind::kTmp) {
                    if (size_t *idx = currTmpGen.valMap[expVal]) {
                        if (currTmpGen.set[*idx] == exp.tmp) {
                            // Note: we only skip the temporary if the rval
                            // of the assignment has no value. This typically
                            // doesn't happen except for cases like "v0 = <arg 0>"
                            continue;
                        }
                    }
                }
                if (currAnticIn.val_insert(expVal, exp)) {
                    changed = true;
                }
            }

            if (succ_count(*bb) == 1) {
                // TODO phi_translate successor
                ExpValueSet &anticOut = gvn.anticIn[(*succ_begin(*bb))->id];

                // Iterate over all values in anticOut excluding temps generated in this block
                for (size_t i = 0; i < anticOut.set.size; i++) {
                    Expression &exp = anticOut.set[i];
                    Value *valPtr = lookup(gvn, exp);
                    assert(valPtr);
                    Value expVal = *valPtr;
                    if (exp.kind == Expression::Kind::kTmp) {
                        if (currTmpGen.valMap[expVal]) {
                            assert(!"TODO: test this case, didn't think this was possible...");
                            continue;
                        }
                    }
                    if (currAnticIn.val_insert(expVal, exp)) {
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
                    if (succ == end) {
                        if (exp.kind == Expression::Kind::kTmp) {
                            if (size_t *idx = currTmpGen.valMap[expVal]) {
                                if (currTmpGen.set[*idx] == exp.tmp) {
                                    continue;
                                }
                            }
                        }
                        if (currAnticIn.val_insert(expVal, exp)) {
                            changed = true;
                        }
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

    size_t i = 0;
    bool changed = true;
    while (changed && i < 2) {
        i++;
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
                        Inst *firstTmp = nullptr;
                        printf("bruh-%d\n", (*bb)->id);
                        for (auto pred = pred_begin(*bb), pend = pred_end(*bb); pred != pend; ++pred) {
                            // TODO: phi_translate expression across predecessor
                            AvailOutSet &predAvailOut = gvn.availOut[(*pred)->id];
                            if (Inst *predTmp = predAvailOut.find_leader(*anticValPtr)) {
                                printf("exists!!\n");
                                avail[numAvail++] = create_temp(predTmp);
                                hasSome = true;
                                if (!firstTmp) {
                                    firstTmp = predTmp;
                                } else if (firstTmp != predTmp) {
                                    allSame = false;
                                }
                            } else {
                                printf("doesnt!!\n");
                                // TODO: fix this after implementing phi_translate
                                avail[numAvail++] = anticExp;
                                allSame = false;
                            }
                            if (!allSame && hasSome) break;
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
                                    if (Value *valptr = gvn.binValTable.try_put(hoistBin, gvn.valCnt)) {
                                        binVal = *valptr;
                                    } else {
                                        binVal = gvn.valCnt++;
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
                case Inst::Kind::kBin: {
                    Value *valptr = gvn.tempValTable[currInst];
                    assert(valptr);
                    Inst *leadTmp = gvn.availOut[(*bb)->id].find_leader(*valptr);
                    if (leadTmp->dst != currInst->dst) {
                        currInst->kind = Inst::Kind::kAssign;
                        currInst->assign.src.kind = Opd::Kind::kReg;
                        currInst->assign.src.regVal = leadTmp;
                        annotate(currInst);
                    } else {
                        // Do value-based copy propagation on bin operands
                        bool lchange = copy_prop_opd(gvn, *bb, currInst->bin.left);
                        bool rchange = copy_prop_opd(gvn, *bb, currInst->bin.right);
                        if (lchange || rchange) {
                            annotate(currInst);
                        }
                    }
                    break;
                }
                case Inst::Kind::kCall:
                    // Copy propagation on call argument opds
                    for (size_t i = 0; i < currInst->call.args.size; i++) {
                        if (copy_prop_opd(gvn, *bb, currInst->call.args[i])) {
                            annotate(currInst);
                        }
                    }
                    break;
                default: break;
            }
            currInst = currInst->next;
        }
        if ((*bb)->term.kind == Terminator::Kind::kCond) {
            if (copy_prop_opd(gvn, *bb, (*bb)->term.cond.predicate)) {
                // TODO: add annotation for conditional terminator
            }
        }
    }
    for (size_t i = 0; i < gvn.cfg->exit->term.ret.args.size; i++) {
        if (copy_prop_opd(gvn, gvn.cfg->exit, gvn.cfg->exit->term.ret.args[i].opd)) {
            // TODO: add annotation for conditional terminator
        }
    }
}

void pass(CFG &cfg) {
    GVNPREContext gvn = {};
    gvn.cfg = &cfg;
    gvn.valCnt = 1;
    gvn.valueToImmediateTable.init();
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

    insert(gvn);

    eliminate(gvn);

#if DBG_GVN
    for (size_t i = 0; i < cfg.numBlocks; i++) {
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
    }
#endif

    for (size_t i = 0; i < gvn.cfg->numBlocks; i++) {
        gvn.expGen[i].destroy();
        gvn.phiGen[i].destroy();
        gvn.tmpGen[i].destroy();
    }
    mem::c_free(gvn.expGen);
    mem::c_free(gvn.phiGen);
    mem::c_free(gvn.tmpGen);

#if DBG_GVN
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
#endif

    mem::c_free(gvn.valueToImmediateTable.table);
    mem::c_free(gvn.immediatesValTable.table);
    mem::c_free(gvn.tempValTable.table);
    mem::c_free(gvn.binValTable.table);

    // TODO: free Valuedsets inside anticIn and availOut
    mem::c_free(gvn.anticIn);
    mem::c_free(gvn.availOut);
}

}  // namespace gvnpre

#define PASS(name, call, args)                                             \
    do {                                                                   \
        printf("\n==================== " #name " ====================\n"); \
        start_pass(#name);                                                 \
        call(args);                                                        \
        print_cfg(cfg);                                                    \
    } while (0)

void optimize(CFG &cfg) {
    PASS(Init, (void), nullptr);
    PASS(Canon, opt::global_canon, cfg);
    PASS(GvnPre, gvnpre::pass, cfg);
    PASS(Dce, dce::pass_mark_pessismistic, cfg);
}

}  // namespace lcc
