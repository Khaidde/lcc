#include "optimize.hpp"

#include "map.hpp"

namespace lcc {

// Simple textbook dead code elimination
namespace dce {

struct DCE {};

void pass(CFG &cfg) {
    LMap<Inst *, size_t, ptr_hash, ptr_equal> useCnt;
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

struct Expression {
    enum Kind {
        kImm,
        kTmp,
        kBin,
    } kind;

    struct Bin {
        TokenType op;
        Value lval;
        Value rval;
    };
    union {
        Bin bin;
        uint16_t imm;
        Inst *tmp;
    };
};

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

struct ValuedExpSet {
    void init() {
        map.init();
        set = {};
    }

    using SetIdx = size_t;
    LMap<Value, SetIdx, val_hash, val_equal> map;
    LList<Expression> set{};
};

struct ValuedTmpSet {
    void init() {
        map.init();
        set = {};
    }

    void insert(Value val, Inst *inst) {
        map.try_put(val, set.size);
        set.add(inst);
    }

    Inst *find_leader(Value val) {
        if (size_t *idx = map[val]) return set[*idx];
        return nullptr;
    }

    LMap<Value, size_t, val_hash, val_equal> map;
    LList<Inst *> set{};
};

struct AvailOutSet {
    void init(AvailOutSet *domSet) {
        domTmpSet = domSet;
        tmpSet.init();
    }

    void val_insert(Value val, Inst *tmp) {
        if (!find_leader(val)) {
            tmpSet.map.try_put(val, tmpSet.set.size);
            tmpSet.set.add(tmp);
        }
    }

    Inst *find_leader(Value val) {
        if (this != domTmpSet) {
            if (Inst *inst = domTmpSet->find_leader(val)) return inst;
        }
        return tmpSet.find_leader(val);
    }

    AvailOutSet *domTmpSet;
    ValuedTmpSet tmpSet;
};

struct GVNPREContext {
    static uint32_t exp_hash(Expression &exp) {
#define HASH(hash, val) (hash) = (((hash) << 5) - (hash)) + (val)
        size_t hash = exp.kind;
        switch (exp.kind) {
            case Expression::Kind::kImm: HASH(hash, exp.imm); break;
            case Expression::Kind::kTmp: HASH(hash, exp.tmp->dst); break;
            case Expression::Kind::kBin:
                HASH(hash, exp.bin.op);
                HASH(hash, exp.bin.lval);
                HASH(hash, exp.bin.rval);
                break;
        }
        return hash;
#undef HASH
    }

    static bool exp_equal(Expression &e1, Expression &e2) {
        if (e1.kind != e2.kind) return false;
        switch (e1.kind) {
            case Expression::Kind::kImm:
                if (e1.imm != e2.imm) return false;
                break;
            case Expression::Kind::kTmp:
                if (e1.tmp != e2.tmp) return false;
                break;
            case Expression::Kind::kBin:
                if (e1.bin.op != e2.bin.op) return false;
                if (e1.bin.lval != e2.bin.lval) return false;
                if (e1.bin.rval != e2.bin.rval) return false;
        }
        return true;
    }

    static uint32_t imm_hash(uint16_t &imm) { return imm; }

    static bool imm_equal(uint16_t &i1, uint16_t &i2) { return i1 == i2; }

    CFG *cfg;

    Value valCnt{1};
    LMap<uint16_t, Value, imm_hash, imm_equal> immValueMap;
    LMap<Inst *, Value, ptr_hash, ptr_equal> tmpValueMap;
    // TODO: create a map for binary expressions exclusively
    LMap<Expression, Value, exp_hash, exp_equal> expValueMap;

    AvailOutSet *availOut;
    ValuedExpSet *anticIn;
};

Value lookup_process_opd(GVNPREContext &gvn, ValuedExpSet &currExpGen, Opd &opd) {
    Value val;
    switch (opd.kind) {
        case Opd::Kind::kReg: {
            Value *valptr = gvn.tmpValueMap[opd.regVal];
            assert(valptr);
            val = *valptr;

            if (!currExpGen.map.try_put(val, currExpGen.set.size)) {
                Expression tmp;
                tmp.kind = Expression::Kind::kTmp;
                tmp.tmp = opd.regVal;
                currExpGen.set.add(tmp);
            }
            break;
        }
        case Opd::Kind::kImm:
            if (Value *valptr = gvn.immValueMap.try_put(opd.intval, gvn.valCnt)) {
                val = *valptr;
            } else {
                val = gvn.valCnt++;
            }
            if (!currExpGen.map.try_put(val, currExpGen.set.size)) {
                Expression imm;
                imm.kind = Expression::Kind::kImm;
                imm.imm = opd.intval;
                currExpGen.set.add(imm);
            }
            break;
    }
    return val;
}

bool is_commutative(TokenType infixOp) {
    switch (infixOp) {
        case TokenType::kAdd: return true;
        default: return false;
    }
}

void buildset_avail(GVNPREContext &gvn) {
    ValuedExpSet *expGen = mem::c_malloc<ValuedExpSet>(gvn.cfg->numBlocks);
    ValuedTmpSet *phiGen = mem::c_malloc<ValuedTmpSet>(gvn.cfg->numBlocks);
    ValuedTmpSet *tmpGen = mem::c_malloc<ValuedTmpSet>(gvn.cfg->numBlocks);
    for (size_t i = 0; i < gvn.cfg->numBlocks; i++) {
        expGen[i].init();
        phiGen[i].init();
        tmpGen[i].init();
    }

    for (auto bb = rpo_begin(*gvn.cfg), end = rpo_end(*gvn.cfg); bb != end; ++bb) {
        ValuedExpSet &currExpGen = expGen[(*bb)->id];
        ValuedTmpSet &currPhiGen = phiGen[(*bb)->id];
        ValuedTmpSet &currTmpGen = tmpGen[(*bb)->id];

        AvailOutSet &domAvailOut = gvn.availOut[gvn.cfg->idom[(*bb)->id]->id];
        AvailOutSet &currAvailOut = gvn.availOut[(*bb)->id];
        currAvailOut.init(&domAvailOut);

        Inst *currInst = (*bb)->start;
        while (currInst) {
            Value val;
            switch (currInst->kind) {
                case Inst::Kind::kPhi:
                    val = gvn.valCnt++;
                    gvn.tmpValueMap.try_put(currInst, val);
                    currPhiGen.insert(val, currInst);
                    currAvailOut.val_insert(val, currInst);
                    break;
                case Inst::Kind::kArg:
                    val = gvn.valCnt++;
                    gvn.tmpValueMap.try_put(currInst, val);
                    currTmpGen.insert(val, currInst);
                    currAvailOut.val_insert(val, currInst);
                    break;
                case Inst::Kind::kAssign:
                    val = lookup_process_opd(gvn, currExpGen, currInst->assign.src);
                    gvn.tmpValueMap.try_put(currInst, val);
                    currTmpGen.insert(val, currInst);
                    currAvailOut.val_insert(val, currInst);
                    break;
                case Inst::Kind::kBin: {
                    Value v1 = lookup_process_opd(gvn, currExpGen, currInst->bin.left);
                    Value v2 = lookup_process_opd(gvn, currExpGen, currInst->bin.right);

                    Expression bin;
                    bin.kind = Expression::Kind::kBin;
                    bin.bin.op = currInst->bin.op;
                    if (is_commutative(currInst->bin.op) && v1 > v2) {
                        bin.bin.lval = v2;
                        bin.bin.rval = v1;
                    } else {
                        bin.bin.lval = v1;
                        bin.bin.rval = v2;
                    }

                    if (Value *valptr = gvn.expValueMap.try_put(bin, gvn.valCnt)) {
                        val = *valptr;
                    } else {
                        val = gvn.valCnt++;
                    }
                    if (!currExpGen.map.try_put(val, currExpGen.set.size)) {
                        currExpGen.set.add(bin);
                    }
                    gvn.tmpValueMap.try_put(currInst, val);
                    currTmpGen.insert(val, currInst);
                    currAvailOut.val_insert(val, currInst);
                    break;
                }
                case Inst::Kind::kCall: break;
            }

            currInst = currInst->next;
        }
    }

    for (size_t i = 0; i < gvn.cfg->numBlocks; i++) {
        mem::c_free(expGen[i].set.data);
        mem::c_free(expGen[i].map.table);
        mem::c_free(phiGen[i].set.data);
        mem::c_free(phiGen[i].map.table);
        mem::c_free(tmpGen[i].set.data);
        mem::c_free(tmpGen[i].map.table);
    }
    mem::c_free(expGen);
    mem::c_free(phiGen);
    mem::c_free(tmpGen);
}

void phi_translate(ValuedExpSet &currAnticIn, BasicBlock *pred, BasicBlock *succ) {
    (void)currAnticIn;
    (void)pred;
    (void)succ;
}

void buildset_antic(GVNPREContext &gvn) {
    bool changed = true;
    while (changed) {
        changed = false;

        for (auto bb = po_begin(*gvn.cfg), end = po_end(*gvn.cfg); bb != end; ++bb) {
            if (succ_count(*bb) == 0) {
            } else if (succ_count(*bb) == 1) {
            }
        }
    }
}

void eliminate(GVNPREContext &gvn) {
    for (size_t i = 0; i < gvn.cfg->numBlocks; i++) {
        BasicBlock *bb = gvn.cfg->map[i];

        Inst *currInst = bb->start;
        while (currInst) {
            if (currInst->kind == Inst::Kind::kBin) {
                Value *valptr = gvn.tmpValueMap[currInst];
                assert(valptr);
                Inst *leadTmp = gvn.availOut[i].find_leader(*valptr);
                if (leadTmp->dst != currInst->dst) {
                    currInst->kind = Inst::Kind::kAssign;
                    currInst->assign.src.kind = Opd::Kind::kReg;
                    currInst->assign.src.regVal = leadTmp;
                } else {
                    // TODO: verify if this extra optimization is necessary/correct
                    if (currInst->bin.left.kind == Opd::Kind::kReg) {
                        Value *lvalPtr = gvn.tmpValueMap[currInst->bin.left.regVal];
                        assert(lvalPtr);
                        currInst->bin.left.regVal = gvn.availOut[i].find_leader(*lvalPtr);
                    }
                    if (currInst->bin.right.kind == Opd::Kind::kReg) {
                        Value *rvalPtr = gvn.tmpValueMap[currInst->bin.right.regVal];
                        assert(rvalPtr);
                        currInst->bin.right.regVal = gvn.availOut[i].find_leader(*rvalPtr);
                    }
                }
            }
            currInst = currInst->next;
        }
    }
}

void pass(CFG &cfg) {
    GVNPREContext gvn = {};
    gvn.cfg = &cfg;
    gvn.valCnt = 1;
    gvn.immValueMap.init();
    gvn.tmpValueMap.init();
    gvn.expValueMap.init();

    gvn.availOut = mem::c_malloc<AvailOutSet>(cfg.numBlocks);
    buildset_avail(gvn);

    gvn.anticIn = mem::c_malloc<ValuedExpSet>(cfg.numBlocks);
    buildset_antic(gvn);

    eliminate(gvn);

    return;

    for (size_t i = 0; i < cfg.numBlocks; i++) {
        printf("b%d: ", i);
        for (size_t ii = 0; ii < gvn.availOut[i].tmpSet.set.size; ii++) {
            Inst *tmp = gvn.availOut[i].tmpSet.set[ii];
            printf("V%d: t%d, ", *gvn.tmpValueMap[tmp], tmp->dst);
        }
        printf("\n");
    }

    // TODO: delete. dumps all values and corresponding values
    LList<Expression> *valTab = mem::c_malloc<LList<Expression>>(gvn.valCnt);
    for (size_t i = 0; i < gvn.valCnt; i++) {
        valTab[i] = {};
    }
    for (size_t i = 0; i < gvn.tmpValueMap.capacity; i++) {
        if (gvn.tmpValueMap.table[i].psl) {
            Expression exp;
            exp.kind = Expression::Kind::kTmp;
            exp.tmp = gvn.tmpValueMap.table[i].key;
            valTab[gvn.tmpValueMap.table[i].val].add(exp);
        }
    }
    for (size_t i = 0; i < gvn.immValueMap.capacity; i++) {
        if (gvn.immValueMap.table[i].psl) {
            Expression exp;
            exp.kind = Expression::Kind::kImm;
            exp.imm = gvn.immValueMap.table[i].key;
            valTab[gvn.immValueMap.table[i].val].add(exp);
        }
    }
    for (size_t i = 0; i < gvn.expValueMap.capacity; i++) {
        if (gvn.expValueMap.table[i].psl) {
            valTab[gvn.expValueMap.table[i].val].add(gvn.expValueMap.table[i].key);
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

    mem::c_free(gvn.immValueMap.table);
    mem::c_free(gvn.tmpValueMap.table);
    mem::c_free(gvn.expValueMap.table);

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
    dce::pass(cfg);
    print_cfg(cfg);
}

}  // namespace lcc
