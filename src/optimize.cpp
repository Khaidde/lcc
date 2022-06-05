#include "optimize.hpp"

#include "map.hpp"

namespace lcc {

namespace gvnpre {

using Value = size_t;
Value kNone{0};

struct Expression {
    enum {
        kAssign,
        kAdd,
    } kind;

    struct BinExpr {
        Value lval;
        Value rval;
    };
    union {
        Value op[3];
        BinExpr bin;
    };
};

uint32_t exp_hash(Expression &exp) {
    size_t hash = exp.kind;
    hash = ((hash << 5) - hash) + exp.op[0];
    if (exp.kind != Expression::kAssign) {
        hash = ((hash << 5) - hash) + exp.bin.rval;
    }
    return hash;
}

bool exp_equal(Expression &e1, Expression &e2) {
    if (e1.kind != e2.kind) return false;
    if (e1.op[0] != e2.op[0]) return false;
    if (e1.kind == Expression::kAssign) return true;
    return e1.bin.rval == e2.bin.rval;
}

struct ValueTable {
    void init() {
        valCnt = 1;
        instValue.init();
        expValue.init();
    }
    void add(Inst *inst, Value val) {
        // TODO a lot of internal map operations can be optimized out
        // by creating a single "put" operation in LMap
        if (instValue[inst]) {
            instValue.remove(inst);
        }
        instValue.try_put(inst, val);
    }
    Value lookup(Inst *inst) {
        if (Value *val = instValue[inst]) return *val;
        return kNone;
    }
    Value lookup_or_add(Inst *inst) {
        Expression exp;
        /*
            switch (inst->kind) {
                case InstKind::kAssign: {
                    exp.kind = Expression::kAssign;
                    exp.op[0] = inst->assign.assignId;
                    break;
                }
                case InstKind::kBin: {
                    switch (inst->bin.op) {
                        case TokenType::kAdd: exp.kind = Expression::kAdd; break;
                        default: assert(false && "TODO unimplemented binary instruction to expression conversion");
                    }
                    exp.bin.lval = inst->bin.left;
                    exp.bin.rval = inst->bin.right;
                    break;
                }
                default: break; assert(false && "TODO unimplemented inst to expr conversion");
            }
        */
        (void)inst;

        if (Value *val = expValue.try_put(exp, valCnt)) {
            return *val;
        } else {
            return valCnt++;
        }
    }

    size_t valCnt{1};
    LMap<Inst *, Value, ptr_hash<Inst>, ptr_equal> instValue;
    LMap<Expression, Value, exp_hash, exp_equal> expValue;
};

void pass(CFG &cfg) {
    ValueTable table;
    table.init();

    (void)cfg;
    /*
    printf("asdf\n");
    Inst *inst = cfg.entry->start;
    while (inst) {
        printf("%d:: \n", table.lookup_or_add(inst));
        print_inst(inst);
        inst = inst->next;
    }
    */
}

}  // namespace gvnpre

uint32_t hash_func(size_t &key) { return key; }

bool equal_func(size_t &k1, size_t &k2) { return k1 == k2; }

void optimize(CFG &cfg) { gvnpre::pass(cfg); }

}  // namespace lcc
