#ifndef LCC_IR_HPP
#define LCC_IR_HPP

#include "compilation.hpp"
#include "list.hpp"
#include "token.hpp"

namespace lcc {

struct BasicBlock;

using BlockId = size_t;
using VReg = size_t;
using VarId = size_t;

struct Inst;

struct VarInfo {
    VarId varid;
    LList<BlockId> defSites;
    LList<Inst *> defStack;
};

struct Opd {
    enum Kind {
        kReg,
        kImm,
    } kind;
    union {
        Inst *regVal;
        uint16_t intval;
    };
};

struct PhiInst {
    VarInfo *varInfo;

    struct Arg {
        Inst *ref;
        BasicBlock *bb;
    };
    LList<Arg> joins;
};

struct ArgInst {
    size_t argNo;
};

struct AssignInst {
    Opd src;
};

struct BinInst {
    TokenType op;
    Opd left;
    Opd right;
};

struct CallInst {
    // TODO: used right now primarily to mark persistent values
    // Used to disable certain optimizations form going haywire (DCE)
    LList<Opd> args;
};

struct Inst {
    enum Kind {
        kPhi,
        kArg,
        kAssign,
        kBin,
        kCall,
    } kind;

    Inst *prev;
    Inst *next;
    VReg dst;
    union {
        PhiInst phi;
        ArgInst arg;
        AssignInst assign;
        BinInst bin;
        CallInst call;
    };
};

struct GotoTerminator {
    BasicBlock *target;
};

struct CondTerminator {
    BasicBlock *then;
    BasicBlock *alt;
    Opd predicate;  // Only relevant for conditional terminator
};

struct Terminator {
    enum Kind {
        kGoto,
        kCond,
        kRet,
    } kind;

    union {
        GotoTerminator tgoto;
        CondTerminator cond;
        BasicBlock *succ[2];
    };
};

struct BasicBlock {
    BlockId id;
    LList<BasicBlock *> pred;
    Inst *start;
    Inst *end;
    Terminator term;
};

struct CFG {
    size_t numBlocks{0};
    BasicBlock *entry;
    BasicBlock *exit;

    BasicBlock **po;
    BasicBlock **rpo;

    BasicBlock **idom;
};

ListIterator<BasicBlock *> po_begin(CFG &cfg);
ListIterator<BasicBlock *> po_end(CFG &cfg);

ListIterator<BasicBlock *> rpo_begin(CFG &cfg);
ListIterator<BasicBlock *> rpo_end(CFG &cfg);

ListIterator<BasicBlock *> pred_begin(BasicBlock *block);
ListIterator<BasicBlock *> pred_end(BasicBlock *block);

size_t succ_count(BasicBlock *block);
ListIterator<BasicBlock *> succ_begin(BasicBlock *block);
ListIterator<BasicBlock *> succ_end(BasicBlock *block);

void remove_inst(BasicBlock *block, Inst *inst);

void print_inst(Inst *inst);

void print_cfg(CFG &cfg);

void translate_function(CFG &cfg, Node *function);

}  // namespace lcc

#endif
