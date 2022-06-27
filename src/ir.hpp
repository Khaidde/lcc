#ifndef LCC_IR_HPP
#define LCC_IR_HPP

#include "compilation.hpp"
#include "list.hpp"
#include "token.hpp"

namespace lcc {

struct BasicBlock;

using CFGId = size_t;
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

struct CFG;

struct CallInst {
    // TODO: used right now primarily to mark persistent values
    // Used to disable certain optimizations form going haywire (DCE)
    CFG *cfg;
    LArray<Opd> args;
};

struct Inst {
    enum Kind {
        kPhi,
        kArg,
        kAssign,
        kBin,
        kCall,
    } kind;

#ifndef NDEBUG
    const char *annotation;
#endif
    BasicBlock *block;
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

struct RetTerminator {
    struct Arg {
        Opd opd;
        BasicBlock *bb;
    };
    LList<Arg> args;
};

struct Terminator {
    enum Kind {
        kGoto,
        kCond,
        kRet,
    } kind;

#ifndef NDEBUG
    const char *annotation;
#endif
    union {
        GotoTerminator tgoto;
        CondTerminator cond;
        RetTerminator ret;
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

struct IR;

struct CFG {
    IR *globalIR;
    LStringView ident;

    VReg nextReg{1};

    size_t numBlocks{0};
    BasicBlock *entry;
    BasicBlock *exit;

    BasicBlock **po;
    BasicBlock **rpo;

    BasicBlock **idom;
};

struct IR {
    LMap<LStringView, CFG, lstr_hash, lstr_equal> funcMap;
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

Inst *create_inst(Inst::Kind kind);
void add_start_inst(BasicBlock *block, Inst *inst);
void add_end_inst(BasicBlock *block, Inst *inst);
void free_inst(Inst *inst);

void print_inst(Inst *inst);

void start_pass(const char *annotation);
void annotate_inst(Inst *inst);
void annotate_term(Terminator &term);

void print_cfg(CFG &cfg);

CFG &generate_cfg(IR &globalIR, Node *functionDecl);

}  // namespace lcc

#endif
