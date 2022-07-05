#ifndef LCC_GEN_HPP
#define LCC_GEN_HPP

#include "ast.hpp"

#if DBG
#define DBG_OPT 1
#else
#define DBG_OPT 0
#endif

namespace lcc {

using BlockIdx = size_t;
using VReg = size_t;
using VarId = size_t;

struct Inst;

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

struct AllocInst {
#if DBG_OPT
    LStringView name;
#endif
    enum Kind {
        kRetVal,
        kArg,
        kLocalVar,
    } kind;
    size_t allocSize;
    uint8_t argNo;  // Only used when alloc type is argument
};

struct PhiInst {
    struct Arg {
        Inst *ref;
        struct BasicBlock *bb;
    };
    LList<Arg> joins;
};

struct AssignInst {
    Opd src;
};

struct BinInst {
    Opd left;
    Opd right;
    TokenType op;
};

struct CallInst {
    Opd *args;
    size_t numArgs;
    struct Function *fn;
};

struct LoadInst {
    Opd src;
};

struct StoreInst {
    Opd addr;
    Opd src;
};

struct Inst {
    enum Kind {
        kAlloc,
        kPhi,
        kAssign,
        kBin,
        kCall,
        kLoad,
        kStore,
    } kind;

#if DBG_OPT
    const char *annotation;
#endif
    BasicBlock *block;
    Inst *prev;
    Inst *next;

    enum Type {
        kUnk,
        kU16,
        kPtr,
    } type;
    VReg dst;
    union {
        AllocInst alloc;
        PhiInst phi;
        AssignInst assign;
        BinInst bin;
        CallInst call;
        LoadInst ld;
        StoreInst st;
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

#if DBG_OPT
    const char *annotation;
#endif
    union {
        GotoTerminator tgoto;
        CondTerminator cond;
        BasicBlock *succ[2];
    };
};

struct BasicBlock {
    BlockIdx idx;  // Block index in program order (typically rpo)

    BasicBlock **pred;
    size_t predCnt;

    Inst *start;
    Inst *end;
    Terminator term;
};

struct Function {
    LStringView ident;

    BasicBlock *entry;
    BasicBlock *exit;
    size_t numBlocks;

    BasicBlock **rpo;
    BasicBlock **po;

    BasicBlock **dom;
    BasicBlock **pdom;
    Bitset *domf;  // dominance frontier sets
};

struct Opt {
#if DBG_OPT
    const char *currentPassName;
#endif
    LMap<LStringView, Function> fnNameMap;

    VReg nextReg{1};
};

ListIterator<BasicBlock *> rpo_begin(Function &fn);
ListIterator<BasicBlock *> rpo_end(Function &fn);

ListIterator<BasicBlock *> pred_begin(BasicBlock *block);
ListIterator<BasicBlock *> pred_end(BasicBlock *block);

size_t succ_count(BasicBlock *block);
ListIterator<BasicBlock *> succ_begin(BasicBlock *block);
ListIterator<BasicBlock *> succ_end(BasicBlock *block);

void print_opd(Opd &opd);
void print_inst(Inst *inst);
void print_function(Function &fn);

void start_pass(Opt &opt, const char *passName);

Inst *create_inst(Inst::Kind kind);
void push_front_inst(Opt &opt, BasicBlock *block, Inst *inst, bool genNewReg);
void push_back_inst(Opt &opt, BasicBlock *block, Inst *inst, bool getNewReg);
void insert_inst(Opt &opt, BasicBlock *block, Inst *afterThis, Inst *inst, bool genNewReg);
void remove_inst(Inst *inst);

Function &gen_function(Opt &opt, Node *funcDecl);

};  // namespace lcc

#endif
