#ifndef LCC_TRANSLATE_HPP
#define LCC_TRANSLATE_HPP

#include "compilation.hpp"
#include "list.hpp"
#include "token.hpp"

namespace lcc {

struct BasicBlock;

using BlockId = size_t;
using ValId = size_t;

enum class InstKind {
    kPhi,
    kConst,
    kUsage,
    kBin,
    kCall,
};

struct PhiInst {
    size_t varId;
    ValId dest;
    LList<ValId> joins;
};

struct ConstInst {
    ValId dest;
    uint16_t intVal;
};

struct UsageInst {
    ValId dest;
    ValId useId;
};

struct BinInst {
    ValId dest;
    TokenType op;
    ValId left;
    ValId right;
};

struct CallInst {
    ValId dest;
    // TODO: should also contain name of call
    size_t cnt;
    ValId *args;
};

struct Inst {
    InstKind kind;
    Inst *next;
    union {
        PhiInst phi;
        ConstInst aconst;
        UsageInst usage;
        BinInst bin;
        CallInst call;
    };
};

enum class TerminatorKind {
    kGoto,
    kCond,
    kRet,
};

struct GotoTerminator {
    BasicBlock *target;
};

struct CondTerminator {
    ValId predicate;
    BasicBlock *then;
    BasicBlock *alt;
};

struct Terminator {
    TerminatorKind kind;
    union {
        GotoTerminator tgoto;
        CondTerminator cond;
    };
};

struct BasicBlock {
    BlockId id;
    Inst *start;
    Inst *end;
    Terminator *terminator;
};

struct IrContext {
    // TODO: store a list of global constants
    LList<BasicBlock *> basicBlocks{};
};

void translate_package(Package *package);

/*
add = :(a: u16, b: u16) -> string {
  c := a + b
  if c > 2 {
    ret "g2"
  } else {
    if (c < 3) {
       ret "l2"
    }
    c = 4
  }
  nop()
  ret "none"
}

===>

constants [
  0 = "g2"
  1 = "l2"
  2 = "none"
]

func0 {
  debug a = v0
  debug b = v1
  // Return value stored in v2
  debug c = v3

  b0 {
    v0 = recv 2 // Receive 2 bytes for argument a
    v1 = recv 2
    v3 = v0 + v1 // c := a + b
    v4 = v3 > 2
    if v4 : b1 else b2
  }
  b1 {
    v2 = constants[0]
    br b5
  }
  b2 {
    v5 = 3
    v6 = v3 < 3
    if v6 : b3 else b4
  }
  b3 {
    v2 = constants[1]
    br b6
  }
  b4 {
    v3 = 4
    // Implicit br b5
  }
  b5 {
    v7 = nop()
    v2 = constants[2]
    // Implicit br b6
  }
  b6 {
    ret
  }
}

 */

}  // namespace lcc

#endif
