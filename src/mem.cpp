#include "mem.hpp"

#include <malloc.h>

#include <cassert>
#include <cstring>

#include "print.hpp"

namespace lcc::mem {

ArenaAllocator<kDefaultArenaSize> gbAlloc;

CAllocator cAlloc;

}  // namespace lcc::mem
