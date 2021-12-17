#ifndef LCC_UTIL_HPP
#define LCC_UTIL_HPP

#include <cstdint>

#if defined(__BYTE_ORDER__) && __BYTE_ORDER__ == __ORDER_BIG_ENDIAN__
static_assert(0 && "Big endian compilation not supported");
#endif

namespace lcc {

using u64 = uint64_t;
using u32 = uint32_t;
using u16 = uint16_t;
using u8 = uint8_t;

using s32 = int32_t;

}  // namespace lcc

#endif
