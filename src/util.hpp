#ifndef LCC_UTIL_HPP
#define LCC_UTIL_HPP

#include <cassert>
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

constexpr int comp_strcmp(char const *a, char const *b) {
    return (*a != *b || *a == '\0') ? *a - *b : comp_strcmp(a + 1, b + 1);
}

constexpr bool is_sorted(size_t ndx, size_t numKeywords, const char *(get_str)(size_t)) {
    if (ndx >= numKeywords - 1) return true;
    return comp_strcmp(get_str(ndx), get_str(ndx + 1)) <= 0 && is_sorted(ndx + 1, numKeywords, get_str);
}

#define ASSERT_ALPHABETIC(numStrings, getStrFromNdx)                                  \
    static_assert(is_sorted(0, numStrings, [](size_t ndx) { return getStrFromNdx; }), \
                  "Strings in list are not alphabetic");

}  // namespace lcc

#endif
