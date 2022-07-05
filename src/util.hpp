#ifndef LCC_UTIL_HPP
#define LCC_UTIL_HPP

#include <cassert>
#include <cstdint>

static_assert(sizeof(unsigned long long) == 8);

#if defined(__BYTE_ORDER__) && __BYTE_ORDER__ == __ORDER_BIG_ENDIAN__
#error Big endian compilation not supported
#endif

#if !defined(_WIN32) || !defined(__MINGW32__)
#error Compilation only supported on windows with posix libraries
#endif

#if !defined(__clang__)
#error Only clang compilation supported
#endif

#if defined(NDEBUG)
#define DBG 0
#else
#define DBG 1
#endif

#define MIN(x, y) ((x) < (y) ? (x) : (y))
#define MAX(x, y) ((x) > (y) ? (x) : (y))

namespace lcc {

int clzll(unsigned long long num);

int ctzll(unsigned long long num);

int lsb(uint64_t num);

int msb(uint64_t num);

template <size_t N>
struct log2 {
    static constexpr size_t res = 1 + log2<N / 2>::res;
};

template <>
struct log2<1> {
    static constexpr size_t res = 0;
};

template <>
struct log2<0> {};

constexpr int comptime_strcmp(char const *a, char const *b) {
    return (*a != *b || *a == '\0') ? *a - *b : comptime_strcmp(a + 1, b + 1);
}

constexpr size_t comptime_strlen(char const *str) { return (*str == '\0') ? 0 : comptime_strlen(str + 1) + 1; }

constexpr bool is_sorted(size_t ndx, size_t numKeywords, const char *(get_str)(size_t)) {
    if (ndx >= numKeywords - 1) return true;
    return comptime_strcmp(get_str(ndx), get_str(ndx + 1)) <= 0 && is_sorted(ndx + 1, numKeywords, get_str);
}

#define ASSERT_ALPHABETIC(numStrings, getStrFromNdx)                                  \
    static_assert(is_sorted(0, numStrings, [](size_t ndx) { return getStrFromNdx; }), \
                  "Strings in list are not alphabetic");

}  // namespace lcc

#endif
