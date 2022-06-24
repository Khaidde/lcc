#ifndef LCC_UTIL_HPP
#define LCC_UTIL_HPP

#include <cassert>
#include <cstdint>

#if defined(__BYTE_ORDER__) && __BYTE_ORDER__ == __ORDER_BIG_ENDIAN__
static_assert(false && "Big endian compilation not supported");
#endif

namespace lcc {

template <typename T>
struct Optional {
    T value;
    bool hasValue;
};

template <typename T>
static Optional<T> make_none_optional(T &noneValue) {
    return {noneValue, true};
}

template <typename T>
static Optional<T> make_optional(T &value) {
    return {value, true};
}

template <typename T1, typename T2>
struct Pair {
    T1 first;
    T1 second;
};

template <typename T1, typename T2>
static Pair<T1, T2> make_pair(T1 first, T2 second) {
    return {first, second};
}

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

constexpr bool is_sorted(size_t ndx, size_t numKeywords, const char *(get_str)(size_t)) {
    if (ndx >= numKeywords - 1) return true;
    return comptime_strcmp(get_str(ndx), get_str(ndx + 1)) <= 0 && is_sorted(ndx + 1, numKeywords, get_str);
}

#define ASSERT_ALPHABETIC(numStrings, getStrFromNdx)                                  \
    static_assert(is_sorted(0, numStrings, [](size_t ndx) { return getStrFromNdx; }), \
                  "Strings in list are not alphabetic");

}  // namespace lcc

#endif
