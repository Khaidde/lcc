#ifndef LCC_LSTRING_HPP
#define LCC_LSTRING_HPP

#include <cstdint>
#include <cstring>

#include "list.hpp"
#include "map.hpp"

namespace lcc {

using LString = LList<char>;

struct LStringView {
    const char *src;
    size_t len;
};

LString lstr_create(const char *str);

LString lstr_create(LStringView &strView);

const char *lstr_raw_str(LStringView &strView);

const char *lstr_raw_view(LString &src, size_t off, size_t len);

size_t lstr_size(LString &dest);

void lstr_cat(LString &dest, const char *src);

void lstr_cat(LString &dest, LStringView &view);

LStringView lstr_view(const char *str, size_t off, size_t len);

void lstr_print(LStringView &strView);

bool lstr_equal(LStringView &str1, LStringView &str2);

template <>
struct HashFn<LStringView> {
    HashValue operator()(LStringView &str) {
        uint32_t hash = 1;
        for (size_t i = 0; i < str.len; i++) {
            hash = ((hash << 5) - hash) + (HashValue)str.src[i];
        }
        return hash;
    }
};

template <>
struct EqualFn<LStringView> {
    bool operator()(LStringView &str1, LStringView &str2) { return lstr_equal(str1, str2); }
};

}  // namespace lcc

#endif
