
#ifndef LCC_LSTRING_HPP
#define LCC_LSTRING_HPP

#include <cstring>

#include "list.hpp"

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

u32 lstr_hash(LStringView &str);

bool lstr_equal(LStringView &str1, LStringView &str2);

}  // namespace lcc

#endif
