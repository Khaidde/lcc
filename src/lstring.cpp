#include "lstring.hpp"

namespace lcc {

template <>
void LString::add(char &ch) {
    ensure_capacity(size + 1);
    data[size - 1] = ch;
    data[size++] = '\0';
}

LString lstr_create(const char *str) {
    LString lStr;
    size_t cap = strlen(str) + 1;
    lStr.init(cap);
    strncpy(lStr.data, str, cap);
    lStr.size = cap;
    return lStr;
}

LString lstr_create(LStringView &strView) {
    LString lStr;
    lStr.init(strView.len + 1);
    strncpy(lStr.data, strView.src, strView.len);
    lStr.size = strView.len + 1;
    lStr.data[strView.len] = '\0';
    return lStr;
}

const char *lstr_raw_view(LString &src, size_t off, size_t len) {
    char *data = mem::c_malloc<char>(len + 1);
    strncpy(data, src.data + off, len);
    data[len] = '\0';
    return data;
}

size_t lstr_size(LString &dest) { return dest.size - 1; }

void lstr_cat(LString &dest, const char *src) {
    size_t len = strlen(src);
    dest.ensure_capacity(dest.size + len);
    strncat(dest.data, src, len);
    dest.size += len;
}

LStringView lstr_view(const char *str, size_t off, size_t len) { return {str + off, len}; }

}  // namespace lcc
