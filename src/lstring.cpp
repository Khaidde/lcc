#include "lstring.hpp"

namespace lcc {

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
    lStr.get(strView.len) = '\0';
    return lStr;
}

const char *lstr_raw_str(LStringView &strView) { return lstr_create(strView).data; }

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

void lstr_cat(LString &dest, LStringView &view) {
    dest.ensure_capacity(dest.size + view.len);
    memcpy(&dest.data[dest.size - 1], view.src, view.len);
    dest.size += view.len;
    dest.data[dest.size - 1] = '\0';
}

LStringView lstr_view(const char *str, size_t off, size_t len) { return {str + off, len}; }

u32 lstr_hash(LStringView &str) {
    u32 hash = 0;
    for (size_t i = 0; i < str.len; i++) {
        hash = ((hash << 5) - hash) + (u32)str.src[i];
    }
    return hash;
}

bool lstr_equal(LStringView &str1, LStringView &str2) {
    if (str1.len != str2.len) return false;
    for (size_t i = 0; i < str1.len; i++) {
        if (str1.src[i] != str2.src[i]) return false;
    }
    return true;
}

}  // namespace lcc
