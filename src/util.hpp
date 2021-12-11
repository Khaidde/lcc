#ifndef LCC_UTIL_HPP
#define LCC_UTIL_HPP

#include <cstdint>

namespace lcc {

using u64 = uint64_t;
using u32 = uint32_t;
using u16 = uint16_t;
using u8 = uint8_t;

namespace file {

static inline char *read_file(const char *path) {
    (void)path;
    return nullptr;
}

};  // namespace file

}  // namespace lcc

#endif
