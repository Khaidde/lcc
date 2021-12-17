#ifndef LCC_MEM_HPP
#define LCC_MEM_HPP

#include "util.hpp"

namespace lcc::mem {

struct ArenaAllocator {
    static constexpr size_t kBlockDataSize = 16384;  // 16KB chunk
    struct Block {
        struct {
            intptr_t prevPtr;
            size_t offset;
            Block *next;
        } header;
        unsigned char buffer[kBlockDataSize];
    };
    Block *head;

    Block *create_block();

    void *malloc(size_t bytes);
    void *realloc(void *ptr, size_t prevSize, size_t newSize);
};

extern ArenaAllocator allocator;

template <class T>
static inline T *malloc() {
    return (T *)allocator.malloc(sizeof(T));
}

template <class T>
static inline T *malloc(size_t bytes) {
    return (T *)allocator.malloc(bytes);
}

template <class T>
static inline T *realloc(void *ptr, size_t prevSize, size_t newSize) {
    return (T *)allocator.realloc(ptr, prevSize, newSize);
}

}  // namespace lcc::mem

#endif
