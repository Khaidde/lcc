#ifndef LCC_MEM_HPP
#define LCC_MEM_HPP

#include <malloc.h>

#include <cstdint>

#include "print.hpp"

namespace lcc::mem {

static inline size_t align(size_t bytes) { return (bytes + sizeof(intptr_t) - 1) & ~(sizeof(intptr_t) - 1); }

struct ArenaAllocator {
    void new_block() {
        cur = (Block *)::malloc(sizeof(Block));
        cur->offset = align(sizeof(size_t));
        debug("Allocating arena block(ptr=%p,size=%lld)\n", (char *)cur + cur->offset, kBlockSize);
    }

    template <typename T>
    T *malloc(size_t bytes) {
        bytes = align(bytes);
#ifndef NDEBUG
        if (!cur) {
            panic("Must create initial arena block before malloc\n");
        }
#endif
        if (cur->offset + bytes > kBlockSize) {
            new_block();
        }
        T *ptr = (T *)&cur->buffer[cur->offset];
        cur->offset += bytes;
        return ptr;
    }

    static constexpr size_t kBlockSize = 16384;  // 16KB chunk
    struct Block {
        size_t offset{sizeof(size_t)};
        unsigned char buffer[kBlockSize];
    };
    Block *cur;
};

extern ArenaAllocator allocator;

template <class T>
static inline T *arena_alloc() {
    return allocator.malloc<T>(sizeof(T));
}

template <class T>
static inline T *malloc(size_t bytes) {
    return (T *)::malloc(bytes);
}

template <class T>
static inline T *realloc(T *memory, size_t newSize) {
    return (T *)::realloc(memory, newSize * sizeof(T));
}

}  // namespace lcc::mem

#endif
