#ifndef LCC_MEM_HPP
#define LCC_MEM_HPP

#include <malloc.h>

#include <cstdint>

namespace lcc::mem {

struct ArenaAllocator {
    static constexpr size_t kBlockDataSize = 16384;  // 16KB chunk
    struct Block {
        struct {
            intptr_t prevPtr;
            size_t offset;
            Block *next;
        } header;
        char buffer[kBlockDataSize];
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
static inline T *malloc(size_t count) {
    return (T *)allocator.malloc(sizeof(T) * count);
}

template <class T>
static inline T *realloc(void *ptr, size_t prevCount, size_t newCount) {
    return (T *)allocator.realloc(ptr, sizeof(T) * prevCount, sizeof(T) * newCount);
}

template <class T>
static inline T *c_malloc() {
    return (T *)::malloc(sizeof(T));
}

template <class T>
static inline T *c_malloc(size_t count) {
    return (T *)::malloc(sizeof(T) * count);
}

template <class T>
static inline T *c_realloc(void *ptr, size_t newCount) {
    return (T *)::realloc(ptr, sizeof(T) * newCount);
}

static inline void c_free(void *ptr) { ::free(ptr); }

}  // namespace lcc::mem

#endif
