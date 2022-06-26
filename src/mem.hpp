#ifndef LCC_MEM_HPP
#define LCC_MEM_HPP

#include <malloc.h>

#include <cstdint>

#ifndef NDEBUG

#include <typeinfo>

#include "print.hpp"
#define DBG_MEM 0

#else
#define DBG_MEM 0
#endif

namespace lcc {
namespace mem {

static constexpr size_t align(size_t bytes) { return (bytes + sizeof(intptr_t) - 1) & ~(sizeof(intptr_t) - 1); }

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

template <typename T>
struct PoolAllocator {
    struct Chunk {
        size_t metadata;
        Chunk *next;
    };
#ifndef NDEBUG
    const char *get_type_name() { return typeid(T).name(); }
#endif
    T *allocate() {
        if (!head) {
            head = (Chunk *)::malloc(kChunksPerBlock * kChunkSize);

            Chunk *curr = head;
            for (size_t i = 0; i < kChunksPerBlock - 1; i++) {
                curr->next = (Chunk *)((uint8_t *)curr + kChunkSize);
                curr = curr->next;
            }
            curr->next = nullptr;
        }
#if DBG_MEM
        debug("Palloc %s(n=%d)\n", get_type_name(), ++numObjects);
#endif
        Chunk *chunk = head;
        head = head->next;
        return (T *)chunk;
    }

    void deallocate(T *ptr) {
#if DBG_MEM
        debug("Pfree %s(n=%d)\n", get_type_name(), --numObjects);
        ((Chunk *)ptr)->metadata = 0xFEEEFEEE;
#endif
        ((Chunk *)ptr)->next = head;
        head = (Chunk *)ptr;
    }

    static inline PoolAllocator<T> pallocator{};
    static constexpr size_t kChunksPerBlock{8};
    static constexpr size_t kChunkSize{align(sizeof(T))};
    Chunk *head{nullptr};
#if DBG_MEM
    size_t numObjects{0};
#endif
};

template <typename T>
static inline T *p_malloc() {
    return PoolAllocator<T>::pallocator.allocate();
}

template <typename T>
static inline void p_free(T *ptr) {
    PoolAllocator<T>::pallocator.deallocate(ptr);
}

template <typename T>
static inline T *malloc() {
    return (T *)allocator.malloc(sizeof(T));
}

template <typename T>
static inline T *malloc(size_t count) {
    return (T *)allocator.malloc(sizeof(T) * count);
}

template <typename T>
static inline T *realloc(void *ptr, size_t prevCount, size_t newCount) {
    return (T *)allocator.realloc(ptr, sizeof(T) * prevCount, sizeof(T) * newCount);
}

template <typename T>
static inline T *c_malloc() {
    return (T *)::malloc(sizeof(T));
}

template <typename T>
static inline T *c_malloc(size_t count) {
    return (T *)::malloc(sizeof(T) * count);
}

template <typename T>
static inline T *c_realloc(void *ptr, size_t newCount) {
    return (T *)::realloc(ptr, sizeof(T) * newCount);
}

static inline void c_free(void *ptr) { ::free(ptr); }

}  // namespace mem
}  // namespace lcc

#endif
