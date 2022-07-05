#ifndef LCC_MEM_HPP
#define LCC_MEM_HPP

#include <malloc.h>

#include <cassert>
#include <cstdint>
#include <cstring>

#if !defined(NDEBUG)

#include <typeinfo>

#include "print.hpp"
#include "util.hpp"

#define DBG_MEM 0

#else
#define DBG_MEM 0
#endif

namespace lcc {

namespace mem {

static constexpr size_t align(size_t bytes) { return (bytes + sizeof(intptr_t) - 1) & ~(sizeof(intptr_t) - 1); }

static constexpr size_t kDefaultArenaSize = 16384;

template <size_t S = kDefaultArenaSize>
struct ArenaAllocator {
    static constexpr size_t kBlockDataSize = S;

    struct Block {
        struct {
            intptr_t prevPtr;
            size_t offset;
            Block *next;
        } header;
        char buffer[kBlockDataSize];
    };
    Block *head{nullptr};

    struct DefaultAllocBlock {
        DefaultAllocBlock *next;
    };
    DefaultAllocBlock *headDefaultAlloc{nullptr};

    Block *create_block() {
        Block *newBlock = (Block *)::malloc(sizeof(Block));
        newBlock->header.prevPtr = 0;
        newBlock->header.offset = 0;
        newBlock->header.next = nullptr;
#if DBG_MEM
        debug("Allocating arena block(ptr=%p,size=%lld+%lld)\n", newBlock, sizeof(Block::header), kBlockDataSize);
#endif
        if (head) {
            Block *curr = head;
            while (curr && curr->header.next) {
                curr = curr->header.next;
            }
            curr->header.next = newBlock;
        } else {
            head = newBlock;
        }
        return newBlock;
    }

    template <typename T>
    T *alloc() {
        T *ptr = (T *)internal_alloc(sizeof(T));
        *ptr = {};
        return ptr;
    }

    template <typename T>
    T *alloc(size_t count) {
        T *ptr = (T *)internal_alloc(sizeof(T) * count);
        *ptr = {};
        return ptr;
    }

    template <typename T>
    T *realloc(void *, size_t) {
        assert(!"Cannot realloc with ArenaAllocator");
        return nullptr;
    }

    void free(void *) {}

    ArenaAllocator() {
        head = create_block();
        headDefaultAlloc = nullptr;
    }

    ~ArenaAllocator() {
        Block *curr = head;
        while (curr) {
            Block *next = curr->header.next;
#if DBG_MEM
            debug("Deallocating arena block(ptr=%p,size=%lld+%lld)\n", curr, sizeof(Block::header), kBlockDataSize);
#endif
            ::free(curr);
            curr = next;
        }
        DefaultAllocBlock *allocCurr = headDefaultAlloc;
        while (allocCurr) {
            DefaultAllocBlock *next = allocCurr->next;
#if DBG_MEM
            debug("Deallocating extra alloc(ptr=%p)\n", allocCurr);
#endif
            ::free(allocCurr);
            allocCurr = next;
        }
    }

private:
    void *internal_alloc(size_t bytes) {
        bytes = align(bytes);

        Block *bestFit = nullptr;
        Block *curr = head;
        while (curr) {
            if (curr->header.offset + bytes == kBlockDataSize) {
                bestFit = curr;
                break;
            }
            if (curr->header.offset + bytes < kBlockDataSize && !bestFit) {
                bestFit = curr;
            }
            curr = curr->header.next;
        }
        if (!bestFit) {
            if (bytes > kBlockDataSize) {
                size_t allocBlockSize = align(sizeof(DefaultAllocBlock));
                DefaultAllocBlock *ptr = (DefaultAllocBlock *)::malloc(allocBlockSize + bytes);
                ptr->next = headDefaultAlloc;
                headDefaultAlloc = ptr;
#if DBG_MEM
                err("Warning: allocating more bytes than arena block size so resolving to default malloc\n");
                debug("Default malloc %lld+%lld bytes\n", allocBlockSize, bytes);
#endif
                return (void *)((uintptr_t)ptr + allocBlockSize);
            }
            bestFit = create_block();
        }
#if DBG_MEM
        debug("Malloc offset=%lld: %lld bytes\n", bestFit->header.offset, bytes);
#endif

        void *ptr = &bestFit->buffer[bestFit->header.offset];
        bestFit->header.prevPtr = (intptr_t)ptr;
        bestFit->header.offset += bytes;
        return ptr;
    }
};

extern ArenaAllocator<kDefaultArenaSize> gbAlloc;

template <typename T>
struct PoolAllocator {
    struct Chunk {
        size_t metadata;
        Chunk *next;
    };
#if DBG
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
        *(T *)chunk = {};
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

struct CAllocator {
    template <typename T>
    T *alloc() {
        T *ptr = (T *)::malloc(sizeof(T));
        *ptr = {};
        return ptr;
    }

    template <typename T>
    T *alloc(size_t count) {
        T *ptr = (T *)::malloc(sizeof(T) * count);
        *ptr = {};
        return ptr;
    }

    template <typename T>
    T *realloc(T *ptr, size_t newCount) {
        return (T *)::realloc(ptr, sizeof(T) * newCount);
    }

    void free(void *ptr) { ::free(ptr); }
};

extern CAllocator cAlloc;

template <typename T, typename A>
static inline T *alloc(A &alloc) {
    return alloc.template alloc<T>();
}

template <typename T, typename A>
static inline T *alloc(A &alloc, size_t count) {
    return alloc.template alloc<T>(sizeof(T) * count);
}

template <typename T>
static inline T *c_alloc() {
    return cAlloc.template alloc<T>();
}

template <typename T>
static inline T *c_alloc(size_t count) {
    return cAlloc.template alloc<T>(count);
}

template <typename T>
static inline T *c_realloc(T *ptr, size_t newCount) {
    return cAlloc.template realloc<T>(ptr, newCount);
}

static inline void c_free(void *ptr) { cAlloc.free(ptr); }

template <typename T>
static inline T *gb_alloc() {
    return (T *)gbAlloc.alloc<T>();
}
template <typename T>
static inline T *gb_alloc(size_t count) {
    return (T *)gbAlloc.alloc<T>(count);
}

template <typename T>
static inline T *p_alloc() {
    return PoolAllocator<T>::pallocator.allocate();
}

template <typename T>
static inline void p_free(T *ptr) {
    PoolAllocator<T>::pallocator.deallocate(ptr);
}

}  // namespace mem

}  // namespace lcc

#endif
