#include "mem.hpp"

#include <malloc.h>

#include <cassert>
#include <cstring>

#include "print.hpp"

namespace lcc::mem {

namespace {

size_t align(size_t bytes) { return (bytes + sizeof(intptr_t) - 1) & ~(sizeof(intptr_t) - 1); }

}  // namespace

ArenaAllocator allocator;

ArenaAllocator::Block *ArenaAllocator::create_block() {
    Block *newBlock = (Block *)::malloc(sizeof(Block));
    newBlock->header.prevPtr = 0;
    newBlock->header.offset = 0;
    newBlock->header.next = nullptr;
    debug("Allocating arena block(ptr=%p,size=%lld+%lld)\n", newBlock, sizeof(Block::header), kBlockDataSize);
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

void *ArenaAllocator::malloc(size_t bytes) {
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
        assert(bytes <= kBlockDataSize && "Cannot allocate more bytes than arena block size");
        bestFit = create_block();
    }
    debug("Malloc offset=%lld: %lld bytes\n", bestFit->header.offset, bytes);

    void *ptr = &bestFit->buffer[bestFit->header.offset];
    bestFit->header.prevPtr = (intptr_t)ptr;
    bestFit->header.offset += bytes;
    return ptr;
}

void *ArenaAllocator::realloc(void *ptr, size_t prevSize, size_t newSize) {
    if (!ptr) {
        return malloc(newSize);
    }
    assert(newSize > 0 && "New size must be a positive");
    size_t alignedNewSize = align(newSize);
    size_t alignedPrevSize = align(prevSize);
    if (alignedNewSize == alignedPrevSize) {
        return ptr;
    }

    Block *bestFit = nullptr;
    Block *curr = head;
    while (curr && curr < ptr) {
        if (curr->header.prevPtr == (intptr_t)ptr) {
            if (curr->header.offset + (alignedNewSize - alignedPrevSize) <= kBlockDataSize) {
                curr->header.offset += (alignedNewSize - alignedPrevSize);
                debug("Fast realloc %lld bytes -> %lld bytes\n", prevSize, newSize);
                return ptr;
            }
        }
        if (curr->header.offset + alignedNewSize == kBlockDataSize ||
            (curr->header.offset + alignedNewSize < kBlockDataSize && !bestFit)) {
            bestFit = curr;
        }
        curr = curr->header.next;
    }
    if (!bestFit) {
        assert(alignedNewSize <= kBlockDataSize && "Cannot reallocate more bytes than arena block size");
        bestFit = create_block();
    }
    debug("Slow realloc %lld bytes -> %lld bytes\n", prevSize, newSize);

    void *oldPtr = ptr;
    ptr = &bestFit->buffer[bestFit->header.offset];
    std::memcpy(ptr, oldPtr, prevSize);

    bestFit->header.prevPtr = (intptr_t)ptr;
    bestFit->header.offset += newSize;
    return ptr;
}

}  // namespace lcc::mem
