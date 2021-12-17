#ifndef LCC_LIST_HPP
#define LCC_LIST_HPP

#include <cassert>

#include "mem.hpp"

namespace lcc {

template <typename T>
struct LList {
    void init(size_t initialSize) {
        assert(initialSize && "Initial size of list must be positive");
        data = mem::malloc<T>(sizeof(T) * initialSize);
        size = 0;
        capacity = initialSize;
    }

    void init(T *items, size_t initialSize) {
        init(initialSize);
        for (size_t i = 0; i < initialSize; i++) {
            data[i] = items[i];
        }
    }

    void resize() {
        size_t oldCapacity = capacity;
        capacity = (capacity << 1) - (capacity >> 1) + 8;
        data = mem::realloc<T>(data, oldCapacity * sizeof(T), capacity * sizeof(T));
    }

    void ensure_capacity(size_t newCapacity) {
        if (capacity < newCapacity) {
            size_t oldCapacity = capacity;
            while (capacity < newCapacity) {
                // capacity = capcity * 1.5 + 8
                capacity = (capacity << 1) - (capacity >> 1) + 8;
            }
            data = mem::realloc<T>(data, oldCapacity * sizeof(T), capacity * sizeof(T));
        }
    }

    void add(T &item) {
        ensure_capacity(size + 1);
        data[size++] = item;
    }

    T &get(size_t i) {
        assert(0 <= i && i < size);
        return data[i];
    }

    T *data{nullptr};
    size_t capacity{0};
    size_t size{0};
};

}  // namespace lcc

#endif
