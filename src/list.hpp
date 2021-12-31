#ifndef LCC_LIST_HPP
#define LCC_LIST_HPP

#include <cassert>
#include <cstdint>

#include "mem.hpp"

namespace lcc {

template <typename T>
struct LList {
    void init(size_t initialSize) {
        assert(initialSize && "Initial size of list must be positive");
        data = mem::c_malloc<T>(initialSize);
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
        capacity = (capacity << 1) - (capacity >> 1) + 8;
        data = mem::c_realloc<T>(data, capacity);
    }

    void ensure_capacity(size_t newCapacity) {
        if (capacity < newCapacity) {
            while (capacity < newCapacity) {
                // capacity = capcity * 1.5 + 8
                capacity = (capacity << 1) - (capacity >> 1) + 8;
            }
            data = mem::c_realloc<T>(data, capacity);
        }
    }

    void add(T &item) {
        ensure_capacity(size + 1);
        data[size++] = item;
    }

    void add(T &&item) {
        ensure_capacity(size + 1);
        data[size++] = item;
    }

    T &get(size_t i) {
        assert(0 <= i && i < size);
        return data[i];
    }

    T &last() {
        assert(size > 0);
        return data[size - 1];
    }

    T *data{nullptr};
    size_t capacity{0};
    size_t size{0};
};

}  // namespace lcc

#endif
