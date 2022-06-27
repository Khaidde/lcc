#ifndef LCC_LIST_HPP
#define LCC_LIST_HPP

#include <cassert>
#include <cstdint>

#include "mem.hpp"

namespace lcc {

template <typename T>
struct ListIterator {
    T operator*() const { return *curr; }

    ListIterator &operator++() {
        ++curr;
        return *this;
    }

    friend bool operator==(const ListIterator &a, const ListIterator &b) { return a.curr == b.curr; }

    friend bool operator!=(const ListIterator &a, const ListIterator &b) { return a.curr != b.curr; }

    T *curr;
};

struct FixedBitField {
    void init(size_t capacity) {
        this->capacity = capacity;
        if (capacity <= 64) {
            data = (uint8_t *)&smallData;
            smallData = 0;
        } else {
            data = mem::c_malloc<uint8_t>(capacity >> 2);
            for (size_t i = 0; i < (capacity >> 2); i++) {
                data[i] = 0;
            }
        }
    }

    void destroy() {
        if (capacity > 64) mem::c_free(data);
    }

    void reset() {
        if (capacity <= 64) {
            smallData = 0;
        } else {
            for (size_t i = 0; i < (capacity >> 2); i++) {
                data[i] = 0;
            }
        }
    }

    void set(size_t i) {
        assert(i < capacity);
        data[i >> 2] |= 1 << (i & 0x7);
    }

    void clear(size_t i) {
        assert(i < capacity);
        data[i >> 2] &= ~((uint8_t)(1 << (i & 0x7)));
    }

    bool operator[](size_t i) {
        assert(i < capacity);
        return data[i >> 2] & (1 << (i & 0x7));
    }

#ifndef NDEBUG
    size_t capacity;
#endif
    uint64_t smallData;
    uint8_t *data;
};

struct SparseSet {
    void init(size_t capacity) { init(capacity, capacity); }

    void init(size_t capacity, size_t valLim) {
        dense = mem::c_malloc<size_t>(capacity);
        sparse = mem::c_malloc<size_t>(valLim);
        size = 0;
#ifndef NDEBUG
        this->capacity = capacity;
        this->valLim = valLim;
#endif
    }

    void clear() { size = 0; }

    bool try_add(size_t val) {
        assert(val < valLim);
        if (contains(val)) return false;
        dense[size] = val;
        sparse[val] = size++;
        return true;
    }

    size_t pop() {
        assert(size > 0);
        return dense[--size];
    }

    bool contains(size_t val) {
        assert(val < valLim);
        size_t s = sparse[val];
        return s < size && dense[s] == val;
    }

    size_t *dense;
    size_t *sparse;
    size_t size;
#ifndef NDEBUG
    size_t capacity;  // size of dense list
    size_t valLim;    // size of sparse list
#endif
};

template <typename T>
struct LArray {
    void init(size_t sz) {
        assert(sz && "Size of array must be positive");
        data = mem::c_malloc<T>(sz);
        size = sz;
    }

    T &operator[](size_t i) {
        assert(0 <= i && i < size);
        return data[i];
    }

    T *data{nullptr};
    size_t size{0};
};

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

    void compact() {
        capacity = size;
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

    T remove() { return data[--size]; }

    T &operator[](size_t i) {
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
