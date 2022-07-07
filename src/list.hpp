#ifndef LCC_LIST_HPP
#define LCC_LIST_HPP

#include <cassert>
#include <cstdint>

#include "mem.hpp"
#include "util.hpp"

namespace lcc {

template <typename T>
struct ListIterator {
    T operator*() const { return *curr; }

    ListIterator &operator++() {
        ++curr;
        return *this;
    }

    ListIterator &operator--() {
        --curr;
        return *this;
    }

    friend bool operator==(const ListIterator &a, const ListIterator &b) { return a.curr == b.curr; }

    friend bool operator!=(const ListIterator &a, const ListIterator &b) { return a.curr != b.curr; }

    T *curr;
};

struct Bitset {
    template <typename A>
    void init(A &alloc, size_t capacity) {
        assert(capacity && "Bitset capacity must be positive");
        this->bitCapacity = capacity;
        if (capacity <= 64) {
            data = &smallData;
        } else {
            data = alloc.template alloc<uint64_t>(get_num_words());
        }
    }

    template <typename A>
    void init_zero(A &alloc, size_t capacity) {
        init(alloc, capacity);
        fill_zero();
    }

    template <typename A>
    void destroy(A &alloc) {
        if (bitCapacity > 64) alloc.free(data);
    }

    size_t get_num_words() { return (bitCapacity >> 6) + 1; }

    void fill_zero() {
        for (size_t i = 0; i < get_num_words(); i++) {
            data[i] = 0;
        }
    }

    void fill_one() {
        for (size_t i = 0; i < get_num_words(); i++) {
            data[i] = ~((uint64_t)0);
        }
        clear_leading_zeros();
    }

    void clear_leading_zeros() {
        size_t wordIdx = get_num_words() - 1;
        size_t mask = (1 << (bitCapacity & 63)) - 1;
        data[wordIdx] &= mask;
    }

    // Return whether or not there was a change
    bool set(size_t i) {
        assert(i < bitCapacity);
        uint64_t &word = data[i >> 6];
        uint64_t prev = word;
        word |= (uint64_t)1 << (i & 63);
        return word != prev;
    }

    // Return whether or not there was a change
    bool clear(size_t i) {
        assert(i < bitCapacity);
        uint64_t &word = data[i >> 6];
        uint64_t prev = word;
        word &= ~((uint64_t)1 << (i & 63));
        return word != prev;
    }

    bool operator[](size_t i) {
        assert(i < bitCapacity);
        return data[i >> 6] & (1 << (i & 63));
    }

    bool all_zero() {
        for (size_t i = 0; i < get_num_words(); i++) {
            if (data[i] != 0) return false;
        }
        return true;
    }

    void intersect(Bitset &other) {
        size_t cnt = MIN(get_num_words(), other.get_num_words());
        size_t i = 0;
        for (; i < cnt; i++) {
            data[i] &= other.data[i];
        }
        for (; i < get_num_words(); i++) {
            data[i] = 0;
        }
    }

    void intersect_not(Bitset &a, Bitset &b) {
        assert(bitCapacity >= a.bitCapacity);
        size_t cnt = MIN(a.get_num_words(), b.get_num_words());
        size_t i = 0;
        for (; i < cnt; i++) {
            data[i] = a.data[i] & ~b.data[i];
        }
        for (; i < a.get_num_words(); i++) {
            data[i] = a.data[i];
        }
    }

    void union_set(Bitset &other) {
        size_t cnt = MIN(get_num_words(), other.get_num_words());
        size_t i = 0;
        for (; i < cnt; i++) {
            data[i] |= other.data[i];
        }
    }

    struct Iterator {
        size_t operator*() const { return idx; }

        Iterator &operator++() {
            if (idx + 1 < set->bitCapacity) {
                idx = set->get_next_idx(idx + 1);
            } else {
                idx = set->bitCapacity;
            }
            return *this;
        }

        friend bool operator==(const Iterator &a, const Iterator &b) {
            assert(a.set == b.set);
            return a.idx == b.idx;
        }

        friend bool operator!=(const Iterator &a, const Iterator &b) {
            assert(a.set == b.set);
            return a.idx != b.idx;
        }

        size_t idx;
        Bitset *set;
    };

    Iterator begin() {
        clear_leading_zeros();
        return {get_next_idx(0), this};
    }

    Iterator end() { return {bitCapacity, this}; }

    size_t get_next_idx(size_t idx) {
        assert(idx < bitCapacity);

        size_t wordIdx = idx >> 6;
        size_t wordOff = idx & 63;

        size_t res = data[wordIdx] >> wordOff;
        if (res != 0) return idx + (size_t)lsb(res);

        for (size_t i = wordIdx + 1; i < get_num_words(); i++) {
            if (data[i] == 0) continue;
            return (i << 6) + (size_t)lsb(data[i]);
        }

        return bitCapacity;
    }

    size_t bitCapacity;  // Number of bits stored in set
    uint64_t smallData;
    uint64_t *data;
};

template <typename T>
struct LList {
    void init(size_t initialSize) {
        assert(initialSize && "Initial size of list must be positive");
        data = mem::c_alloc<T>(initialSize);
        size = 0;
        capacity = initialSize;
    }

    void destroy() {
        if (data) mem::c_free(data);
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

    T pop_back() { return data[--size]; }

    T &operator[](size_t i) {
        assert(0 <= i && i < size);
        return data[i];
    }

    T &back() {
        assert(size > 0);
        return data[size - 1];
    }

    T *data{nullptr};
    size_t capacity{0};
    size_t size{0};
};

}  // namespace lcc

#endif
