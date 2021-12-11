#ifndef LCC_LIST_HPP
#define LCC_LIST_HPP

#include <cassert>

#include "mem.hpp"
#include "print.hpp"

namespace lcc {

template <typename T>
struct LList {
    void init(size_t initialSize) {
        data = mem::malloc<T>(sizeof(T) * initialSize);
        size = initialSize;
        capacity = initialSize;
    }

    void init(T *items, size_t initialSize) {
        init(initialSize);
        for (size_t i = 0; i < initialSize; i++) {
            data[i] = items[i];
        }
    }

    void ensure_capacity(size_t newCapacity) {
        if (capacity < newCapacity) {
            while (capacity < newCapacity) {
                // capacity = capcity * 1.5 + 1
                capacity += (capacity << 1) - (capacity >> 1) + 1;
            }
            data = mem::realloc(data, capacity);
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

using LString = LList<char>;

struct LStrView {
    const char *string;
    size_t len;
};

static inline LString str_init(const char *str) {
    LString lStr;

    const char *orig = str;
    while (*str) str++;
    lStr.init((size_t)(str - orig) + 1);

    char *in = lStr.data;
    while (*orig) *(in++) = *(orig++);
    *in = '\0';

    return lStr;
}

}  // namespace lcc

#endif
