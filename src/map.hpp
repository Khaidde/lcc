#ifndef LCC_MAP_HPP
#define LCC_MAP_HPP

#include <cassert>
#include <cstdint>

#include "mem.hpp"

namespace lcc {

using HashValue = uint32_t;

template <typename K>
struct HashFn {
    static_assert(!sizeof(K), "No hash function implemented for type");
    HashValue operator()(K) { return 0; }
};

template <typename K>
struct HashFn<K *> {
    HashValue operator()(K *ptr) { return (uintptr_t)(ptr) / sizeof(K); }
};

template <typename K>
struct EqualFn {
    static_assert(!sizeof(K), "No equal function implemented for type");
    bool operator()(K, K) { return false; }
};

template <typename K>
struct EqualFn<K *> {
    bool operator()(K *ptr1, K *ptr2) { return ptr1 == ptr2; }
};

// Robin hood algorithm based hashmap
template <typename K, typename V, typename H = HashFn<K>, typename E = EqualFn<K>>
struct LMap {
    using Key = K;
    using Value = V;

    struct Entry {
        Key key;
        Value val;
    };

    struct TableEntry {
        Entry entry;
        size_t psl;  // Probe sequence length, psl=0 means entry is empty
    };

    void init() { init(8); }

    void init(size_t newCapacity) {
        capacity = newCapacity;
        table = mem::c_alloc<TableEntry>(capacity);
        clear();
    }

    void destroy() {
        if (table) mem::c_free(table);
    }

    void clear() {
        size = 0;
        maxPSL = 0;
        for (size_t i = 0; i < capacity; i++) {
            table[i].psl = 0;
        }
    }

    Value *try_put(Key &key, Value &val) {  // Returns duplicate key, val if found
        // Load factor of 0.67...
        if ((size + 1) * 3 >= capacity << 1) {
            TableEntry *oldTable = table;
            size_t oldCapacity = capacity;
            init(capacity << 1);
            for (size_t i = 0; i < oldCapacity; i++) {
                if (oldTable[i].psl) {
                    internal_try_put(oldTable[i].entry.key, oldTable[i].entry.val);
                }
            }
            mem::c_free(oldTable);
        }
        return internal_try_put(key, val);
    }

    Value *try_put(Key &key, Value &&val) { return try_put(key, val); }

    Value *operator[](Key &&key) { return (*this)[key]; }

    Value *operator[](Key &key) {
        size_t ndx = hash(key) % capacity;
        for (size_t off = 0; off < maxPSL; off++) {
            TableEntry *tentry = &table[ndx];
            if (tentry->psl && equal(tentry->entry.key, key)) return &tentry->entry.val;
            ndx = (ndx + 1) % capacity;
        }
        return nullptr;
    }

    Value *remove(Key &key) {
        size_t ndx = hash(key) % capacity;
        for (size_t off = 0; off < maxPSL; off++) {
            TableEntry *tentry = &table[ndx];
            if (tentry->psl && equal(tentry->entry.key, key)) {
                for (;;) {
                    ndx = (ndx + 1) % capacity;
                    TableEntry *next = &table[ndx];
                    if (next->psl <= 1) {
                        tentry->psl = 0;
                        size--;
                        break;
                    }
                    tentry->entry = {next->key, next->val};
                    tentry->psl = next->psl - 1;
                    tentry = next;
                }
                break;
            }
            ndx = (ndx + 1) % capacity;
        }
        return nullptr;
    }

    struct Iterator {
        Entry &operator*() const { return map->table[idx].entry; }

        Iterator &operator++() {
            idx = map->get_valid_entry_index(idx + 1);
            return *this;
        }

        friend bool operator==(const Iterator &a, const Iterator &b) {
            assert(a.map == b.mpa);
            return a.idx == b.idx;
        }

        friend bool operator!=(const Iterator &a, const Iterator &b) {
            assert(a.map == b.map);
            return a.idx != b.idx;
        }

        size_t idx;
        LMap<K, V> *map;
    };

    Iterator begin() { return {get_valid_entry_index(0), this}; }

    Iterator end() { return {capacity, this}; }

    size_t get_valid_entry_index(size_t idx) {
        while (idx < capacity && !table[idx].psl) {
            idx++;
        }
        return idx;
    }

    TableEntry *table{nullptr};
    size_t capacity{0};
    size_t size{0};
    size_t maxPSL{0};
    H hash{};
    E equal{};

private:
    Value *internal_try_put(Key key, Value val) {
        size_t ndx = hash(key) % capacity;
        for (size_t off = 0, p = 1; off < capacity; off++, p++) {
            TableEntry *tentry = &table[ndx];
            if (tentry->psl == 0) {
                if (p > maxPSL) maxPSL = p;
                *tentry = {{key, val}, p};
                size++;
                return nullptr;
            }

            if (equal(tentry->entry.key, key)) return &tentry->entry.val;

            if (tentry->psl < p) {
                if (p > maxPSL) maxPSL = p;
                TableEntry temp = *tentry;
                *tentry = {{key, val}, p};
                key = temp.entry.key;
                val = temp.entry.val;
                p = temp.psl;
            }
            ndx = (ndx + 1) % capacity;
        }
        assert(!"Map is full");
        return nullptr;
    }
};

}  // namespace lcc

#endif
