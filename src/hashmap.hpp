#ifndef LCC_HASHMAP_HPP
#define LCC_HASHMAP_HPP

#include "lstring.hpp"
#include "util.hpp"

namespace lcc {

// Robin hood algorithm based hashmap
template <typename K, typename V, u32 hash_func(K &), bool equal_func(K &, K &)>
struct LMap {
    struct Entry {
        K key;
        V val;
        size_t psl;  // Probe sequence length, psl=0 means entry is empty
    };
    void init() { init(8); }
    void init(size_t newCapacity) {
        capacity = newCapacity;
        size = 0;
        maxPSL = 0;
        table = mem::c_malloc<Entry>(capacity);
        for (size_t i = 0; i < capacity; i++) {
            table[i].psl = 0;
        }
    }
    V *try_put(K &key, V &val) {  // Returns duplicate key, val if found
        // Load factor of 0.67...
        if ((size + 1) * 3 >= capacity << 1) {
            Entry *oldTable = table;
            size_t oldCapacity = capacity;
            init(capacity << 1);
            for (size_t i = 0; i < oldCapacity; i++) {
                if (oldTable[i].psl) {
                    internal_try_put(oldTable[i].key, oldTable[i].val);
                }
            }
            mem::c_free(oldTable);
        }
        return internal_try_put(key, val);
    }
    V *get(K &key) {
        u32 ndx = hash_func(key) % capacity;
        for (size_t off = 0; off < maxPSL; off++) {
            Entry *entry = &table[ndx];
            if (entry->psl && equal_func(entry->key, key)) return &entry->val;
            ndx = (ndx + 1) % capacity;
        }
        return nullptr;
    }

    Entry *table{nullptr};
    size_t capacity;
    size_t size;
    size_t maxPSL;

private:
    V *internal_try_put(K &key, V &val) {
        u32 ndx = hash_func(key) % capacity;
        for (size_t off = 0, p = 1; off < capacity; off++, p++) {
            Entry *entry = &table[ndx];
            if (entry->psl == 0) {
                if (p > maxPSL) maxPSL = p;
                *entry = {key, val, p};
                size++;
                return nullptr;
            }

            if (equal_func(entry->key, key)) return &entry->val;

            if (entry->psl < p) {
                if (p > maxPSL) maxPSL = p;
                Entry temp = *entry;
                *entry = {key, val, p};
                key = temp.key;
                val = temp.val;
                p = temp.psl;
            }
            ndx = (ndx + 1) % capacity;
        }
        assert(false && "Map is full");
        return nullptr;
    }
};

}  // namespace lcc

#endif