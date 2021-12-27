#include <stdio.h>

#include "hashmap.hpp"
#include "list.hpp"
#include "mem.hpp"

using namespace lcc;
constexpr const char *kKeywords[]{
    "Adult",       "Aeroplane", "Air",        "Aircraft Carrier",
    "Airforce",    "Airport",   "Album",      "Alphabet",
    "Apple",       "Arm",       "Army",       "Baby",
    "Backpack",    "Balloon",   "Banana",     "Bank",
    "Barbecue",    "Bathroom",  "Bathtub",    "Bed",
    "Bee",         "Bible",     "Bird",       "Bomb",
    "Book",        "Boss",      "Bottle",     "Bowl",
    "Box",         "Boy",       "Brain",      "Bridge",
    "Butterfly",   "Button",    "Cappuccino", "Car",
    "Carpet",      "Carrot",    "Cave",       "Chair",
    "Chess Board", "Chief",     "Child",      "Chisel",
    "Chocolates",
};
constexpr size_t kNumKeywords = sizeof(kKeywords) / sizeof(const char *);

bool is_letter(char c) { return ('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z'); }

bool is_letter_or_underscore(char c) { return is_letter(c) || c == '_'; }

#include <cstring>

char peek(const char *str, size_t ndx) {
    if (ndx >= strlen(str)) return 0;
    return str[ndx];
}

void find_string(const char *str) {
    size_t cndx = 0;

    size_t ksi = 0;
    size_t kei = kNumKeywords - 1;
    while (char c = peek(str, cndx)) {
        if (!is_letter_or_underscore(c)) break;
        while (ksi <= kei && kKeywords[ksi][cndx] < c) ksi++;
        while (kei >= ksi && kKeywords[kei][cndx] > c) kei--;
        cndx++;
    }
    if (ksi == kei) {
        printf("keyword--%s--%d\n", kKeywords[kei], cndx);
    } else {
        printf("identifier--%s--%d\n", str, cndx);
    }
}

u32 int_hash(int &val) { return (u32)val; }

bool int_eq(int &val1, int &val2) { return val1 == val2; }

int main() {
    LMap<int, int, int_hash, int_eq> map;
    map.init(50);

    LList<int> keys{};

    int factor = 1;
    for (int i = 0; i < 30; i++) {
        factor *= (i + 1);
        int val = i;
        // factor %= 1000;
        keys.add(factor);
        if (!map.try_put(factor, val)) {
            printf("wtf-%d-%d\n", factor, i);
        }
    }

    for (size_t i = 0; i < map.capacity; i++) {
        if (map.table[i].psl != 0) {
            printf("[%d]-%d-%d\n", map.table[i].psl, map.table[i].key, map.table[i].val);
        } else {
            printf("-\n");
        }
    }
    // int val = 255024;
    for (size_t i = 0; i < 30; i++) {
        printf("-->%d\n", *map.get(keys.get(i)));
    }

    // find_string("a");
    // find_string("_Butterfly _a+sdf");
    return 0;
    /*
    printUseColor = false;

    mem::malloc<char>(150);

    constexpr int num = 100;
    int test[num];
    for (int i = 0; i < num; i++) test[i] = i * 3;

    LList<int> testL;
    testL.add(test[0]);
    int *ptr = testL.data;
    for (int i = 1; i < num / 3; i++) {
        testL.add(test[i]);
    }

    // An idiot did this
    mem::malloc<int>(1);

    for (int i = num / 3; i < num; i++) {
        testL.add(test[i]);
    }

    printf("\n");
    for (size_t i = 0; i < num; i++) {
        if (i % 20 == 0) {
            printf("  ");
        }
        printf("%03d", testL.get(i));
        if (i % 20 != 19) {
            printf("-");
        } else {
            printf("\n");
        }
    }
    printf("\n");

    printf("  %p-%p\n", ptr, testL.data);
    */
}
