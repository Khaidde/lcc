#include <stdio.h>

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

int main() {
    find_string("a");
    find_string("_Butterfly _a+sdf");
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
    mem::malloc<int>(4);

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
