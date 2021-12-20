#include "print.hpp"

#include <cstdarg>
#include <cstring>
#include <exception>

namespace lcc {

bool printUseColor = true;

void print_color(const char *color) {
    if (printUseColor) printf("%s", color);
}

void reset_print_color() {
    if (printUseColor) printf("%s", kAnsiColorReset);
}

namespace {

// TODO: edge cases where dest is too small to hold entire concatenation
void strcat_color(char *dest, const char *src, const char *col) {
    if (printUseColor) {
        while (*col) {
            *(dest++) = *col;
            col++;
        }
    }
    while (*src) {
        *(dest++) = *src;
        src++;
    }
    if (printUseColor) {
        const char *colReset = kAnsiColorReset;
        while (*colReset) {
            *(dest++) = *colReset;
            colReset++;
        }
    }
    *dest = '\0';
}

}  // namespace

#define DEFINE_PRINT(stream, name, color)                     \
    do {                                                      \
        va_list ap;                                           \
        va_start(ap, format);                                 \
        char buffer[100];                                     \
        memcpy(buffer, header, kHeaderLen);                   \
        strcat_color(buffer + kHeaderLen, #name ": ", color); \
        size_t messageLen = strlen(buffer);                   \
        vsprintf(buffer + messageLen, format, ap);            \
        fprintf(stream, buffer);                              \
        va_end(ap);                                           \
    } while (0)

void unreachable() {
    char buffer[20] = "  lcc: ";
    size_t headerLen = strlen(buffer);
    strcat_color(buffer + headerLen, "unreachable ", kAnsiColorRed);
    fprintf(stderr, buffer);
    throw std::exception();
}

void panic(const char *format, ...) {
    DEFINE_PRINT(stderr, panic, kAnsiColorRed);
    throw std::exception();
}

void err(const char *format, ...) { DEFINE_PRINT(stderr, err, kAnsiColorRed); }

void todo(const char *format, ...) {
#ifndef NDEBUG
    DEFINE_PRINT(stderr, TODO, kAnsiColorYellow);
#else
    (void)format;
    (void)kAnsiColorYellow;
#endif
}

void debug(const char *format, ...) {
#ifndef NDEBUG
    DEFINE_PRINT(stdout, debug, kAnsiColorCyan);
#else
    (void)format;
    (void)kAnsiColorCyan;
#endif
}

void info(const char *format, ...) { DEFINE_PRINT(stdout, info, kAnsiColorReset); }

#undef DEFINE_PRINT

}  // namespace lcc
