#include "print.hpp"

#include <windows.h>

#include <cstdarg>
#include <cstdlib>

#include "util.hpp"

namespace lcc {

Color kColorRed = FOREGROUND_RED;
Color kColorGreen = FOREGROUND_GREEN;
Color kColorYellow = FOREGROUND_RED | FOREGROUND_GREEN;
Color kColorBlue = FOREGROUND_BLUE;
Color kColorMagenta = FOREGROUND_RED | FOREGROUND_BLUE;
Color kColorCyan = FOREGROUND_GREEN | FOREGROUND_BLUE;
Color kColorWhite = FOREGROUND_RED | FOREGROUND_GREEN | FOREGROUND_BLUE;

bool printUseColor = true;

HANDLE hOut = GetStdHandle(STD_OUTPUT_HANDLE);

void set_color(Color color) {
    if (printUseColor) SetConsoleTextAttribute(hOut, color);
}

void reset_color() {
    if (printUseColor) SetConsoleTextAttribute(hOut, kColorWhite);
}

#define DEFINE_PRINT(stream, name, color) \
    do {                                  \
        va_list ap;                       \
        va_start(ap, format);             \
        fprintf(stream, "%s", header);    \
        set_color(color);                 \
        fprintf(stream, "%s: ", name);    \
        reset_color();                    \
        vfprintf(stream, format, ap);     \
        va_end(ap);                       \
    } while (0)

void unreachable() {
    printf("\nUNREACHABLE\n");
    exit(1);
}

void err(const char *format, ...) { DEFINE_PRINT(stderr, "err", kColorRed); }

void todo(const char *format, ...) {
#if DBG
    DEFINE_PRINT(stderr, "TODO", kColorYellow);
#else
    (void)format;
    (void)kColorYellow;
#endif
}

void debug(const char *format, ...) {
#if DBG
    DEFINE_PRINT(stdout, "debug", kColorCyan);
#else
    (void)format;
    (void)kColorCyan;
#endif
}

void info(const char *format, ...) { DEFINE_PRINT(stdout, "info", kColorGreen); }

#undef DEFINE_PRINT

}  // namespace lcc
