#include "print.hpp"

#include <cstdarg>

namespace lcc {

bool printUseColor = true;

void print_color(const char *color) {
    if (printUseColor) printf("%s", color);
}

void reset_print_color() {
    if (printUseColor) printf("%s", kAnsiColorReset);
}

#define DEFINE_PRINT(stream, name, color)                              \
    do {                                                               \
        va_list ap;                                                    \
        va_start(ap, format);                                          \
        fprintf(stream, "%s", header);                                 \
        if (printUseColor) {                                           \
            fprintf(stream, "%s%s: %s", color, name, kAnsiColorReset); \
        } else {                                                       \
            fprintf(stream, "%s: ", name);                             \
        }                                                              \
        vfprintf(stream, format, ap);                                  \
        va_end(ap);                                                    \
    } while (0)

void err(const char *format, ...) { DEFINE_PRINT(stderr, "err", kAnsiColorRed); }

void todo(const char *format, ...) {
#ifndef NDEBUG
    DEFINE_PRINT(stderr, "TODO", kAnsiColorYellow);
#else
    (void)format;
    (void)kAnsiColorYellow;
#endif
}

void debug(const char *format, ...) {
#ifndef NDEBUG
    DEFINE_PRINT(stdout, "debug", kAnsiColorCyan);
#else
    (void)format;
    (void)kAnsiColorCyan;
#endif
}

void info(const char *format, ...) { DEFINE_PRINT(stdout, "info", kAnsiColorReset); }

#undef DEFINE_PRINT

}  // namespace lcc
