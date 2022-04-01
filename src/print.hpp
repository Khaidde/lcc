#ifndef LCC_PRINT_HPP
#define LCC_PRINT_HPP

#include <cstdio>

namespace lcc {

constexpr const char *header = "  lcc: ";
constexpr size_t kHeaderLen = 7;

constexpr const char *kAnsiColorRed = "\x1b[31m";
constexpr const char *kAnsiColorGreen = "\x1b[32m";
constexpr const char *kAnsiColorYellow = "\x1b[33m";
constexpr const char *kAnsiColorBlue = "\x1b[34m";
constexpr const char *kAnsiColorMagenta = "\x1b[35m";
constexpr const char *kAnsiColorCyan = "\x1b[36m";
constexpr const char *kAnsiColorGrey = "\x1b[90m";
constexpr const char *kAnsiColorReset = "\x1b[0m";

extern bool printUseColor;

void print_color(const char *color);

void reset_print_color();

[[noreturn]] void unreachable();

void err(const char *format, ...);

void todo(const char *format, ...);

void debug(const char *format, ...);

void info(const char *format, ...);

}  // namespace lcc

#endif
