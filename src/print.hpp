#ifndef LCC_PRINT_HPP
#define LCC_PRINT_HPP

#include <cstdio>
#include <util.hpp>

namespace lcc {

constexpr const char *header = "lcc: ";
constexpr size_t kHeaderLen = comptime_strlen(header);

extern bool printUseColor;

using Color = size_t;
extern Color kColorRed;
extern Color kColorGreen;
extern Color kColorYellow;
extern Color kColorBlue;
extern Color kColorMagenta;
extern Color kColorCyan;
extern Color kColorWhite;

void set_color(Color color);

void reset_color();

[[noreturn]] void unreachable();

void err(const char *format, ...);

void todo(const char *format, ...);

void debug(const char *format, ...);

void info(const char *format, ...);

}  // namespace lcc

#endif
