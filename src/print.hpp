#ifndef LCC_PRINT_HPP
#define LCC_PRINT_HPP

namespace lcc {

[[noreturn]] void panic(const char *format, ...);

void err(const char *format, ...);

void todo(const char *format, ...);

void debug(const char *format, ...);

void info(const char *format, ...);

}  // namespace lcc

#endif
