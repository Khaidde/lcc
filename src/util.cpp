#include "util.hpp"

namespace lcc {

int clzll(unsigned long long num) { return __builtin_clzll(num); }

int ctzll(unsigned long long num) { return __builtin_ctzll(num); }

int lsb(uint64_t num) { return ctzll(num); }

int msb(uint64_t num) { return 64 - clzll(num); }

}  // namespace lcc
