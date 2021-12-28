#ifndef LCC_HPP
#define LCC_HPP

#include "astnode.hpp"

namespace lcc {

enum class ErrCode : unsigned char {
    kSuccess,
    kFailure,
};

ErrCode command_line(int argc, char **argv);

ErrCode compile(const char *path);

}  // namespace lcc

#endif
