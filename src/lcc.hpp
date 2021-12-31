#ifndef LCC_HPP
#define LCC_HPP

#include "types.hpp"

namespace lcc {

enum class ErrCode {
    kSuccess,
    kFailure,
};

CompilationContext resolve_packages(const char *mainFile);

ErrCode command_line(int argc, char **argv);

ErrCode compile(const char *path);

}  // namespace lcc

#endif
