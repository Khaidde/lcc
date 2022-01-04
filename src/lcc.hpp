#ifndef LCC_HPP
#define LCC_HPP

#include "types.hpp"

namespace lcc {

enum class ErrCode {
    kSuccess,
    kFailure,
};

ErrCode command_line(int argc, char **argv);

ErrCode compile(const char *path);

CompilationContext preload(const char *preloadFilePath);

ErrCode resolve_packages(CompilationContext &cmp, const char *mainFile);

}  // namespace lcc

#endif
