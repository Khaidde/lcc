#ifndef LCC_HPP
#define LCC_HPP

namespace lcc {

enum class ErrCode {
    kSuccess,
    kFailure,
};

ErrCode command_line(int argc, char **argv);

ErrCode compile(const char *path);

}  // namespace lcc

#endif
