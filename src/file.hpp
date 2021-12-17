#ifndef LCC_FILE_HPP
#define LCC_FILE_HPP

#include "lstring.hpp"

namespace lcc::file {

enum class FileErrCode : unsigned char {
    kSuccess,
    kFileNotFound,
    kFileInternalError,
};

FileErrCode read_file(LString &out, const char *path);

};  // namespace lcc::file

#endif
