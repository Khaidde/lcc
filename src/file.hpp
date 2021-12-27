#ifndef LCC_FILE_HPP
#define LCC_FILE_HPP

#include "list.hpp"
#include "lstring.hpp"

namespace lcc::file {

struct FileInfo {
    const char *path;
    LString src;
};

enum class FileErrCode : unsigned char {
    kSuccess,
    kNotFound,
    kInternalError,
};

FileErrCode read_file(FileInfo **out, const char *filepath);

bool is_regular_file(const char *path);

LString get_dir(const char *path);

FileErrCode file_in_dir(LList<LString> &outfiles, LString &dirname);

};  // namespace lcc::file

#endif
