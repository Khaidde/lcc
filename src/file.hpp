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

void replace_backslashes(char *dest, const char *src);

FileErrCode read_file(LString &out, LString &path);

FileErrCode get_files_same_dir(const char *filepath, LList<LString> &filenames);

};  // namespace lcc::file

#endif
