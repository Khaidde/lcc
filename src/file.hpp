#ifndef LCC_FILE_HPP
#define LCC_FILE_HPP

#include "list.hpp"
#include "lstring.hpp"

namespace lcc::file {

enum class FileErrCode : unsigned char {
    kSuccess,
    kNotFound,
    kInternalError,
};

FileErrCode read_file(LString &out, LString &path);

FileErrCode get_files_same_dir(const char *filepath, LList<LString> &filenames);

};  // namespace lcc::file

#endif
