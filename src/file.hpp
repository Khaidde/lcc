#ifndef LCC_FILE_HPP
#define LCC_FILE_HPP

#include "list.hpp"
#include "lstring.hpp"
#include "types.hpp"

namespace lcc::file {

enum class FileErrCode {
    kSuccess,
    kNotFound,
    kInternalError,
};

FileErrCode read_file(FileInfo **out, const char *filepath);

bool is_regular_file(const char *path);

LStringView split_dir(const char *path);

FileErrCode file_in_dir(LList<LString> &outfiles, LString &dirname);

};  // namespace lcc::file

#endif
