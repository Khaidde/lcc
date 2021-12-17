#include "file.hpp"

#include <cstdio>

namespace lcc::file {

FileErrCode read_file(LString &out, const char *path) {
    FILE *file = fopen(path, "rb");
    if (!file) {
        return FileErrCode::kFileNotFound;
    }

    out.init(0x100);
    for (;;) {
        size_t totalRead = fread(out.data + out.size, 1, out.capacity - out.size, file);
        out.size += totalRead;

        if (out.size != out.capacity) {
            FileErrCode rv = FileErrCode::kSuccess;
            if (!feof(file)) {
                rv = FileErrCode::kFileInternalError;
            }
            fclose(file);
            return rv;
        }
        out.resize();
    }
}

}  // namespace lcc::file
