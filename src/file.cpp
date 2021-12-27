#include "file.hpp"

#include <cstdio>

#if defined(_WIN32) && defined(__MINGW32__)
#include <direct.h>
#include <dirent.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <windows.h>
#else
#error "Compilation only supported on windows with posix libraries"
#endif

namespace lcc::file {

FileErrCode read_file(FileInfo **out, const char *filepath) {
    FILE *file = fopen(filepath, "rb");
    if (!file) {
        return FileErrCode::kNotFound;
    }
    FileInfo *finfo = mem::malloc<FileInfo>();
    finfo->path = filepath;
    finfo->src.init(0x100);
    for (;;) {
        size_t totalRead = fread(finfo->src.data + finfo->src.size, 1, finfo->src.capacity - finfo->src.size, file);
        finfo->src.size += totalRead;

        if (finfo->src.size != finfo->src.capacity) {
            if (!feof(file)) {
                *out = nullptr;
                fclose(file);
                return FileErrCode::kInternalError;
            } else {
                *out = finfo;
                fclose(file);
                return FileErrCode::kSuccess;
            }
        }
        finfo->src.resize();
    }
}

bool is_regular_file(const char *path) {
    struct stat pathStat;
    stat(path, &pathStat);
    return S_ISREG(pathStat.st_mode);
}

LString get_dir(const char *path) {
    LString out;
    out.init(MAX_PATH);

    char *optr = out.data;
    char *sep = nullptr;
    while (*path) {
        if (*path == '/' || *path == '\\') sep = optr;
        *(optr++) = *(path++);
    }
    if (sep) {
        *sep = '\0';
        out.size = (size_t)(sep - out.data + 1);
    } else {
        out.data[0] = '.';
        out.data[1] = '\0';
        out.size = 2;
    }
    return out;
}

FileErrCode file_in_dir(LList<LString> &outfiles, LString &dirname) {
    assert(dirname.size < MAX_PATH && "Directory path is too long");
    char buf[MAX_PATH];
    memcpy(buf, dirname.data, dirname.size - 1);
    size_t dirlen = dirname.size - 1;
    buf[dirlen] = '\0';

    if (DIR *dir = opendir(buf)) {
        buf[dirlen] = '/';
        dirent *ent;
        while ((ent = readdir(dir)) != nullptr) {
            memcpy(&buf[dirlen + 1], ent->d_name, ent->d_namlen);
            buf[dirlen + ent->d_namlen + 1] = '\0';
            if (is_regular_file(buf)) {
                LString filename = lstr_create(buf);
                outfiles.add(filename);
            }
        }
        closedir(dir);
        return FileErrCode::kSuccess;
    }
    return FileErrCode::kNotFound;
}

}  // namespace lcc::file
