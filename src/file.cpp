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

void replace_backslashes(char *dest, const char *src) {
    while (*src) {
        if (*src == '\\') {
            *dest = '/';
        } else {
            *dest = *src;
        }
        src++;
        dest++;
    }
    *dest = '\0';
}

FileErrCode read_file(LString &out, LString &path) {
    FILE *file = fopen(path.data, "rb");
    if (!file) {
        return FileErrCode::kNotFound;
    }

    out.init(0x100);
    for (;;) {
        size_t totalRead = fread(out.data + out.size, 1, out.capacity - out.size, file);
        out.size += totalRead;

        if (out.size != out.capacity) {
            FileErrCode rv = FileErrCode::kSuccess;
            if (!feof(file)) {
                rv = FileErrCode::kInternalError;
            }
            fclose(file);
            return rv;
        }
        out.resize();
    }
}

namespace {

FileErrCode get_abs_dir(const char *filename, char *outdir) {
    // if (!GetFullPathName(filename, MAX_PATH, outdir, nullptr)) {
    // return FileErrCode::kInternalError;
    // }
    strcpy(outdir, filename);

    char *ptr = outdir;
    char *lastSlash = nullptr;
    while (*ptr) {
        if (*ptr == '/' || *ptr == '\\') lastSlash = ptr;
        ptr++;
    }
    *lastSlash = '\0';
    return FileErrCode::kSuccess;
}

}  // namespace

FileErrCode get_files_same_dir(const char *filepath, LList<LString> &filenames) {
    char dirname[MAX_PATH];
    get_abs_dir(filepath, dirname);
    // strcpy(dirname, filepath);
    size_t dirnameLen = strlen(dirname);

    char fullpath[MAX_PATH];
    if (DIR *dir = opendir(dirname)) {
        dirent *ent;
        while ((ent = readdir(dir)) != nullptr) {
            struct stat pathStat;

            strcpy(fullpath, dirname);
            fullpath[dirnameLen] = '\\';
            memcpy(fullpath + dirnameLen + 1, ent->d_name, ent->d_namlen);
            fullpath[dirnameLen + ent->d_namlen + 1] = '\0';
            stat(fullpath, &pathStat);

            if (!S_ISDIR(pathStat.st_mode)) {
                LString filename = lstr_create(fullpath);
                filenames.add(filename);
            }
        }
        closedir(dir);
        return FileErrCode::kSuccess;
    }
    return FileErrCode::kNotFound;
}

}  // namespace lcc::file
