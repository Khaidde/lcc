#include "file.hpp"

#include <dirent.h>
#include <sys/stat.h>
#include <windows.h>

#include <cstdio>

namespace lcc::file {

FileErrCode read_file(FileInfo **out, const char *filepath) {
    FILE *file = fopen(filepath, "rb");
    if (!file) {
        return FileErrCode::kNotFound;
    }
    FileInfo *finfo = mem::gb_alloc<FileInfo>();
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
                finfo->src.compact();
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

LStringView split_dir(const char *path) {
    const char *ptr = path;
    const char *sep = path;
    while (*ptr) {
        if (*ptr == '/' || *ptr == '\\') sep = ptr;
        ptr++;
    }
    if (sep == path) {
        return {".", 1};
    } else {
        return {path, (size_t)(sep - path)};
    }
}

namespace {

void strcpy_remove_backslash(char *dest, char *src) {
    while (*src) {
        if (*src == '\\') {
            *dest = '/';
        } else {
            *dest = *src;
        }
        dest++;
        src++;
    }
    *dest = '\0';
}

bool has_extension(const char *path, const char *extension) {
    size_t pathlen = strlen(path);
    size_t extlen = strlen(extension);
    for (size_t i = 0; i < extlen; i++) {
        if (path[pathlen - i - 1] != extension[extlen - i - 1]) return false;
        if (i == 0) break;
    }
    return true;
}

}  // namespace

FileErrCode file_in_dir(LList<LString> &outfiles, LString &dirname) {
    assert(dirname.size < MAX_PATH && "Directory path is too long");
    char buf[MAX_PATH];
    strcpy_remove_backslash(buf, dirname.data);
    if (DIR *dir = opendir(buf)) {
        buf[dirname.size - 1] = '/';
        dirent *ent;
        while ((ent = readdir(dir)) != nullptr) {
            strcpy_remove_backslash(&buf[dirname.size], ent->d_name);
            if (is_regular_file(buf) && has_extension(buf, ".tc")) {
                outfiles.add(lstr_create(buf));
            }
        }
        closedir(dir);
        return FileErrCode::kSuccess;
    }
    return FileErrCode::kNotFound;
}

}  // namespace lcc::file
