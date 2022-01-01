#ifndef LCC_TYPES_HPP
#define LCC_TYPES_HPP

#include <cstdint>

#include "list.hpp"
#include "lstring.hpp"
#include "map.hpp"

namespace lcc {

enum Result {
    kAccept = 0,
    kError = 1,
};

struct FileInfo {
    const char *path;
    LString src;
};

struct Node;

struct File {
    struct Package *package;
    FileInfo *finfo;

    LMap<LStringView, Node *, lstr_hash, lstr_equal> imports;
};

struct Package {
    LMap<LStringView, Node *, lstr_hash, lstr_equal> globalDecls;
    LList<File *> files;
};

struct CompilationContext {
    LMap<LStringView, Package *, lstr_hash, lstr_equal> packageMap;
    bool isPackageResolutionSuccesful;

    struct ScopeStack *scopeStack;

    struct FunctionCtx {
        File *file;
        Node *func;
        bool isSeparator;
    };
    LList<FunctionCtx> resolveFuncBodyStack;
    size_t currNumPendingFunc;
    Node *currFunction;
    File *currFile;
};

}  // namespace lcc

#endif
