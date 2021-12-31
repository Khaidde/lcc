#ifndef LCC_TYPES_HPP
#define LCC_TYPES_HPP

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

    struct ScopeStack *scopeStack;
};

struct Package {
    LMap<LStringView, Node *, lstr_hash, lstr_equal> globalDecls;
    LList<File *> files;
};

struct CompilationContext {
    LMap<LStringView, Package *, lstr_hash, lstr_equal> packageMap;
    bool isPackageResolutionSuccesful;

    struct FunctionCtx {
        File *file;
        Node *func;
    };
    LList<FunctionCtx> resolveFuncBodyStack;
    Node *currFunction;
    File *currFile;
};

}  // namespace lcc

#endif
