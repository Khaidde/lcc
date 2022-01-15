#ifndef LCC_TYPES_HPP
#define LCC_TYPES_HPP

#include <cstdint>

#include "list.hpp"
#include "lstring.hpp"
#include "map.hpp"

namespace lcc {

struct Node;
struct Type;

enum Result {
    kAccept = 0,
    kError = 1,
};

struct FileInfo {
    const char *path;
    LString src;
};

struct File {
    struct Package *package;
    FileInfo *finfo;

    Node *importListHead;
    LMap<LStringView, Node *, lstr_hash, lstr_equal> imports;
};

struct DeclInfo {
    bool isResolving : 1;
    bool isUsed : 1;

    DeclInfo *nextDecl;
    Node *declNode;
    File *file;
};

struct Package {
    DeclInfo *globalDeclListHead;
    LMap<LStringView, DeclInfo *, lstr_hash, lstr_equal> globalDecls;

    LList<File *> files;
};

struct CompilationContext {
    Package *preloadPkg;

    LMap<LStringView, Package *, lstr_hash, lstr_equal> packageMap;

    struct ScopeStack *scopeStack;

    LList<Node *> resolveFuncBodyStack;
    size_t currNumPendingFunc;
    Node *currRetTy;
    Node *currResolvingTypeAlias;
    File *currFile;
};

}  // namespace lcc

#endif
