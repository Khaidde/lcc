#ifndef LCC_TYPES_HPP
#define LCC_TYPES_HPP

#include "astnode.hpp"
#include "hashmap.hpp"
#include "list.hpp"
#include "lstring.hpp"
#include "scope.hpp"
#include "util.hpp"

namespace lcc {

struct ScopeStack;

struct FileUnit {
    file::FileInfo *finfo;
    Node *unit;
    size_t numUnresolvedImports;
};

struct Package {
    ScopeStack *scopeStack;
    LList<FileUnit> files;
};

struct ExecutionContext {
    LMap<LStringView, Package, lstr_hash, lstr_equal> packageMap;
    ScopeStack *currScopeStack;

    FileUnit *currFile;
};

}  // namespace lcc

#endif
