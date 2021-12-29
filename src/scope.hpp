#ifndef LCC_SCOPE_HPP
#define LCC_SCOPE_HPP

#include "astnode.hpp"
#include "hashmap.hpp"

namespace lcc {

constexpr size_t kMaxScopeDepth = 8;

struct ScopeStack {
    size_t size;
    LMap<LStringView, struct Node *, lstr_hash, lstr_equal> scopes[kMaxScopeDepth];
};

struct File {
    file::FileInfo *finfo;
    struct Node *unit;
    ScopeStack *scopeStack;
};

struct Package {
    LMap<LStringView, struct Node *, lstr_hash, lstr_equal> globalDecls;
    LList<File *> files;
};

struct Context {
    Package *package;
    File *file;
};

struct CompilationContext {
    LMap<LStringView, Package *, lstr_hash, lstr_equal> packageMap;
    bool isPackageResolutionSuccesful;

    Context ctx;
};

ScopeStack *scope_init();

void scope_enter(ScopeStack *stack);

void scope_exit(ScopeStack *stack);

Node *scope_bind(ScopeStack *stack, Node *decl);

Node *decl_lookup(CompilationContext *cmp, LStringView &symbol);

CompilationContext resolve_packages(const char *mainFile);

}  // namespace lcc

#endif
