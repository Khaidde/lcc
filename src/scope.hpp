#ifndef LCC_SCOPE_HPP
#define LCC_SCOPE_HPP

#include "astnode.hpp"
#include "hashmap.hpp"

namespace lcc {

constexpr size_t kMaxScopeDepth = 8;

struct ScopeStack {
    size_t size;
    LMap<LStringView, Node *, lstr_hash, lstr_equal> scopes[kMaxScopeDepth];
};

struct FileUnit {
    file::FileInfo *finfo;
    Node *unit;
};

struct DeclContext {
    struct Package *package;
    FileUnit *fileUnit;
    Node *decl;
};

struct Package {
    LMap<LStringView, DeclContext, lstr_hash, lstr_equal> globalDecls;
    LList<FileUnit> files;
};

struct Context {
    Package *currPackage;
    FileUnit *currFile;
    ScopeStack *currScopeStack;
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

Node *scope_lookup(Context *ctx, LStringView &symbol);

DeclContext *scope_lookup_global(CompilationContext *cmp, LStringView &symbol);

CompilationContext resolve_packages(const char *mainFile);

}  // namespace lcc

#endif
