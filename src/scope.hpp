#ifndef LCC_SCOPE_HPP
#define LCC_SCOPE_HPP

#include "astnode.hpp"
#include "hashmap.hpp"
#include "types.hpp"

namespace lcc {

struct ExecutionContext;

struct TableEntry {
    file::FileInfo *finfo;
    Node *decl;
};

constexpr size_t kMaxScopeDepth = 8;

struct ScopeStack {
    size_t size;
    LMap<LStringView, TableEntry, lstr_hash, lstr_equal> scopes[kMaxScopeDepth];
};

ScopeStack *scope_init();

void scope_enter(ScopeStack *stack);

void scope_exit(ScopeStack *stack);

TableEntry *scope_bind(ScopeStack *stack, TableEntry &entry);

Node *scope_lookup_global(ScopeStack *stack, LStringView &symbol);

Node *scope_lookup(ExecutionContext *ctx, LStringView &symbol);

}  // namespace lcc

#endif
