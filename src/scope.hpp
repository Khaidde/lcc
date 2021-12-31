#ifndef LCC_SCOPE_HPP
#define LCC_SCOPE_HPP

#include <cstdio>

#include "ast.hpp"
#include "map.hpp"

namespace lcc {

struct Scope {
    LMap<LStringView, Node *, lstr_hash, lstr_equal> decls;
    bool isClosed;  // Can only access global declarations
};

constexpr size_t kMaxScopeDepth = 8;

struct ScopeStack {
    size_t size;
    Scope scopes[kMaxScopeDepth];
};

ScopeStack *scope_init();

void scope_enter(ScopeStack *stack, bool isClosed);

void scope_exit(ScopeStack *stack);

Node *scope_bind(ScopeStack *stack, Node *decl);

}  // namespace lcc

#endif
