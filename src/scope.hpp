#ifndef LCC_SCOPE_HPP
#define LCC_SCOPE_HPP

#include <cstdio>

#include "ast.hpp"
#include "list.hpp"
#include "map.hpp"

namespace lcc {

struct Scope {
    LMap<LStringView, Node *, lstr_hash, lstr_equal> decls;
    bool isClosed;  // Can only access global declarations
};

struct ScopeStack {
    size_t size;
    LList<Scope *> scopes;
};

ScopeStack *scope_init();

void scope_enter(ScopeStack *stack, bool isClosed);

void scope_exit(ScopeStack *stack);

Node *scope_bind(ScopeStack *stack, Node *decl);

}  // namespace lcc

#endif
