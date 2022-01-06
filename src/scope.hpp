#ifndef LCC_SCOPE_HPP
#define LCC_SCOPE_HPP

#include <cstdio>

#include "ast.hpp"
#include "list.hpp"
#include "map.hpp"

namespace lcc {

struct Scope {
    Node *declListHead;
    LMap<LStringView, Node *, lstr_hash, lstr_equal> decls;

    Node *owner;
};

struct ScopeStack {
    size_t size;
    LList<Scope *> scopes;
};

ScopeStack *scope_init();

size_t scope_depth(ScopeStack *stack);

void scope_enter(ScopeStack *stack, Node *owner);

Node *scope_bind(ScopeStack *stack, Node *decl);

Scope *scope_get(ScopeStack *stack);

void scope_exit(ScopeStack *stack);

}  // namespace lcc

#endif
