#include "scope.hpp"

namespace lcc {

ScopeStack *scope_init() {
    ScopeStack *table = mem::malloc<ScopeStack>();
    table->size = 0;
    table->scopes = {};
    return table;
}

size_t scope_depth(ScopeStack *stack) {
    assert(stack->size > 0);
    return stack->size - 1;
}

void scope_enter(ScopeStack *stack, Node *owner) {
    assert(owner);
    Scope *scope;
    if (stack->size >= stack->scopes.size) {
        scope = mem::malloc<Scope>();
        stack->scopes.add(scope);
    } else {
        scope = stack->scopes.data[stack->size];
    }
    scope->decls.init();
    scope->owner = owner;
    stack->size++;
}

Node *scope_bind(ScopeStack *stack, Node *decl) {
    assert(decl->decl.lval->kind == NodeKind::kName && "LValue of decl should be a name");
    Scope *scope = scope_get(stack);
    if (Node **other = scope->decls.try_put(decl->decl.lval->name.ident, decl)) {
        return *other;
    } else {
        return nullptr;
    }
}

Scope *scope_get(ScopeStack *stack) { return stack->scopes.get(stack->size - 1); }

void scope_exit(ScopeStack *stack) {
    assert(stack->size);
    mem::c_free(stack->scopes.get(--stack->size)->decls.table);
}

}  // namespace lcc
