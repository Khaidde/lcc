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
    Scope *scope = mem::malloc<Scope>();
    scope->decls.init();
    scope->owner = owner;
    if (stack->size >= stack->scopes.size) {
        stack->scopes.add(scope);
    } else {
        stack->scopes.data[stack->size] = scope;
    }
    stack->size++;
}

void scope_exit(ScopeStack *stack) {
    assert(stack->size);
    Scope *scope = stack->scopes.get(--stack->size);
    mem::c_free(scope->decls.table);
}

Node *scope_bind(ScopeStack *stack, Node *decl) {
    assert(decl->decl.lval->kind == NodeKind::kName && "LValue of decl should be a name");
    Scope *scope = stack->scopes.get(stack->size - 1);
    if (Node **other = scope->decls.try_put(decl->decl.lval->name.ident, decl)) {
        return *other;
    } else {
        return nullptr;
    }
}

}  // namespace lcc
