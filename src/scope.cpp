#include "scope.hpp"

namespace lcc {

ScopeStack *scope_init() {
    ScopeStack *table = mem::malloc<ScopeStack>();
    table->size = 0;
    return table;
}

void scope_enter(ScopeStack *stack, bool isClosed) {
    assert(stack->size < kMaxScopeDepth && "Max scope depth exceeded: 8");
    Scope &scope = stack->scopes[stack->size++];
    scope.decls.init();
    scope.isClosed = isClosed;
}

void scope_exit(ScopeStack *stack) { mem::c_free(stack->scopes[--stack->size].decls.table); }

Node *scope_bind(ScopeStack *stack, Node *decl) {
    assert(stack);
    assert(decl->decl.lval->kind == NodeKind::kName && "Lvalue of decl should be a name");
    if (Node **other = stack->scopes[stack->size - 1].decls.try_put(decl->decl.lval->name.ident, decl)) {
        return *other;
    } else {
        return nullptr;
    }
}

}  // namespace lcc
