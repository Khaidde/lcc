#include "scope.hpp"

#include "diagnostics.hpp"

namespace lcc {

ScopeStack *scope_init() {
    ScopeStack *table = mem::malloc<ScopeStack>();
    table->size = 0;
    return table;
}

void scope_enter(ScopeStack *stack) {
    assert(stack->size < kMaxScopeDepth && "Max scope depth exceeded: 8");
    stack->scopes[stack->size++].init();
}

void scope_exit(ScopeStack *stack) { mem::c_free(stack->scopes[--stack->size].table); }

TableEntry *scope_bind(ScopeStack *stack, TableEntry &entry) {
    assert(entry.decl->data.decl.lval->type == NodeType::kName && "Lvalue of decl should be a name");
    if (TableEntry *other =
            stack->scopes[stack->size - 1].try_put(entry.decl->data.decl.lval->data.name.ident, entry)) {
        return other;
    } else {
        return nullptr;
    }
}

Node *scope_lookup(ScopeStack *stack, LStringView &symbol) {
    for (int i = stack->size - 1; i >= 0; i--) {
        if (TableEntry *entry = stack->scopes[i].get(symbol)) {
            return entry->decl;
        }
    }
    return nullptr;
}

}  // namespace lcc
