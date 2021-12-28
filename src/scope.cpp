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
    return stack->scopes[stack->size - 1].try_put(entry.decl->data.decl.lval->data.name.ident, entry);
}

Node *scope_lookup_global(ScopeStack *stack, LStringView &symbol) {
    if (TableEntry *entry = stack->scopes[0].get(symbol)) {
        // TODO: determine whether function should return TableEntry instead
        return entry->decl;
    } else {
        return nullptr;
    }
}

Node *scope_lookup(ExecutionContext *ctx, LStringView &symbol) {
    for (int i = ctx->currScopeStack->size - 1; i >= 0; i--) {
        if (TableEntry *entry = ctx->currScopeStack->scopes[i].get(symbol)) {
            return entry->decl;
        }
    }
    for (size_t i = 0; i < ctx->currFile->unit->data.unit.imports.size; i++) {
        LStringView &importName = ctx->currFile->unit->data.unit.imports.get(i);
        if (Package *pkg = ctx->packageMap.get(importName)) {
            return scope_lookup_global(pkg->scopeStack, symbol);
        } else {
            assert(false && "Should only ever do scope_lookup if all imports have been resolved for a file");
        }
    }
    return nullptr;
}

}  // namespace lcc
