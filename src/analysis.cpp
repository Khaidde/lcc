#include "analysis.hpp"

#include "diagnostics.hpp"
#include "hashmap.hpp"

namespace lcc {

namespace {

// TODO: make kMaxScopeDepth a command line argument
constexpr size_t kMaxScopeDepth = 8;

struct SymbolTable {
    size_t size;
    LMap<LStringView, Node *, lstr_hash, lstr_equal> scopes[kMaxScopeDepth];
};

SymbolTable *scope_init() {
    SymbolTable *table = mem::malloc<SymbolTable>();
    table->size = 0;
    return table;
}

bool scope_enter(SymbolTable *table) {
    if (table->size >= kMaxScopeDepth) {
        err("Max scope depth exceeded: 8\n");
        return true;
    }
    table->scopes[table->size++].init();
    return true;
}

void scope_exit(SymbolTable *table) { mem::c_free(table->scopes[--table->size].table); }

Node *scope_bind(SymbolTable *table, LStringView &symbol, Node *decl) {
    if (Node **other = table->scopes[table->size - 1].try_put(symbol, decl)) {
        return *other;
    } else {
        return nullptr;
    }
}

Node *scope_lookup(SymbolTable *table, LStringView &symbol) {
    for (int i = table->size - 1; i >= 0; i--) {
        if (Node **decl = table->scopes[i].get(symbol)) {
            return *decl;
        }
    }
    return nullptr;
}

struct Context {
    SymbolTable *table;
    LString *src;
};

bool analyze_decl(Context *ctx, Node *decl);

namespace builtin {
Type u16 = {TypeKind::kBase, {{"u16", 3}}};

}  // namespace builtin

bool is_type_equal(Type *t1, Type *t2) {
    if (!t1 && !t2) return true;
    if (!t1 || !t2) return false;
    if (t1->kind != t2->kind) return false;

    switch (t1->kind) {
        case TypeKind::kBase: return lstr_equal(t1->data.base.name, t2->data.base.name);
        case TypeKind::kPtr: return is_type_equal(t1->data.ptr.inner, t2->data.ptr.inner);
        case TypeKind::kFuncTy:
            if (t1->data.func.argTys.size != t2->data.func.argTys.size) return false;
            if (!is_type_equal(t1->data.func.retTy, t2->data.func.retTy)) return false;
            for (size_t i = 0; i < t1->data.func.argTys.size; i++) {
                if (!is_type_equal(t1->data.func.argTys.data[i], t2->data.func.argTys.data[i])) {
                    return false;
                }
            }
            return true;
    }
}

Type *create_type(TypeKind kind) {
    Type *type = mem::malloc<Type>();
    type->kind = kind;
    return type;
}

Type *resolve_type(Context *ctx, Node *expr);

Type *resolve_prefix(Context *ctx, Node *prefix) {
    assert(prefix->type == NodeType::kPrefix);

    Type *inner = resolve_type(ctx, prefix->data.prefix.inner);
    if (!inner) return nullptr;

    switch (prefix->data.prefix.op) {
        case TokenType::kSubNeg:
            if (is_type_equal(inner, &builtin::u16)) {
                return inner;
            }
            dx_err(ctx->src, at_node(ctx->src, prefix->data.prefix.inner),
                   "Argument type of negation '-' expected be an integer: Found '%s'\n", type_string(inner));
            return nullptr;
        case TokenType::kSubSub:
            if (inner->kind == TypeKind::kPtr || is_type_equal(inner, &builtin::u16)) {
                return inner;
            }
            dx_err(ctx->src, at_node(ctx->src, prefix->data.prefix.inner),
                   "Argument type of pre-increment '--' expected to be numeric: Found '%s'\n", type_string(inner));
            return nullptr;
        case TokenType::kPtr: {
            Type *ptr = create_type(TypeKind::kPtr);
            ptr->data.ptr.inner = inner;
            return ptr;
        }
        case TokenType::kDeref:
            if (inner->kind == TypeKind::kPtr) {
                return inner->data.ptr.inner;
            }
            dx_err(ctx->src, at_node(ctx->src, prefix->data.prefix.inner),
                   "Argument type of deref '@' expected to be a pointer: Found '%s'\n", type_string(inner));
            return nullptr;
        default:
            todo("Cannot resolve prefix with operator %s\n", token_type_string(prefix->data.prefix.op));
            assert(false);
    }
    return nullptr;
}

Type *resolve_type(Context *ctx, Node *expr) {
    switch (expr->type) {
        case NodeType::kIntLit: return &builtin::u16;
        case NodeType::kName:
            if (Node *decl = scope_lookup(ctx->table, expr->data.name.ident)) {
                if (analyze_decl(ctx, decl)) return nullptr;
                return decl->data.decl.resolvedTy;
            } else {
                printf("what\n");
                dx_err(ctx->src, at_node(ctx->src, expr), "No definition found for '%s'\n",
                       lstr_create(expr->data.name.ident).data);
                return nullptr;
            }
        case NodeType::kPrefix: return resolve_prefix(ctx, expr);
        case NodeType::kInfix: {
            Type *ltype = resolve_type(ctx, expr->data.infix.left);
            if (!ltype) return nullptr;

            Type *rtype = resolve_type(ctx, expr->data.infix.right);
            if (!rtype) return nullptr;

            if (!is_type_equal(ltype, rtype)) {
                dx_err(ctx->src, at_node(ctx->src, expr),
                       "Mismatched infix operator types: left is '%s', right is '%s'\n", type_string(ltype),
                       type_string(rtype));
                return nullptr;
            }
            return ltype;
        }
        default: todo("Cannot resolve type of node %s\n", node_type_string(expr->type)); assert(false);
    }
    return nullptr;
}

bool analyze_decl(Context *ctx, Node *decl) {
    assert(decl->type == NodeType::kDecl);
    if (decl->data.decl.resolvedTy) return false;

    if (decl->data.decl.isChecked) {
        dx_err(ctx->src, at_node(ctx->src, decl), "Detected circular declaration\n");
        return true;
    }
    decl->data.decl.isChecked = true;

    bool hasTy = decl->data.decl.staticTy;
    bool hasRval = decl->data.decl.rval;
    if (hasRval) {
        decl->data.decl.resolvedTy = resolve_type(ctx, decl->data.decl.rval);
        if (!decl->data.decl.resolvedTy) return true;
    }
    if (hasRval && hasTy) {
        Type *staticTy = &decl->data.decl.staticTy->data.type;
        if (!is_type_equal(staticTy, decl->data.decl.resolvedTy)) {
            dx_err(ctx->src, at_node(ctx->src, decl),
                   "Mismatched declaration types: declared type is '%s', expression type is '%s'\n",
                   type_string(staticTy), type_string(decl->data.decl.resolvedTy));
            return true;
        }
    } else if (hasTy) {
        decl->data.decl.resolvedTy = &decl->data.decl.staticTy->data.type;
    }
    return false;
}

}  // namespace

bool analyze_unit(Node *unit) {
    Context ctx;
    ctx.src = unit->data.unit.src;
    ctx.table = scope_init();
    scope_enter(ctx.table);

    for (size_t i = 0; i < unit->data.unit.decls.size; i++) {
        Node *decl = unit->data.unit.decls.data[i];
        if (decl->data.decl.lval->type != NodeType::kName) {
            dx_err(ctx.src, at_node(ctx.src, decl->data.decl.lval),
                   "Must only declare variable names in global scope\n");
            scope_exit(ctx.table);
            return true;
        }
        scope_bind(ctx.table, decl->data.decl.lval->data.name.ident, decl);
        decl->data.decl.isDecl = true;
    }

    LStringView main{"main", 4};
    if (!scope_lookup(ctx.table, main)) {
        err("Could not find main function in global scope\n");
        scope_exit(ctx.table);
        return true;
    }

    for (size_t i = 0; i < unit->data.unit.decls.size; i++) {
        Node *decl = unit->data.unit.decls.data[i];
        if (analyze_decl(&ctx, decl)) {
            scope_exit(ctx.table);
            return true;
        }
    }

    scope_exit(ctx.table);
    return false;
}

};  // namespace lcc
