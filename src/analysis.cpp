#include "analysis.hpp"

#include "diagnostics.hpp"
#include "hashmap.hpp"

namespace lcc {

namespace {

struct TableEntry {
    LString *src;
    Node *decl;
};

// TODO: make kMaxScopeDepth a command line argument
constexpr size_t kMaxScopeDepth = 8;

struct SymbolTable {
    size_t size;
    LMap<LStringView, TableEntry, lstr_hash, lstr_equal> scopes[kMaxScopeDepth];
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

TableEntry *scope_bind(SymbolTable *table, TableEntry &entry) {
    assert(entry.decl->data.decl.lval->type == NodeType::kName && "Lvalue of decl should be a name");
    if (TableEntry *other =
            table->scopes[table->size - 1].try_put(entry.decl->data.decl.lval->data.name.ident, entry)) {
        return other;
    } else {
        return nullptr;
    }
}

Node *scope_lookup(SymbolTable *table, LStringView &symbol) {
    for (int i = table->size - 1; i >= 0; i--) {
        if (TableEntry *entry = table->scopes[i].get(symbol)) {
            return entry->decl;
        }
    }
    return nullptr;
}

struct Context {
    SymbolTable *table;
    LString *src;
};

bool resolve_decl_type(Context *ctx, Node *decl);

bool is_type_equal(Type *t1, Type *t2) {
    if (!t1 && !t2) return true;
    if (!t1 || !t2) return false;
    if (t1->kind != t2->kind) return false;

    switch (t1->kind) {
        case TypeKind::kNone: return true;
        case TypeKind::kBase: return t1->data.base.kind == t2->data.base.kind;
        case TypeKind::kPtr: return is_type_equal(t1->data.ptr.inner, t2->data.ptr.inner);
        case TypeKind::kFuncTy:
            if (t1->data.func.paramTys.size != t2->data.func.paramTys.size) return false;
            if (!is_type_equal(t1->data.func.retTy, t2->data.func.retTy)) return false;
            for (size_t i = 0; i < t1->data.func.paramTys.size; i++) {
                if (!is_type_equal(t1->data.func.paramTys.get(i), t2->data.func.paramTys.get(i))) {
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
            if (is_type_equal(inner, &builtin_type::u16)) {
                return inner;
            }
            dx_err(ctx->src, at_node(ctx->src, prefix->data.prefix.inner),
                   "Argument type of negation '-' expected be an integer: Found '%s'\n", type_string(inner));
            return nullptr;
        case TokenType::kSubSub:
            if (inner->kind == TypeKind::kPtr || is_type_equal(inner, &builtin_type::u16)) {
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
        case NodeType::kIntLit: return &builtin_type::u16;
        case NodeType::kName:
            if (Node *decl = scope_lookup(ctx->table, expr->data.name.ident)) {
                if (resolve_decl_type(ctx, decl)) return nullptr;
                return decl->data.decl.resolvedTy;
            } else {
                dx_err(ctx->src, at_node(ctx->src, expr), "No definition found for '%s'\n",
                       lstr_create(expr->data.name.ident).data);
                return nullptr;
            }
        case NodeType::kPrefix: return resolve_prefix(ctx, expr);
        case NodeType::kInfix: {
            Type *ltype = resolve_type(ctx, expr->data.infix.left);
            if (!ltype) return nullptr;
            if (ltype->kind == TypeKind::kNone) {
                dx_err(ctx->src, at_node(ctx->src, expr->data.infix.left),
                       "Cannot use expression of type 'none' in infix operation\n");
                return nullptr;
            }

            Type *rtype = resolve_type(ctx, expr->data.infix.right);
            if (!rtype) return nullptr;
            if (rtype->kind == TypeKind::kNone) {
                dx_err(ctx->src, at_node(ctx->src, expr->data.infix.right),
                       "Cannot use expression of type 'none' in infix operation\n");
                return nullptr;
            }

            if (!is_type_equal(ltype, rtype)) {
                dx_err(ctx->src, at_node(ctx->src, expr),
                       "Mismatched infix operator types: left is '%s', right is '%s'\n", type_string(ltype),
                       type_string(rtype));
                return nullptr;
            }

            if (!is_type_equal(ltype, &builtin_type::u16)) {
                dx_err(ctx->src, at_node(ctx->src, expr),
                       "Argument types of infix operator expected to be integers: Found '%s'\n", type_string(ltype));
                return nullptr;
            }
            return ltype;
        }
        case NodeType::kCall: {
            Type *ctype = resolve_type(ctx, expr->data.call.callee);
            if (!ctype) return nullptr;
            if (ctype->kind != TypeKind::kFuncTy) {
                dx_err(ctx->src, at_node(ctx->src, expr->data.call.callee),
                       "Expression has type '%s' which cannot be called\n", type_string(ctype));
                return nullptr;
            }
            return ctype->data.func.retTy;
        }
        case NodeType::kFunc: {
            Type *funcTy = create_type(TypeKind::kFuncTy);
            funcTy->data.func.paramTys = {};
            if (expr->data.func.params.size) funcTy->data.func.paramTys.init(expr->data.func.params.size);
            for (size_t i = 0; i < expr->data.func.params.size; i++) {
                Node *decl = expr->data.func.params.get(i);
                if (!decl->data.decl.staticTy) {
                    dx_err(ctx->src, at_node(ctx->src, decl), "Function parameter must explicitly specify a type\n");
                    return nullptr;
                }
                decl->data.decl.resolvedTy = &decl->data.decl.staticTy->data.type;
                funcTy->data.func.paramTys.add(decl->data.decl.resolvedTy);
            }
            if (expr->data.func.retTy) {
                funcTy->data.func.retTy = &expr->data.func.retTy->data.type;
            } else if (expr->data.func.body->type == NodeType::kBlock) {
                funcTy->data.func.retTy = &builtin_type::none;
            } else {
                // TODO: return type inference for single line function seems hairy
                scope_enter(ctx->table);
                for (size_t i = 0; i < expr->data.func.params.size; i++) {
                    TableEntry entry = {ctx->src, expr->data.func.params.get(i)};
                    scope_bind(ctx->table, entry);
                }
                funcTy->data.func.retTy = resolve_type(ctx, expr->data.func.body);
                if (!funcTy->data.func.retTy) return nullptr;
                scope_exit(ctx->table);
            }
            return funcTy;
        }
        default: todo("Cannot resolve type of node %s\n", node_type_string(expr->type)); assert(false);
    }
    return nullptr;
}

bool resolve_decl_type(Context *ctx, Node *decl) {
    assert(decl->type == NodeType::kDecl);
    if (decl->data.decl.resolvedTy) return false;

    if (decl->data.decl.isChecked) {
        dx_err(ctx->src, at_node(ctx->src, decl), "Detected circular dependency\n");
        return true;
    }
    decl->data.decl.isChecked = true;

    bool hasTy = decl->data.decl.staticTy;
    bool hasRval = decl->data.decl.rval;
    if (hasRval) {
        decl->data.decl.resolvedTy = resolve_type(ctx, decl->data.decl.rval);
        if (!decl->data.decl.resolvedTy) return true;
        if (decl->data.decl.resolvedTy->kind == TypeKind::kNone) {
            dx_err(ctx->src, at_node(ctx->src, decl->data.decl.rval),
                   "Cannot assign value of type 'none' to variable\n");
            return true;
        }
    }
    if (hasRval && hasTy) {
        Type *staticTy = &decl->data.decl.staticTy->data.type;
        if (!is_type_equal(staticTy, decl->data.decl.resolvedTy)) {
            dx_err(ctx->src, at_node(ctx->src, decl),
                   "Mismatched declaration types: declared type is '%s', value type is '%s'\n", type_string(staticTy),
                   type_string(decl->data.decl.resolvedTy));
            return true;
        }
    } else if (hasTy) {
        decl->data.decl.resolvedTy = &decl->data.decl.staticTy->data.type;
    }
    return false;
}

bool analyze_decl(Context *ctx, Node *decl) {
    if (resolve_decl_type(ctx, decl)) return true;

    if (decl->data.decl.rval) {
        switch (decl->data.decl.rval->type) {
            case NodeType::kIntLit: break;
            default: todo("Implement declaration rvalue analyze\n");
        }
    }
    return false;
}

}  // namespace

bool analyze_package(LList<FileInfo *> &files) {
    SymbolTable *globals = scope_init();
    scope_enter(globals);

    for (size_t i = 0; i < files.size; i++) {
        Node *unit = files.get(i)->unit;
        LString *src = &files.get(i)->src;

        for (size_t k = 0; k < unit->data.unit.decls->size; k++) {
            Node *decl = unit->data.unit.decls->get(k);
            if (decl->data.decl.lval->type != NodeType::kName) {
                dx_err(src, at_node(src, decl->data.decl.lval), "Must only declare variable names in global scope\n");
                return true;
            }

            TableEntry entry{src, decl};
            if (TableEntry *other = scope_bind(globals, entry)) {
                dx_err(src, at_node(src, decl->data.decl.lval), "Found duplicate declaration\n");
                dx_err(other->src, at_node(other->src, other->decl->data.decl.lval),
                       "Previous declaration found here\n");
                return true;
            }
        }
    }

    for (size_t i = 0; i < files.size; i++) {
        Node *unit = files.get(i)->unit;
        Context ctx{globals, &files.get(i)->src};

        for (size_t i = 0; i < unit->data.unit.decls->size; i++) {
            Node *decl = unit->data.unit.decls->get(i);
            if (analyze_decl(&ctx, decl)) {
                return true;
            }
        }
    }

    scope_exit(globals);
    return false;
}

};  // namespace lcc
