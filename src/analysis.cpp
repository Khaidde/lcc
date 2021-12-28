#include "analysis.hpp"

#include "diagnostics.hpp"
#include "file.hpp"
#include "hashmap.hpp"
#include "print.hpp"

namespace lcc {

namespace {

bool resolve_decl_type(CompilationContext *cmp, Node *decl);

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

Type *resolve_type(CompilationContext *cmp, Node *expr);

Type *resolve_prefix(CompilationContext *cmp, Node *prefix) {
    assert(prefix->type == NodeType::kPrefix);

    Type *inner = resolve_type(cmp, prefix->data.prefix.inner);
    if (!inner) return nullptr;

    switch (prefix->data.prefix.op) {
        case TokenType::kSubNeg:
            if (is_type_equal(inner, &builtin_type::u16)) {
                return inner;
            }
            dx_err(at_node(cmp->ctx.currFile->finfo, prefix->data.prefix.inner),
                   "Argument type of negation '-' expected be an integer: Found '%s'\n", type_string(inner));
            return nullptr;
        case TokenType::kSubSub:
            if (inner->kind == TypeKind::kPtr || is_type_equal(inner, &builtin_type::u16)) {
                return inner;
            }
            dx_err(at_node(cmp->ctx.currFile->finfo, prefix->data.prefix.inner),
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
            dx_err(at_node(cmp->ctx.currFile->finfo, prefix->data.prefix.inner),
                   "Argument type of deref '@' expected to be a pointer: Found '%s'\n", type_string(inner));
            return nullptr;
        default:
            todo("Cannot resolve prefix with operator %s\n", token_type_string(prefix->data.prefix.op));
            assert(false);
    }
    return nullptr;
}

Type *resolve_type(CompilationContext *cmp, Node *expr) {
    switch (expr->type) {
        case NodeType::kIntLit: return &builtin_type::u16;
        case NodeType::kName:
            if (Node *decl = scope_lookup(&cmp->ctx, expr->data.name.ident)) {
                if (resolve_decl_type(cmp, decl)) return nullptr;
                return decl->data.decl.resolvedTy;
            } else if (DeclContext *declCtx = scope_lookup_global(cmp, expr->data.name.ident)) {
                Context save = cmp->ctx;
                cmp->ctx.currPackage = declCtx->package;
                cmp->ctx.currFile = declCtx->fileUnit;
                cmp->ctx.currScopeStack = scope_init();
                if (resolve_decl_type(cmp, declCtx->decl)) return nullptr;
                cmp->ctx = save;
                return declCtx->decl->data.decl.resolvedTy;
            } else {
                dx_err(at_node(cmp->ctx.currFile->finfo, expr), "No definition found for '%s'\n",
                       lstr_create(expr->data.name.ident).data);
                return nullptr;
            }
        case NodeType::kPrefix: return resolve_prefix(cmp, expr);
        case NodeType::kInfix: {
            Type *ltype = resolve_type(cmp, expr->data.infix.left);
            if (!ltype) return nullptr;
            if (ltype->kind == TypeKind::kNone) {
                dx_err(at_node(cmp->ctx.currFile->finfo, expr->data.infix.left),
                       "Cannot use expression of type 'none' in infix operation\n");
                return nullptr;
            }

            Type *rtype = resolve_type(cmp, expr->data.infix.right);
            if (!rtype) return nullptr;
            if (rtype->kind == TypeKind::kNone) {
                dx_err(at_node(cmp->ctx.currFile->finfo, expr->data.infix.right),
                       "Cannot use expression of type 'none' in infix operation\n");
                return nullptr;
            }

            if (!is_type_equal(ltype, rtype)) {
                dx_err(at_node(cmp->ctx.currFile->finfo, expr),
                       "Mismatched infix operator types: left is '%s', right is '%s'\n", type_string(ltype),
                       type_string(rtype));
                return nullptr;
            }

            if (!is_type_equal(ltype, &builtin_type::u16)) {
                dx_err(at_node(cmp->ctx.currFile->finfo, expr),
                       "Argument types of infix operator expected to be integers: Found '%s'\n", type_string(ltype));
                return nullptr;
            }
            return ltype;
        }
        case NodeType::kCall: {
            Type *ctype = resolve_type(cmp, expr->data.call.callee);
            if (!ctype) return nullptr;
            if (ctype->kind != TypeKind::kFuncTy) {
                dx_err(at_node(cmp->ctx.currFile->finfo, expr->data.call.callee),
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
                    dx_err(at_node(cmp->ctx.currFile->finfo, decl),
                           "Function parameter must explicitly specify a type\n");
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
                scope_enter(cmp->ctx.currScopeStack);
                for (size_t i = 0; i < expr->data.func.params.size; i++) {
                    scope_bind(cmp->ctx.currScopeStack, expr->data.func.params.get(i));
                }
                funcTy->data.func.retTy = resolve_type(cmp, expr->data.func.body);
                if (!funcTy->data.func.retTy) return nullptr;
                scope_exit(cmp->ctx.currScopeStack);
            }
            return funcTy;
        }
        default: todo("Cannot resolve type of node %s\n", node_type_string(expr->type)); assert(false);
    }
    return nullptr;
}

bool resolve_decl_type(CompilationContext *cmp, Node *decl) {
    assert(decl->type == NodeType::kDecl);
    if (decl->data.decl.resolvedTy) return false;

    if (decl->data.decl.isChecked) {
        dx_err(at_node(cmp->ctx.currFile->finfo, decl), "Detected circular dependency\n");
        return true;
    }
    decl->data.decl.isChecked = true;

    bool hasTy = decl->data.decl.staticTy;
    bool hasRval = decl->data.decl.rval;
    if (hasRval) {
        decl->data.decl.resolvedTy = resolve_type(cmp, decl->data.decl.rval);
        if (!decl->data.decl.resolvedTy) return true;
        if (decl->data.decl.resolvedTy->kind == TypeKind::kNone) {
            dx_err(at_node(cmp->ctx.currFile->finfo, decl->data.decl.rval),
                   "Cannot assign value of type 'none' to variable\n");
            return true;
        }
    }
    if (hasRval && hasTy) {
        Type *staticTy = &decl->data.decl.staticTy->data.type;
        if (!is_type_equal(staticTy, decl->data.decl.resolvedTy)) {
            dx_err(at_node(cmp->ctx.currFile->finfo, decl),
                   "Mismatched declaration types: declared type is '%s', value type is '%s'\n", type_string(staticTy),
                   type_string(decl->data.decl.resolvedTy));
            return true;
        }
    } else if (hasTy) {
        decl->data.decl.resolvedTy = &decl->data.decl.staticTy->data.type;
    }
    return false;
}

bool analyze_decl(CompilationContext *cmp, Node *decl) {
    if (resolve_decl_type(cmp, decl)) return true;

    if (decl->data.decl.rval) {
        switch (decl->data.decl.rval->type) {
            case NodeType::kIntLit: break;
            default: todo("Implement declaration rvalue analyze\n");
        }
    }
    return false;
}

}  // namespace

AnalysisResult analyze_file(CompilationContext *cmp) {
    Node *unit = cmp->ctx.currFile->unit;
    for (size_t i = 0; i < unit->data.unit.decls.size; i++) {
        Node *decl = unit->data.unit.decls.get(i);
        if (analyze_decl(cmp, decl)) return AnalysisResult::kFailure;
    }
    return AnalysisResult::kSuccess;
}

};  // namespace lcc
