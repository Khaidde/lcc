#include "analysis.hpp"

#include "diagnostic.hpp"
#include "print.hpp"

namespace lcc {

namespace {

AnalysisResult resolve_decl_type(CompilationContext *cmp, Node *decl);

bool is_type_equal(Type *t1, Type *t2) {
    if (!t1 && !t2) return true;
    if (!t1 || !t2) return false;
    if (t1->kind != t2->kind) return false;

    switch (t1->kind) {
        case TypeKind::kNone: return true;
        case TypeKind::kBase: return t1->base.kind == t2->base.kind;
        case TypeKind::kPtr: return is_type_equal(t1->ptr.inner, t2->ptr.inner);
        case TypeKind::kFuncTy:
            if (t1->func.paramTys.size != t2->func.paramTys.size) return false;
            if (!is_type_equal(t1->func.retTy, t2->func.retTy)) return false;
            for (size_t i = 0; i < t1->func.paramTys.size; i++) {
                if (!is_type_equal(t1->func.paramTys.get(i), t2->func.paramTys.get(i))) {
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

Node *expand_name(CompilationContext *cmp, Node *nameRef) {
    assert(nameRef->kind == NodeKind::kName);
    if (Node *declOrImport = decl_lookup(cmp, nameRef->name.ident)) return declOrImport;
    dx_err(at_node(cmp->ctx.file->finfo, nameRef), "No definition found for '%s'\n", lstr_raw_str(nameRef->name.ident));
    return nullptr;
}

Node *expand_dot_access(CompilationContext *cmp, Node *dotAccessRef) {
    assert(dotAccessRef->kind == NodeKind::kInfix && dotAccessRef->infix.op == TokenType::kDot);

    Node *baseRef;
    switch (dotAccessRef->infix.left->kind) {
        case NodeKind::kName: baseRef = expand_name(cmp, dotAccessRef->infix.left); break;
        case NodeKind::kInfix:
            if (dotAccessRef->infix.op == TokenType::kDot) {
                baseRef = expand_dot_access(cmp, dotAccessRef->infix.left);
                break;
            } else {
                dx_err(at_node(cmp->ctx.file->finfo, dotAccessRef->infix.left), "Cannot expand infix operation '%s'\n",
                       token_type_string(dotAccessRef->infix.left->infix.op));
                return nullptr;
            }
        default:
            dx_err(at_node(cmp->ctx.file->finfo, dotAccessRef->infix.left),
                   "Cannot expand left side of dot access with kind %s\n",
                   node_kind_string(dotAccessRef->infix.left->kind));
            return nullptr;
    }
    if (!baseRef) return nullptr;

    if (dotAccessRef->infix.right->kind != NodeKind::kName) {
        dx_err(at_node(cmp->ctx.file->finfo, dotAccessRef->infix.right), "Right side of dot access must be a name\n");
        return nullptr;
    }
    if (baseRef->kind == NodeKind::kDecl) {
        todo("Dot access into declaration is not yet supported. See 'struct's when they're implemeneted :P\n");
        return nullptr;
    } else if (baseRef->kind == NodeKind::kImport) {
        if (Package **pkg = cmp->packageMap.get(baseRef->import.package)) {
            if (Node **decl = (*pkg)->globalDecls.get(dotAccessRef->infix.right->name.ident)) {
                return *decl;
            } else {
                dx_err(at_node(cmp->ctx.file->finfo, dotAccessRef->infix.right),
                       "Could not find declaration in package '%s'\n", lstr_raw_str(baseRef->import.package));
                return nullptr;
            }
        }
    }
    assert(false && "Import/packages should have been resolved before analysis\n");
    return nullptr;
}

Type *resolve_expansion(CompilationContext *cmp, Node *original, Node *expansion) {
    if (expansion->kind == NodeKind::kImport) {
        dx_err(at_node(cmp->ctx.file->finfo, original), "Package alias cannot be used as a variable\n");
        return nullptr;
    }
    assert(expansion->kind == NodeKind::kDecl);
    Context save = cmp->ctx;
    cmp->ctx.package = expansion->decl.package;
    cmp->ctx.file = expansion->decl.file;
    if (resolve_decl_type(cmp, expansion)) return nullptr;
    cmp->ctx = save;
    return expansion->decl.resolvedTy;
}

Type *resolve_type(CompilationContext *cmp, Node *expr);

Type *resolve_prefix(CompilationContext *cmp, Node *prefix) {
    assert(prefix->kind == NodeKind::kPrefix);

    Type *inner = resolve_type(cmp, prefix->prefix.inner);
    if (!inner) return nullptr;

    switch (prefix->prefix.op) {
        case TokenType::kSubNeg:
            if (is_type_equal(inner, &builtin_type::u16)) {
                return inner;
            }
            dx_err(at_node(cmp->ctx.file->finfo, prefix->prefix.inner),
                   "Argument type of negation '-' expected be an integer: Found '%s'\n", type_string(inner));
            return nullptr;
        case TokenType::kSubSub:
            if (inner->kind == TypeKind::kPtr || is_type_equal(inner, &builtin_type::u16)) {
                return inner;
            }
            dx_err(at_node(cmp->ctx.file->finfo, prefix->prefix.inner),
                   "Argument type of pre-increment '--' expected to be numeric: Found '%s'\n", type_string(inner));
            return nullptr;
        case TokenType::kPtr: {
            Type *ptr = create_type(TypeKind::kPtr);
            ptr->ptr.inner = inner;
            return ptr;
        }
        case TokenType::kDeref:
            if (inner->kind == TypeKind::kPtr) {
                return inner->ptr.inner;
            }
            dx_err(at_node(cmp->ctx.file->finfo, prefix->prefix.inner),
                   "Argument type of deref '@' expected to be a pointer: Found '%s'\n", type_string(inner));
            return nullptr;
        default: todo("Cannot resolve prefix with operator %s\n", token_type_string(prefix->prefix.op)); assert(false);
    }
    return nullptr;
}

Type *resolve_infix(CompilationContext *cmp, Node *infix) {
    if (infix->infix.op == TokenType::kDot) {
        if (Node *decl = expand_dot_access(cmp, infix)) {
            return resolve_expansion(cmp, infix, decl);
        }
        return nullptr;
    }

    Type *ltype = resolve_type(cmp, infix->infix.left);
    if (!ltype) return nullptr;
    if (ltype->kind == TypeKind::kNone) {
        dx_err(at_node(cmp->ctx.file->finfo, infix->infix.left),
               "Cannot use expression of type 'none' in infix operation\n");
        return nullptr;
    }

    Type *rtype = resolve_type(cmp, infix->infix.right);
    if (!rtype) return nullptr;
    if (rtype->kind == TypeKind::kNone) {
        dx_err(at_node(cmp->ctx.file->finfo, infix->infix.right),
               "Cannot use expression of type 'none' in infix operation\n");
        return nullptr;
    }

    if (!is_type_equal(ltype, rtype)) {
        dx_err(at_node(cmp->ctx.file->finfo, infix), "Mismatched infix operator types: left is '%s', right is '%s'\n",
               type_string(ltype), type_string(rtype));
        return nullptr;
    }

    if (!is_type_equal(ltype, &builtin_type::u16)) {
        dx_err(at_node(cmp->ctx.file->finfo, infix),
               "Argument types of infix operator expected to be integers: Found '%s'\n", type_string(ltype));
        return nullptr;
    }
    return ltype;
}

Type *resolve_type(CompilationContext *cmp, Node *expr) {
    switch (expr->kind) {
        case NodeKind::kIntLit: return &builtin_type::u16;
        case NodeKind::kStrLit: return &builtin_type::string;
        case NodeKind::kName:
            if (Node *decl = expand_name(cmp, expr)) {
                return resolve_expansion(cmp, expr, decl);
            }
            return nullptr;
        case NodeKind::kPrefix: return resolve_prefix(cmp, expr);
        case NodeKind::kInfix: return resolve_infix(cmp, expr);
        case NodeKind::kCall: {
            Type *ctype = resolve_type(cmp, expr->call.callee);
            if (!ctype) return nullptr;
            if (ctype->kind != TypeKind::kFuncTy) {
                dx_err(at_node(cmp->ctx.file->finfo, expr->call.callee),
                       "Expression has type '%s' which cannot be called\n", type_string(ctype));
                return nullptr;
            }
            return ctype->func.retTy;
        }
        case NodeKind::kFunc: {
            Type *funcTy = create_type(TypeKind::kFuncTy);
            funcTy->func.paramTys = {};
            if (expr->func.params.size) funcTy->func.paramTys.init(expr->func.params.size);
            for (size_t i = 0; i < expr->func.params.size; i++) {
                Node *decl = expr->func.params.get(i);
                if (!decl->decl.staticTy) {
                    dx_err(at_node(cmp->ctx.file->finfo, decl), "Function parameter must explicitly specify a type\n");
                    return nullptr;
                }
                decl->decl.resolvedTy = &decl->decl.staticTy->type;
                funcTy->func.paramTys.add(decl->decl.resolvedTy);
            }
            if (expr->func.retTy) {
                funcTy->func.retTy = &expr->func.retTy->type;
            } else if (expr->func.body->kind == NodeKind::kBlock) {
                funcTy->func.retTy = &builtin_type::none;
            } else {
                // TODO: return type inference for single line function seems hairy
                if (!cmp->ctx.file->scopeStack) {
                    cmp->ctx.file->scopeStack = scope_init();
                }
                scope_enter(cmp->ctx.file->scopeStack);
                for (size_t i = 0; i < expr->func.params.size; i++) {
                    scope_bind(cmp->ctx.file->scopeStack, expr->func.params.get(i));
                }
                funcTy->func.retTy = resolve_type(cmp, expr->func.body);
                if (!funcTy->func.retTy) return nullptr;
                scope_exit(cmp->ctx.file->scopeStack);
            }
            return funcTy;
        }
        default: todo("Cannot resolve type of node %s\n", node_kind_string(expr->kind)); assert(false);
    }
    return nullptr;
}

AnalysisResult resolve_decl_type(CompilationContext *cmp, Node *decl) {
    assert(decl->kind == NodeKind::kDecl);
    if (decl->decl.resolvedTy) return AnalysisResult::kSuccess;

    if (decl->decl.isChecked) {
        dx_err(at_node(cmp->ctx.file->finfo, decl), "Detected circular dependency\n");
        return AnalysisResult::kFailure;
    }
    decl->decl.isChecked = true;

    bool hasTy = decl->decl.staticTy;
    bool hasRval = decl->decl.rval;
    if (hasRval) {
        decl->decl.resolvedTy = resolve_type(cmp, decl->decl.rval);
        if (!decl->decl.resolvedTy) return AnalysisResult::kFailure;
        if (decl->decl.resolvedTy->kind == TypeKind::kNone) {
            dx_err(at_node(cmp->ctx.file->finfo, decl->decl.rval), "Cannot assign value of type 'none' to variable\n");
            return AnalysisResult::kFailure;
        }
    }
    if (hasRval && hasTy) {
        Type *staticTy = &decl->decl.staticTy->type;
        if (!is_type_equal(staticTy, decl->decl.resolvedTy)) {
            dx_err(at_node(cmp->ctx.file->finfo, decl),
                   "Mismatched declaration types: declared type is '%s', value type is '%s'\n", type_string(staticTy),
                   type_string(decl->decl.resolvedTy));
            return AnalysisResult::kFailure;
        }
    } else if (hasTy) {
        decl->decl.resolvedTy = &decl->decl.staticTy->type;
    }
    return AnalysisResult::kSuccess;
}

AnalysisResult analyze_decl(CompilationContext *cmp, Node *decl) {
    if (resolve_decl_type(cmp, decl)) return AnalysisResult::kFailure;

    if (decl->decl.rval) {
        switch (decl->decl.rval->kind) {
            case NodeKind::kIntLit:
            case NodeKind::kStrLit: break;
            default: todo("Implement declaration rvalue analyze\n");
        }
    }
    return AnalysisResult::kSuccess;
}

}  // namespace

AnalysisResult analyze_file(CompilationContext *cmp) {
    Node *unit = cmp->ctx.file->unit;
    for (size_t i = 0; i < unit->unit.decls.size; i++) {
        Node *decl = unit->unit.decls.get(i);
        if (analyze_decl(cmp, decl)) return AnalysisResult::kFailure;
    }
    return AnalysisResult::kSuccess;
}

};  // namespace lcc
