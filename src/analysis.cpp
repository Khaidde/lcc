#include "analysis.hpp"

#include "diagnostic.hpp"
#include "print.hpp"
#include "scope.hpp"
#include "types.hpp"

namespace lcc {

namespace {

Result scope_enter_func_bind_params(CompilationContext *cmp, Node *func) {
    if (!cmp->file->scopeStack) {
        cmp->file->scopeStack = scope_init();
    }
    scope_enter(cmp->file->scopeStack, true);
    for (size_t i = 0; i < func->func.params.size; i++) {
        Node *param = func->func.params.get(i);
        param->decl.file = cmp->file;
        if (Node *other = scope_bind(cmp->file->scopeStack, param)) {
            dx_err(at_node(cmp->file->finfo, param->decl.lval), "Duplicate parameter name\n");
            dx_note(at_node(other->decl.file->finfo, other->decl.lval), "Previous parameter here\n");
            return Result::kError;
        }
        param->decl.isBound = true;
    }
    return Result::kAccept;
}

Result analyze_function_bodies(CompilationContext *cmp);

Result resolve_decl_type(CompilationContext *cmp, Node *decl);

Node *decl_lookup(CompilationContext *cmp, LStringView &symbol) {
    if (cmp->file->scopeStack) {
        for (int i = cmp->file->scopeStack->size - 1; i >= 0; i--) {
            Scope &scope = cmp->file->scopeStack->scopes[i];
            if (Node **decl = scope.decls.get(symbol)) {
                return *decl;
            }
            if (scope.isClosed) break;
        }
    }
    if (Node **globalDecl = cmp->file->package->globalDecls.get(symbol)) {
        return *globalDecl;
    }
    if (Node **import = cmp->file->imports.get(symbol)) {
        return *import;
    }
    return nullptr;
}

Type *create_type(TypeKind kind) {
    Type *type = mem::malloc<Type>();
    type->kind = kind;
    return type;
}

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

Node *expand_name(CompilationContext *cmp, Node *nameRef) {
    assert(nameRef->kind == NodeKind::kName);
    if (Node *declOrImport = decl_lookup(cmp, nameRef->name.ident)) return declOrImport;
    dx_err(at_node(cmp->file->finfo, nameRef), "No definition found for '%s'\n", lstr_raw_str(nameRef->name.ident));
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
                dx_err(at_node(cmp->file->finfo, dotAccessRef->infix.left), "Cannot expand infix operation '%s'\n",
                       token_type_string(dotAccessRef->infix.left->infix.op));
                return nullptr;
            }
        default:
            dx_err(at_node(cmp->file->finfo, dotAccessRef->infix.left),
                   "Cannot expand left side of dot access with kind %s\n",
                   node_kind_string(dotAccessRef->infix.left->kind));
            return nullptr;
    }
    if (!baseRef) return nullptr;

    if (dotAccessRef->infix.right->kind != NodeKind::kName) {
        dx_err(at_node(cmp->file->finfo, dotAccessRef->infix.right), "Right side of dot access must be a name\n");
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
                dx_err(at_node(cmp->file->finfo, dotAccessRef->infix.right),
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
        dx_err(at_node(cmp->file->finfo, original), "Package alias cannot be used as a variable\n");
        return nullptr;
    }
    assert(expansion->kind == NodeKind::kDecl);
    File *save = cmp->file;
    cmp->file = expansion->decl.file;
    if (resolve_decl_type(cmp, expansion)) return nullptr;
    cmp->file = save;
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
            dx_err(at_node(cmp->file->finfo, prefix->prefix.inner),
                   "Argument type of negation '-' expected be an integer: Found '%s'\n", type_string(inner));
            return nullptr;
        case TokenType::kSubSub:
            if (inner->kind == TypeKind::kPtr || is_type_equal(inner, &builtin_type::u16)) {
                return inner;
            }
            dx_err(at_node(cmp->file->finfo, prefix->prefix.inner),
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
            dx_err(at_node(cmp->file->finfo, prefix->prefix.inner),
                   "Argument type of deref '@' expected to be a pointer: Found '%s'\n", type_string(inner));
            return nullptr;
        default: todo("Cannot resolve prefix with operator %s\n", token_type_string(prefix->prefix.op)); assert(false);
    }
    return nullptr;
}

Type *resolve_infix(CompilationContext *cmp, Node *infix) {
    if (infix->infix.op == TokenType::kDot) {
        if (Node *decl = expand_dot_access(cmp, infix)) {
            assert(infix->infix.right->kind == NodeKind::kName);
            infix->infix.right->name.ref = decl;
            return resolve_expansion(cmp, infix, decl);
        }
        return nullptr;
    }

    Type *ltype = resolve_type(cmp, infix->infix.left);
    if (!ltype) return nullptr;
    if (ltype->kind == TypeKind::kNone) {
        dx_err(at_node(cmp->file->finfo, infix->infix.left),
               "Cannot use expression of type 'none' in infix operation\n");
        return nullptr;
    }

    Type *rtype = resolve_type(cmp, infix->infix.right);
    if (!rtype) return nullptr;
    if (rtype->kind == TypeKind::kNone) {
        dx_err(at_node(cmp->file->finfo, infix->infix.right),
               "Cannot use expression of type 'none' in infix operation\n");
        return nullptr;
    }

    if (!is_type_equal(ltype, rtype)) {
        dx_err(at_node(cmp->file->finfo, infix), "Mismatched infix operator types: left is '%s', right is '%s'\n",
               type_string(ltype), type_string(rtype));
        return nullptr;
    }

    if (!is_type_equal(ltype, &builtin_type::u16)) {
        dx_err(at_node(cmp->file->finfo, infix),
               "Argument types of infix operator expected to be integers: Found '%s'\n", type_string(ltype));
        return nullptr;
    }
    return ltype;
}

Type *resolve_call(CompilationContext *cmp, Node *call) {
    Type *ctype = resolve_type(cmp, call->call.callee);
    if (!ctype) return nullptr;
    if (ctype->kind != TypeKind::kFuncTy) {
        dx_err(at_node(cmp->file->finfo, call->call.callee), "Expression has type '%s' which cannot be called\n",
               type_string(ctype));
        return nullptr;
    }
    if (call->call.args.size != ctype->func.paramTys.size) {
        dx_err(at_node(cmp->file->finfo, call), "Expected %d argument(s) but found %d\n", ctype->func.paramTys.size,
               call->call.args.size);
        return nullptr;
    }
    for (size_t i = 0; i < ctype->func.paramTys.size; i++) {
        Type *argTy = resolve_type(cmp, call->call.args.get(i));
        if (!argTy) return nullptr;
        if (!is_type_equal(argTy, ctype->func.paramTys.get(i))) {
            dx_err(at_node(cmp->file->finfo, call->call.args.get(i)),
                   "Argument expected to have type '%s': Found '%s'\n", type_string(ctype->func.paramTys.get(i)),
                   type_string(argTy));
            return nullptr;
        }
    }
    return ctype->func.retTy;
}

Type *resolve_type(CompilationContext *cmp, Node *expr) {
    switch (expr->kind) {
        case NodeKind::kIntLit: return &builtin_type::u16;
        case NodeKind::kStrLit: return &builtin_type::string;
        case NodeKind::kName:
            if (Node *decl = expand_name(cmp, expr)) {
                expr->name.ref = decl;
                return resolve_expansion(cmp, expr, decl);
            }
            return nullptr;
        case NodeKind::kPrefix: return resolve_prefix(cmp, expr);
        case NodeKind::kInfix: return resolve_infix(cmp, expr);
        case NodeKind::kCall: return resolve_call(cmp, expr);
        case NodeKind::kFunc: {
            Type *funcTy = create_type(TypeKind::kFuncTy);
            funcTy->func.paramTys = {};
            if (expr->func.params.size) funcTy->func.paramTys.init(expr->func.params.size);
            for (size_t i = 0; i < expr->func.params.size; i++) {
                Node *decl = expr->func.params.get(i);
                decl->decl.file = cmp->file;
                if (!decl->decl.staticTy) {
                    dx_err(at_node(cmp->file->finfo, decl), "Function parameter must explicitly specify a type\n");
                    return nullptr;
                }
                decl->decl.resolvedTy = &decl->decl.staticTy->type;
                funcTy->func.paramTys.add(decl->decl.resolvedTy);
            }
            if (expr->func.retTy) {
                funcTy->func.retTy = &expr->func.retTy->type;
                cmp->resolveFuncBodyStack.add({cmp->file, expr});
            } else if (expr->func.body->kind == NodeKind::kBlock) {
                funcTy->func.retTy = &builtin_type::none;
                cmp->resolveFuncBodyStack.add({cmp->file, expr});
            } else {
                // FIXME: multiple single line functions in one file global scope which
                // depend on each other(forward declarations) will cause scope to expand and potentially exceed
                // limit. Solution that's not very appealing: each declaration has its own scopeStack Other
                // Solution: scopeStack is defined as a dynamic list of scopes (no limit on scope depth) Note: hairy
                // because single line functions may need to be type inferred
                if (scope_enter_func_bind_params(cmp, expr)) return nullptr;
                funcTy->func.retTy = resolve_type(cmp, expr->func.body);
                if (!funcTy->func.retTy) return nullptr;
                scope_exit(cmp->file->scopeStack);
            }
            return funcTy;
        }
        default: todo("Cannot resolve type of node %s\n", node_kind_string(expr->kind)); assert(false);
    }
    return nullptr;
}

Result resolve_decl_type(CompilationContext *cmp, Node *decl) {
    assert(decl->kind == NodeKind::kDecl);
    if (decl->decl.resolvedTy) return Result::kAccept;

    if (decl->decl.isVisited) {
        dx_err(at_node(cmp->file->finfo, decl), "Detected circular type dependency\n");
        return Result::kError;
    }
    decl->decl.isVisited = true;

    bool hasTy = decl->decl.staticTy;
    bool hasRval = decl->decl.rval;
    if (hasRval) {
        decl->decl.resolvedTy = resolve_type(cmp, decl->decl.rval);
        if (!decl->decl.resolvedTy) return Result::kError;
        if (decl->decl.resolvedTy->kind == TypeKind::kNone) {
            dx_err(at_node(cmp->file->finfo, decl->decl.rval), "Cannot assign value of type 'none' to variable\n");
            return Result::kError;
        }
    }
    if (hasRval && hasTy) {
        Type *staticTy = &decl->decl.staticTy->type;
        if (!is_type_equal(staticTy, decl->decl.resolvedTy)) {
            dx_err(at_node(cmp->file->finfo, decl),
                   "Mismatched declaration types: declared type is '%s', value type is '%s'\n", type_string(staticTy),
                   type_string(decl->decl.resolvedTy));
            return Result::kError;
        }
    } else if (hasTy) {
        decl->decl.resolvedTy = &decl->decl.staticTy->type;
    }
    return Result::kAccept;
}

Result analyze_decl(CompilationContext *cmp, Node *decl);

Result analyze_block(CompilationContext *cmp, Node *block) {
    assert(block->kind == NodeKind::kBlock);

    for (size_t i = 0; i < block->block.stmts.size; i++) {
        Node *stmt = block->block.stmts.get(i);
        switch (stmt->kind) {
            case NodeKind::kDecl:
                if (analyze_decl(cmp, stmt)) return Result::kError;
                break;
            case NodeKind::kBlock:
                assert(cmp->file->scopeStack);
                scope_enter(cmp->file->scopeStack, false);
                if (analyze_block(cmp, stmt)) return Result::kError;
                scope_exit(cmp->file->scopeStack);
                break;
            case NodeKind::kIf: todo("If statement analysis\n"); break;
            case NodeKind::kWhile: todo("While statement analysis\n"); break;
            case NodeKind::kRet:
                // Gotta do that control flow analysis owo
                todo("Ret statement analysis\n");
                break;
            default:
                if (!resolve_type(cmp, stmt)) return Result::kError;
                return analyze_function_bodies(cmp);
        }
    }
    return Result::kAccept;
}

Result analyze_function_bodies(CompilationContext *cmp) {
    while (cmp->resolveFuncBodyStack.size) {
        Node *func = cmp->resolveFuncBodyStack.get(cmp->resolveFuncBodyStack.size - 1).func;
        File *save = cmp->file;
        cmp->file = cmp->resolveFuncBodyStack.get(cmp->resolveFuncBodyStack.size - 1).file;
        cmp->resolveFuncBodyStack.size--;

        if (scope_enter_func_bind_params(cmp, func)) return Result::kError;
        if (func->func.body->kind == NodeKind::kBlock) {
            if (analyze_block(cmp, func->func.body)) return Result::kError;
        } else {
            Type *bodyType = resolve_type(cmp, func->func.body);
            if (!bodyType) return Result::kError;
            if (!is_type_equal(bodyType, &func->func.retTy->type)) {
                dx_err(at_node(cmp->file->finfo, func),
                       "Mismatched types: declared return type is '%s', return value type is '%s'\n",
                       type_string(&func->func.retTy->type), type_string(bodyType));
                return Result::kError;
            }
        }
        scope_exit(cmp->file->scopeStack);

        cmp->file = save;
    }
    return Result::kAccept;
}

Result analyze_decl(CompilationContext *cmp, Node *decl) {
    assert(decl->kind == NodeKind::kDecl);
    decl->decl.file = cmp->file;
    if (resolve_decl_type(cmp, decl)) return Result::kError;

    if (decl->decl.isDecl) {
        if (decl->decl.lval->kind != NodeKind::kName) {
            dx_err(at_node(cmp->file->finfo, decl->decl.lval), "Left side of declaration must be a variable name\n");
            return Result::kError;
        }
        if (!decl->decl.isBound) {
            if (Node *other = decl_lookup(cmp, decl->decl.lval->name.ident)) {
                dx_err(at_node(cmp->file->finfo, decl->decl.lval), "Duplicate declaration\n");
                dx_note(at_node(other->decl.file->finfo, other->decl.lval), "Previous declaration here\n");
                return Result::kError;
            }
            scope_bind(cmp->file->scopeStack, decl);
            decl->decl.isBound = true;
        }
    } else {
        assert(!decl->decl.staticTy);
        Type *lType = resolve_type(cmp, decl->decl.lval);
        if (!lType) return Result::kError;
        if (!is_type_equal(lType, decl->decl.resolvedTy)) {
            dx_err(at_node(cmp->file->finfo, decl), "Mismatched assignment types: left is '%s', right is '%s'\n",
                   type_string(lType), type_string(decl->decl.resolvedTy));
            return Result::kError;
        }
    }

    return analyze_function_bodies(cmp);
}

}  // namespace

Result analyze_file(CompilationContext *cmp) {
    for (size_t i = 0; i < cmp->file->package->globalDecls.capacity; i++) {
        auto &entry = cmp->file->package->globalDecls.table[i];
        if (entry.psl) {
            cmp->file = entry.val->decl.file;
            if (analyze_decl(cmp, entry.val)) return Result::kError;
        }
    }
    return Result::kAccept;
}
};  // namespace lcc
