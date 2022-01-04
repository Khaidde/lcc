#include "analysis.hpp"

#include "diagnostic.hpp"
#include "print.hpp"
#include "scope.hpp"
#include "types.hpp"

namespace lcc {

namespace {

Result scope_enter_func_bind_params(CompilationContext *cmp, Node *func) {
    scope_enter(cmp->scopeStack, func);
    for (size_t i = 0; i < func->func.params.size; i++) {
        Node *param = func->func.params.get(i);
        param->decl.file = cmp->currFile;
        if (Node *other = scope_bind(cmp->scopeStack, param)) {
            dx_err(at_node(cmp->currFile->finfo, param->decl.lval), "Duplicate parameter name\n");
            dx_note(at_node(other->decl.file->finfo, other->decl.lval), "Previous parameter here\n");
            return kError;
        }
        param->decl.isBound = true;
    }
    return kAccept;
}

Result analyze_function_bodies(CompilationContext *cmp);

Node *import_lookup(CompilationContext *cmp, LStringView &symbol) {
    if (Node **import = cmp->currFile->imports.get(symbol)) {
        return *import;
    }
    return nullptr;
}

Node *decl_lookup(CompilationContext *cmp, LStringView &symbol) {
    if (Node **preloadDecl = cmp->preloadPkg->globalDecls.get(symbol)) {
        return *preloadDecl;
    }
    if (cmp->scopeStack->size > 0) {
        for (size_t i = scope_depth(cmp->scopeStack);; i--) {
            Scope *scope = cmp->scopeStack->scopes.get(i);
            if (Node **decl = scope->decls.get(symbol)) {
                return *decl;
            }
            if (scope->owner->kind == NodeKind::kFunc) break;
            if (i == 0) break;
        }
    }
    if (Node **globalDecl = cmp->currFile->package->globalDecls.get(symbol)) {
        return *globalDecl;
    }
    return nullptr;
}

Result scope_exit_check_unused(CompilationContext *cmp) {
    Scope *scope = scope_get(cmp->scopeStack);
    for (size_t i = 0; i < scope->decls.capacity; i++) {
        if (scope->decls.table[i].psl) {
            Node *decl = scope->decls.table[i].val;
            if (!decl->decl.isUsed) {
                dx_err(at_node(cmp->currFile->finfo, decl->decl.lval), "Unused variable\n");
                return kError;
            }
        }
    }
    scope_exit(cmp->scopeStack);
    return kAccept;
}

Type *create_type(TypeKind kind) {
    Type *type = mem::malloc<Type>();
    type->kind = kind;
    return type;
}

bool is_type_equal(Type *t1, Type *t2) {
    assert(t1 && t2);
    if (t1->kind != t2->kind) return false;

    switch (t1->kind) {
        case TypeKind::kNone: return true;
        case TypeKind::kType: return true;
        case TypeKind::kNamed: return t1->name.ref && t2->name.ref && t1->name.ref == t2->name.ref;
        case TypeKind::kPtr: return is_type_equal(t1->ptr.inner, t2->ptr.inner);
        case TypeKind::kFuncTy:
            if (t1->funcTy.paramTys.size != t2->funcTy.paramTys.size) return false;
            if (!is_type_equal(t1->funcTy.retTy, t2->funcTy.retTy)) return false;
            for (size_t i = 0; i < t1->funcTy.paramTys.size; i++) {
                if (!is_type_equal(t1->funcTy.paramTys.get(i), t2->funcTy.paramTys.get(i))) {
                    return false;
                }
            }
            return true;
    }
}

bool is_numeric_type(Type *type) {
    switch (type->kind) {
        case TypeKind::kNone: return false;
        case TypeKind::kType: return false;
        case TypeKind::kNamed: return type->name.ref && type->name.ref == builtin_type::u16->name.ref;
        case TypeKind::kPtr: return true;
        case TypeKind::kFuncTy: return false;
    }
}

Result analyze_decl(CompilationContext *cmp, Node *decl);

Type *resolve_expanded_decl_type(CompilationContext *cmp, Node *expansion) {
    assert(expansion->kind == NodeKind::kDecl);
    File *save = cmp->currFile;
    cmp->currFile = expansion->decl.file;
    if (analyze_decl(cmp, expansion)) return nullptr;
    cmp->currFile = save;
    return expansion->decl.resolvedTy;
}

Node *resolve_type_alias(CompilationContext *cmp, Node *typeAlias) {
    assert(typeAlias->kind == NodeKind::kDecl);
    assert(typeAlias->decl.isDecl);
    if (Type *resolvedType = resolve_expanded_decl_type(cmp, typeAlias)) {
        if (resolvedType->kind != TypeKind::kType) {
            dx_err(at_node(typeAlias->decl.file->finfo, typeAlias),
                   "'%s' expected to be a type alias: Found static type of '%s'\n",
                   lstr_raw_str(typeAlias->decl.lval->name.ident), type_string(resolvedType));
            dx_note(at_node(cmp->currFile->finfo, cmp->currResolvingTypeAlias), "Type alias needed here\n");
            return nullptr;
        }

        // TODO: Also return when encountering a struct rval
        if (typeAlias->decl.isExtern) return typeAlias;
        typeAlias->decl.isUsed = true;

        assert(typeAlias->decl.rval);
        assert(typeAlias->decl.rval->kind == NodeKind::kName);
        typeAlias->decl.rval->name.ref = resolve_type_alias(cmp, typeAlias->decl.rval->name.ref);
        return typeAlias->decl.rval->name.ref;
    }
    return nullptr;
}

Result r_simplify_type_alias(CompilationContext *cmp, Node *src, Type *type) {
    switch (type->kind) {
        case TypeKind::kNone:
        case TypeKind::kType: break;
        case TypeKind::kNamed:
            if (Node *decl = decl_lookup(cmp, type->name.ident)) {
                Node *saveTypeAlias = src;
                cmp->currResolvingTypeAlias = src;
                type->name.ref = resolve_type_alias(cmp, decl);
                if (!type->name.ref) return kError;
                cmp->currResolvingTypeAlias = saveTypeAlias;
            } else {
                dx_err(at_node(cmp->currFile->finfo, src), "No type alias found for '%s'\n",
                       lstr_raw_str(type->name.ident));
                return kError;
            }
            break;
        case TypeKind::kPtr: r_simplify_type_alias(cmp, src, type->ptr.inner); break;
        case TypeKind::kFuncTy:
            for (size_t i = 0; i < type->funcTy.paramTys.size; i++) {
                if (r_simplify_type_alias(cmp, src, type->funcTy.paramTys.get(i))) return kError;
            }
            return r_simplify_type_alias(cmp, src, type->funcTy.retTy);
    }
    return kAccept;
}

Result simplify_type_alias(CompilationContext *cmp, Node *type) {
    assert(type->kind == NodeKind::kType);
    return r_simplify_type_alias(cmp, type, &type->type);
}

Node *expand_name(CompilationContext *cmp, Node *nameRef) {
    assert(nameRef->kind == NodeKind::kName);
    if (Node *decl = decl_lookup(cmp, nameRef->name.ident)) {
        decl->decl.isUsed = true;
        return decl;
    } else if (Node *import = import_lookup(cmp, nameRef->name.ident)) {
        return import;
    }
    dx_err(at_node(cmp->currFile->finfo, nameRef), "No definition found for '%s'\n", lstr_raw_str(nameRef->name.ident));
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
                dx_err(at_node(cmp->currFile->finfo, dotAccessRef->infix.left), "Cannot expand infix operation '%s'\n",
                       token_type_string(dotAccessRef->infix.left->infix.op));
                return nullptr;
            }
        default:
            dx_err(at_node(cmp->currFile->finfo, dotAccessRef->infix.left),
                   "Cannot expand left side of dot access with kind %s\n",
                   node_kind_string(dotAccessRef->infix.left->kind));
            return nullptr;
    }
    if (!baseRef) return nullptr;

    if (dotAccessRef->infix.right->kind != NodeKind::kName) {
        dx_err(at_node(cmp->currFile->finfo, dotAccessRef->infix.right), "Right side of dot access must be a name\n");
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
                dx_err(at_node(cmp->currFile->finfo, dotAccessRef->infix.right),
                       "Could not find declaration in package '%s'\n", lstr_raw_str(baseRef->import.package));
                return nullptr;
            }
        }
    }
    assert(false && "Import/packages should have been resolved before analysis\n");
    return nullptr;
}

Type *resolve_type(CompilationContext *cmp, Node *expr);

Type *resolve_prefix(CompilationContext *cmp, Node *prefix) {
    assert(prefix->kind == NodeKind::kPrefix);

    Type *inner = resolve_type(cmp, prefix->prefix.inner);
    if (!inner) return nullptr;

    switch (prefix->prefix.op) {
        case TokenType::kSubNeg:
            if (is_type_equal(inner, builtin_type::u16)) {
                return inner;
            }
            dx_err(at_node(cmp->currFile->finfo, prefix->prefix.inner),
                   "Argument type of negation '-' expected be an integer: Found '%s'\n", type_string(inner));
            return nullptr;
        case TokenType::kSubSub:
            if (is_numeric_type(inner)) return inner;

            dx_err(at_node(cmp->currFile->finfo, prefix->prefix.inner),
                   "Argument type of pre-increment '--' expected to be numeric: Found '%s'\n", type_string(inner));
            return nullptr;
        case TokenType::kPtr: {
            Type *ptr = create_type(TypeKind::kPtr);
            if (prefix->prefix.inner->kind != NodeKind::kName) {
                if (prefix->prefix.inner->kind != NodeKind::kInfix ||
                    prefix->prefix.inner->infix.op != TokenType::kDot) {
                    dx_err(at_node(cmp->currFile->finfo, prefix),
                           "Argument type of address '*' expected to be a variable\n");
                    return nullptr;
                }
            }
            ptr->ptr.inner = inner;
            return ptr;
        }
        case TokenType::kDeref:
            if (inner->kind == TypeKind::kPtr) {
                return inner->ptr.inner;
            }
            dx_err(at_node(cmp->currFile->finfo, prefix->prefix.inner),
                   "Argument type of deref '@' expected to be a pointer: Found '%s'\n", type_string(inner));
            return nullptr;
        default: todo("Cannot resolve prefix with operator %s\n", token_type_string(prefix->prefix.op)); assert(false);
    }
    return nullptr;
}

Type *resolve_infix(CompilationContext *cmp, Node *infix) {
    if (infix->infix.op == TokenType::kDot) {
        if (Node *declOrImport = expand_dot_access(cmp, infix)) {
            assert(infix->infix.right->kind == NodeKind::kName);
            infix->infix.right->name.ref = declOrImport;
            if (declOrImport->kind == NodeKind::kImport) {
                dx_err(at_node(cmp->currFile->finfo, infix->infix.right), "Package alias cannot be used as a value\n");
                return nullptr;
            }
            return resolve_expanded_decl_type(cmp, declOrImport);
        }
        return nullptr;
    }

    Type *ltype = resolve_type(cmp, infix->infix.left);
    if (!ltype) return nullptr;
    if (ltype->kind == TypeKind::kNone) {
        dx_err(at_node(cmp->currFile->finfo, infix->infix.left),
               "Cannot use expression of type 'none' in infix operation\n");
        return nullptr;
    }

    Type *rtype = resolve_type(cmp, infix->infix.right);
    if (!rtype) return nullptr;
    if (rtype->kind == TypeKind::kNone) {
        dx_err(at_node(cmp->currFile->finfo, infix->infix.right),
               "Cannot use expression of type 'none' in infix operation\n");
        return nullptr;
    }

    if (!is_type_equal(ltype, rtype)) {
        dx_err(at_node(cmp->currFile->finfo, infix), "Mismatched infix operator types: left is '%s', right is '%s'\n",
               type_string(ltype), type_string(rtype));
        return nullptr;
    }

    if (!is_numeric_type(ltype)) {
        dx_err(at_node(cmp->currFile->finfo, infix),
               "Argument types of infix operator expected to be numeric: Found '%s'\n", type_string(ltype));
        return nullptr;
    }
    return ltype;
}

Type *resolve_call(CompilationContext *cmp, Node *call) {
    Type *ctype = resolve_type(cmp, call->call.callee);
    if (!ctype) return nullptr;
    if (ctype->kind != TypeKind::kFuncTy) {
        dx_err(at_node(cmp->currFile->finfo, call->call.callee), "Expression has type '%s' which cannot be called\n",
               type_string(ctype));
        return nullptr;
    }
    if (call->call.args.size != ctype->funcTy.paramTys.size) {
        dx_err(at_node(cmp->currFile->finfo, call), "Expected %d argument(s) but found %d\n",
               ctype->funcTy.paramTys.size, call->call.args.size);
        return nullptr;
    }
    for (size_t i = 0; i < ctype->funcTy.paramTys.size; i++) {
        Type *argTy = resolve_type(cmp, call->call.args.get(i));
        if (!argTy) return nullptr;
        if (!is_type_equal(argTy, ctype->funcTy.paramTys.get(i))) {
            dx_err(at_node(cmp->currFile->finfo, call->call.args.get(i)),
                   "Argument expected to have type '%s': Found '%s'\n", type_string(ctype->funcTy.paramTys.get(i)),
                   type_string(argTy));
            return nullptr;
        }
    }
    return ctype->funcTy.retTy;
}

Type *resolve_type(CompilationContext *cmp, Node *expr) {
    switch (expr->kind) {
        case NodeKind::kIntLit: return builtin_type::u16;
        case NodeKind::kStrLit: return builtin_type::string;
        case NodeKind::kName:
            if (Node *decl = expand_name(cmp, expr)) {
                expr->name.ref = decl;
                if (decl->kind == NodeKind::kImport) {
                    dx_err(at_node(cmp->currFile->finfo, expr), "Package alias cannot be used as a value\n");
                    return nullptr;
                }
                return resolve_expanded_decl_type(cmp, decl);
            }
            return nullptr;
        case NodeKind::kPrefix: return resolve_prefix(cmp, expr);
        case NodeKind::kInfix: return resolve_infix(cmp, expr);
        case NodeKind::kCall: return resolve_call(cmp, expr);
        case NodeKind::kFunc: {
            Type *funcTy = create_type(TypeKind::kFuncTy);
            funcTy->funcTy.paramTys = {};
            if (expr->func.params.size) funcTy->funcTy.paramTys.init(expr->func.params.size);
            for (size_t i = 0; i < expr->func.params.size; i++) {
                Node *param = expr->func.params.get(i);
                param->decl.file = cmp->currFile;
                if (!param->decl.staticTy) {
                    dx_err(at_node(cmp->currFile->finfo, param), "Function parameter must explicitly specify a type\n");
                    return nullptr;
                }
                if (simplify_type_alias(cmp, param->decl.staticTy)) return nullptr;
                param->decl.resolvedTy = &param->decl.staticTy->type;
                funcTy->funcTy.paramTys.add(param->decl.resolvedTy);
            }
            if (expr->func.staticRetTy) {
                if (simplify_type_alias(cmp, expr->func.staticRetTy)) return nullptr;
                funcTy->funcTy.retTy = &expr->func.staticRetTy->type;
                cmp->resolveFuncBodyStack.add(expr);
                cmp->currNumPendingFunc++;
            } else if (expr->func.body->kind == NodeKind::kBlock) {
                funcTy->funcTy.retTy = builtin_type::none;
                cmp->resolveFuncBodyStack.add(expr);
                cmp->currNumPendingFunc++;
            } else {
                if (scope_enter_func_bind_params(cmp, expr)) return nullptr;
                funcTy->funcTy.retTy = resolve_type(cmp, expr->func.body);
                if (!funcTy->funcTy.retTy) return nullptr;
                if (scope_exit_check_unused(cmp)) return nullptr;
            }
            return funcTy;
        }
        default: todo("Cannot resolve type of node %s\n", node_kind_string(expr->kind)); assert(false);
    }
    return nullptr;
}

Result resolve_decl_type(CompilationContext *cmp, Node *decl) {
    assert(decl->kind == NodeKind::kDecl);
    if (decl->decl.resolvedTy) return kAccept;

    if (decl->decl.isResolving) {
        dx_err(at_node(cmp->currFile->finfo, decl), "Detected circular type dependency\n");
        return kError;
    }
    decl->decl.isResolving = true;

    bool hasTy = decl->decl.staticTy;
    bool hasRval = decl->decl.rval;
    if (hasRval) {
        decl->decl.resolvedTy = resolve_type(cmp, decl->decl.rval);
        if (!decl->decl.resolvedTy) return kError;
        if (decl->decl.resolvedTy->kind == TypeKind::kNone) {
            dx_err(at_node(cmp->currFile->finfo, decl->decl.rval), "Cannot assign value of type 'none' to variable\n");
            return kError;
        }
    }
    if (hasTy) {
        if (simplify_type_alias(cmp, decl->decl.staticTy)) return kError;
        Type *staticTy = &decl->decl.staticTy->type;
        if (hasRval) {
            if (!is_type_equal(staticTy, decl->decl.resolvedTy)) {
                dx_err(at_node(cmp->currFile->finfo, decl),
                       "Mismatched declaration types: declared type is '%s', value type is '%s'\n",
                       type_string(staticTy), type_string(decl->decl.resolvedTy));
                return kError;
            }
        }
        if (decl->decl.resolvedTy) {
            // TODO: free the previous decl->decl.resolvedTy if it exists
        }
        decl->decl.resolvedTy = staticTy;
    }
    return kAccept;
}

Result analyze_block(CompilationContext *cmp, Node *block);

Result analyze_if(CompilationContext *cmp, Node *ifstmt) {
    Type *condType = resolve_type(cmp, ifstmt->ifstmt.cond);
    if (analyze_function_bodies(cmp)) return kError;
    if (!condType) return kError;
    if (!is_numeric_type(condType)) {
        dx_err(at_node(cmp->currFile->finfo, ifstmt->ifstmt.cond),
               "If statement condition expected to be a numeric type\n");
        return kError;
    }

    scope_enter(cmp->scopeStack, ifstmt);
    if (analyze_block(cmp, ifstmt->ifstmt.then)) return kError;
    if (ifstmt->ifstmt.alt) {
        if (scope_exit_check_unused(cmp)) return kError;

        ifstmt->ifstmt.branchLevel = ifstmt->ifstmt.then->block.branchLevel;
        if (ifstmt->ifstmt.alt->kind == NodeKind::kIf) {
            if (analyze_if(cmp, ifstmt->ifstmt.alt)) return kError;
            if (ifstmt->ifstmt.alt->ifstmt.branchLevel > ifstmt->ifstmt.branchLevel) {
                ifstmt->ifstmt.branchLevel = ifstmt->ifstmt.alt->ifstmt.branchLevel;
            }
        } else if (ifstmt->ifstmt.alt->kind == NodeKind::kBlock) {
            scope_enter(cmp->scopeStack, ifstmt);
            if (analyze_block(cmp, ifstmt->ifstmt.alt)) return kError;
            if (scope_exit_check_unused(cmp)) return kError;
            if (ifstmt->ifstmt.alt->block.branchLevel > ifstmt->ifstmt.branchLevel) {
                ifstmt->ifstmt.branchLevel = ifstmt->ifstmt.alt->block.branchLevel;
            }
        } else {
            assert(false && "Alt field of ifstmt should either be a block or another if statement");
        }
    } else {
        ifstmt->ifstmt.branchLevel = scope_depth(cmp->scopeStack);
        if (scope_exit_check_unused(cmp)) return kError;
    }
    return kAccept;
}

Result analyze_while(CompilationContext *cmp, Node *whilestmt) {
    Type *condType = resolve_type(cmp, whilestmt->whilestmt.cond);
    if (analyze_function_bodies(cmp)) return kError;
    if (!condType) return kError;
    if (!is_numeric_type(condType)) {
        dx_err(at_node(cmp->currFile->finfo, whilestmt->whilestmt.cond),
               "While statement condition expected to be a numeric type\n");
        return kError;
    }

    scope_enter(cmp->scopeStack, whilestmt);
    if (analyze_block(cmp, whilestmt->whilestmt.loop)) return kError;
    whilestmt->whilestmt.branchLevel = whilestmt->whilestmt.loop->block.branchLevel;
    if (scope_exit_check_unused(cmp)) return kError;
    return kAccept;
}

Result analyze_block(CompilationContext *cmp, Node *block) {
    assert(block->kind == NodeKind::kBlock);
    // +1 so that if there is no modification to branchLevel (no returns, breaks, etc)
    // block will not "break out of itself". The algorithm checks out ;)
    block->block.branchLevel = scope_depth(cmp->scopeStack) + 1;

    for (size_t i = 0; i < block->block.stmts.size; i++) {
        Node *stmt = block->block.stmts.get(i);
        if (block->block.branchLevel <= scope_depth(cmp->scopeStack)) {
            dx_err(at_node(cmp->currFile->finfo, stmt), "Unreachable code\n");
            return kError;
        }
        switch (stmt->kind) {
            case NodeKind::kDecl:
                if (stmt->decl.isExtern) {
                    dx_err(at_node(cmp->currFile->finfo, stmt), "Local declaration cannot be marked as #extern\n");
                    return kError;
                }
                if (analyze_decl(cmp, stmt)) return kError;
                break;
            case NodeKind::kBlock:
                scope_enter(cmp->scopeStack, stmt);
                if (analyze_block(cmp, stmt)) return kError;
                if (scope_exit_check_unused(cmp)) return kError;
                if (stmt->block.branchLevel < block->block.branchLevel) {
                    block->block.branchLevel = stmt->block.branchLevel;
                }
                break;
            case NodeKind::kIf:
                if (analyze_if(cmp, stmt)) return kError;
                if (stmt->ifstmt.branchLevel < block->block.branchLevel) {
                    block->block.branchLevel = stmt->ifstmt.branchLevel;
                }
                break;
            case NodeKind::kWhile:
                if (analyze_while(cmp, stmt)) return kError;
                if (stmt->whilestmt.branchLevel < block->block.branchLevel) {
                    block->block.branchLevel = stmt->whilestmt.branchLevel;
                }
                break;
            case NodeKind::kRet:
                if (stmt->ret.value) {
                    stmt->ret.resolvedTy = resolve_type(cmp, stmt->ret.value);
                    if (!stmt->ret.resolvedTy) return kError;
                    if (!cmp->currRetTy) {
                        dx_err(at_node(cmp->currFile->finfo, stmt),
                               "Returns value of type '%s' but function has no return type\n",
                               type_string(stmt->ret.resolvedTy));
                        return kError;
                    }
                    if (!is_type_equal(&cmp->currRetTy->type, stmt->ret.resolvedTy)) {
                        dx_err(at_node(cmp->currFile->finfo, stmt),
                               "Mismatched types: function return type is '%s', return statement type is '%s'\n",
                               type_string(&cmp->currRetTy->type), type_string(stmt->ret.resolvedTy));
                        dx_note(at_node(cmp->currFile->finfo, cmp->currRetTy), "Return type specified here\n");
                        return kError;
                    }
                } else {
                    stmt->ret.resolvedTy = builtin_type::none;
                    if (cmp->currRetTy) {
                        dx_err(at_node(cmp->currFile->finfo, stmt), "Must return value of type '%s'\n",
                               type_string(&cmp->currRetTy->type));
                        dx_note(at_node(cmp->currFile->finfo, cmp->currRetTy), "Return type specified here\n");
                        return kError;
                    }
                }
                for (size_t i = scope_depth(cmp->scopeStack);; i--) {
                    Scope *scope = cmp->scopeStack->scopes.get(i);
                    if (scope->owner->kind == NodeKind::kFunc) {
                        block->block.branchLevel = i;
                        break;
                    }
                    if (i == 0) assert(false);
                }
                break;
            case NodeKind::kLoopBr:
                for (size_t i = scope_depth(cmp->scopeStack);; i--) {
                    Scope *scope = cmp->scopeStack->scopes.get(i);
                    if (scope->owner->kind == NodeKind::kWhile) {
                        if (stmt->loopbr.label.src) {
                            if (scope->owner->whilestmt.label.src &&
                                lstr_equal(scope->owner->whilestmt.label, stmt->loopbr.label)) {
                                block->block.branchLevel = i;
                                break;
                            }
                        } else {
                            block->block.branchLevel = i;
                            break;
                        }
                    }
                    if (scope->owner->kind == NodeKind::kFunc) {
                        if (stmt->loopbr.label.src) {
                            dx_err(at_node(cmp->currFile->finfo, stmt), "Could not find label '%s'\n",
                                   lstr_raw_str(stmt->loopbr.label));
                        } else {
                            dx_err(at_node(cmp->currFile->finfo, stmt), "%s statement must be inside a loop\n",
                                   node_kind_string(stmt->kind));
                        }
                        return kError;
                    }
                    if (i == 0) assert(false);
                }
                break;
            default:
                if (Type *exprType = resolve_type(cmp, stmt)) {
                    if (exprType->kind != TypeKind::kNone) {
                        dx_err(at_node(cmp->currFile->finfo, stmt), "Unused expression result\n");
                        return kError;
                    }
                } else {
                    return kError;
                }
                if (analyze_function_bodies(cmp)) return kError;
        }
    }
    return kAccept;
}

Result analyze_function_bodies(CompilationContext *cmp) {
    while (cmp->resolveFuncBodyStack.size && cmp->currNumPendingFunc) {
        size_t saveNumPendingFunc = cmp->currNumPendingFunc;
        cmp->currNumPendingFunc = 0;

        Node *saveRetTy = cmp->currRetTy;
        Node *currFunction = cmp->resolveFuncBodyStack.get(cmp->resolveFuncBodyStack.size - 1);
        cmp->currRetTy = currFunction->func.staticRetTy;
        cmp->resolveFuncBodyStack.size--;

        if (scope_enter_func_bind_params(cmp, currFunction)) return kError;
        if (currFunction->func.body->kind == NodeKind::kBlock) {
            if (analyze_block(cmp, currFunction->func.body)) return kError;
            if (cmp->currRetTy) {
                if (currFunction->func.body->block.branchLevel > scope_depth(cmp->scopeStack)) {
                    dx_err(at_node(cmp->currFile->finfo, cmp->currRetTy),
                           "Function does not return a value in all scenarios\n");
                    return kError;
                }
            }
        } else {
            Type *bodyType = resolve_type(cmp, currFunction->func.body);
            if (!bodyType) return kError;

            assert(cmp->currRetTy);
            if (!is_type_equal(bodyType, &cmp->currRetTy->type)) {
                dx_err(at_node(cmp->currFile->finfo, currFunction),
                       "Mismatched types: declared return type is '%s', return value type is '%s'\n",
                       type_string(&cmp->currRetTy->type), type_string(bodyType));
                return kError;
            }
        }
        if (scope_exit_check_unused(cmp)) return kError;

        cmp->currRetTy = saveRetTy;
        cmp->currNumPendingFunc = saveNumPendingFunc - 1;
    }
    return kAccept;
}

Result analyze_decl(CompilationContext *cmp, Node *decl) {
    assert(decl->kind == NodeKind::kDecl);
    decl->decl.file = cmp->currFile;
    if (resolve_decl_type(cmp, decl)) return kError;

    if (decl->decl.isDecl) {
        if (!decl->decl.isBound) {
            if (Node *otherDecl = decl_lookup(cmp, decl->decl.lval->name.ident)) {
                dx_err(at_node(decl->decl.file->finfo, decl->decl.lval), "Redeclaration\n");
                dx_note(at_node(otherDecl->decl.file->finfo, otherDecl->decl.lval), "First declaration here\n");
                return kError;
            }
            if (Node *import = import_lookup(cmp, decl->decl.lval->name.ident)) {
                dx_err(at_node(decl->decl.file->finfo, decl->decl.lval),
                       "Declaration cannot have the same name as a package alias\n");
                dx_note(at_node(decl->decl.file->finfo, import), "Package alias found here\n");
                return kError;
            }
            scope_bind(cmp->scopeStack, decl);
            decl->decl.isBound = true;
        }
    } else {
        assert(!decl->decl.staticTy);
        Type *lType = resolve_type(cmp, decl->decl.lval);
        if (lType->kind == TypeKind::kType) {
            dx_err(at_node(decl->decl.file->finfo, decl->decl.lval), "Cannot reassign to a type alias\n");
            return kError;
        }
        if (!lType) return kError;
        if (!is_type_equal(lType, decl->decl.resolvedTy)) {
            dx_err(at_node(decl->decl.file->finfo, decl), "Mismatched assignment types: left is '%s', right is '%s'\n",
                   type_string(lType), type_string(decl->decl.resolvedTy));
            return kError;
        }
    }

    return analyze_function_bodies(cmp);
}

}  // namespace

Result analyze_file(CompilationContext *cmp) {
    for (size_t i = 0; i < cmp->currFile->package->globalDecls.capacity; i++) {
        auto &entry = cmp->currFile->package->globalDecls.table[i];
        if (entry.psl) {
            cmp->currFile = entry.val->decl.file;
            if (analyze_decl(cmp, entry.val)) return kError;
        }
    }
    return kAccept;
}
};  // namespace lcc
