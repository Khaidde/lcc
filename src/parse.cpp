#include "parse.hpp"

#include "ast.hpp"
#include "diagnostic.hpp"
#include "print.hpp"
#include "token.hpp"

namespace lcc {

namespace {

bool check_peek(Lexer *l, TokenType type) { return lex_peek(l)->type == type; }

Node *create_node(Lexer *l, NodeKind kind) {
    Node *node = mem::malloc<Node>();
    node->startI = lex_peek(l)->startI;
    node->kind = kind;
    return node;
}

void align_node_start(Node *node, size_t startI) { node->startI = startI; }

void end_node(Lexer *l, Node *node) { node->endI = lex_peek(l)->startI - 1; }

Node *parse_decl_from_lval(Lexer *l, Node *lval);

Node *parse_type(Lexer *l);

Node *parse_operand(Lexer *l);
Node *parse_infix(Lexer *l, int prec, Node *left);
Node *parse_expr(Lexer *l);

Node *parse_block(Lexer *l);

Result parse_directive(Lexer *l, LStringView &directive) {
    assert(lex_peek(l)->type == TokenType::kDirective);
    lex_next(l);  // next #
    if (check_peek(l, TokenType::kErr)) return kError;
    if (!check_peek(l, TokenType::kIdent)) {
        dx_err(at_token(l->finfo, lex_peek(l)), "Compiler directive must have a name\n");
        return kError;
    }
    if (!lstr_equal(lex_peek(l)->ident, directive)) {
        dx_err(at_token(l->finfo, lex_peek(l)), "Unknown compiler directive. Did you mean '%s'?\n",
               lstr_raw_str(directive));
        return kError;
    }
    lex_next(l);  // next directive
    if (check_peek(l, TokenType::kErr)) return kError;
    return kAccept;
}

Node *parse_import(Lexer *l) {
    Node *import = create_node(l, NodeKind::kImport);

    bool hasAlias = true;

    Token *tkn = lex_peek(l);
    if (tkn->type == TokenType::kErr) return nullptr;
    if (tkn->type == TokenType::kIdent) {
        import->import.alias = tkn->ident;

        lex_next(l);  // next 'ident'
        if (check_peek(l, TokenType::kErr)) return nullptr;
        tkn = lex_peek(l);
    } else {
        hasAlias = false;
    }
    if (!check_peek(l, TokenType::kStrLiteral)) {
        dx_err(at_token(l->finfo, tkn), "Expected package path after import keyword\n");
        return nullptr;
    }
    import->import.package = tkn->str;
    if (!hasAlias) {
        // Get rightmost directory to use as alias
        const char *optr = &tkn->str.src[tkn->str.len - 1];
        while (optr >= tkn->str.src) {
            if (*optr == '/' || *optr == '\\') break;
            optr--;
        }
        if (optr != tkn->str.src || *optr == '/' || *optr == '\\') optr++;
        import->import.alias = {optr, tkn->str.len - (size_t)(optr - tkn->str.src)};
    }

    lex_next(l);  // next string
    end_node(l, import);
    return import;
}

Node *parse_decl_or_expr(Lexer *l) {
    Node *expr = parse_expr(l);
    if (!expr) return nullptr;

    if (check_peek(l, TokenType::kColon) || check_peek(l, TokenType::kAssign)) {
        return parse_decl_from_lval(l, expr);
    }
    return expr;
}

Node *parse_decl_from_lval(Lexer *l, Node *lval) {
    switch (lval->kind) {
        case NodeKind::kName: break;
        case NodeKind::kPrefix:
            if (lval->prefix.op == TokenType::kDeref) break;
            dx_err(at_node(l->finfo, lval), "%s prefix expression not allowed as lvalue\n",
                   token_type_string(lval->prefix.op));
            return nullptr;
        case NodeKind::kInfix:
            if (lval->infix.op == TokenType::kDot) break;
            dx_err(at_node(l->finfo, lval), "%s infix expression not allowed as lvalue\n",
                   token_type_string(lval->infix.op));
            return nullptr;
        default:
            dx_err(at_node(l->finfo, lval), "%s expression not allowed as lvalue\n", node_kind_string(lval->kind));
            return nullptr;
    }

    Node *decl = create_node(l, NodeKind::kDecl);
    align_node_start(decl, lval->startI);
    decl->decl.lval = lval;
    decl->decl.file = nullptr;
    decl->decl.resolvedTy = nullptr;
    decl->decl.isExtern = false;
    decl->decl.isResolving = false;
    decl->decl.isBound = false;
    decl->decl.isUsed = false;

    if (check_peek(l, TokenType::kErr)) return nullptr;
    bool hasType = check_peek(l, TokenType::kColon);
    if (hasType) {
        decl->decl.isDecl = true;
        lex_next(l);  // next :

        if (check_peek(l, TokenType::kAssign)) {
            decl->decl.staticTy = nullptr;
        } else {
            decl->decl.staticTy = parse_type(l);
            if (!decl->decl.staticTy) return nullptr;
        }
    } else {
        decl->decl.isDecl = false;
        decl->decl.staticTy = nullptr;
    }

    if (check_peek(l, TokenType::kErr)) return nullptr;
    bool hasAssignment = check_peek(l, TokenType::kAssign);
    if (hasAssignment) {
        lex_next(l);  // next =
        decl->decl.rval = parse_expr(l);
        if (!decl->decl.rval) return nullptr;
    } else {
        decl->decl.rval = nullptr;
        if (check_peek(l, TokenType::kDirective)) {
            LStringView externDirective{"extern", 6};
            if (parse_directive(l, externDirective)) return nullptr;
            decl->decl.isExtern = true;
        }
    }
    if (!hasType && !hasAssignment) {
        dx_err(at_node(l->finfo, decl->decl.lval), "Declaration must have either a type or value\n");
        return nullptr;
    }

    end_node(l, decl);
    return decl;
}

constexpr BaseTypeKind kBaseTypes[] = {BaseTypeKind::u16, BaseTypeKind::string};

constexpr size_t kNumBaseTypes = sizeof(kBaseTypes) / sizeof(BaseTypeKind);

bool r_parse_type(Lexer *l, Type *dest) {
    Token *tkn = lex_peek(l);
    switch (tkn->type) {
        case TokenType::kIdent: {
            dest->kind = TypeKind::kBase;
            size_t i;
            for (i = 0; i < kNumBaseTypes; i++) {
                size_t k;
                const char *baseType = base_type_string(kBaseTypes[i]);
                for (k = 0; k < tkn->ident.len; k++) {
                    if (tkn->ident.src[k] != baseType[k]) break;
                }
                if (k == tkn->ident.len) {
                    dest->base.kind = kBaseTypes[i];
                    break;
                }
            }
            if (i == kNumBaseTypes) {
                todo("Unknown type should be converted into a named type\n");
                dx_err(at_token(l->finfo, tkn), "Unknown type\n");
                return true;
            }
            lex_next(l);  // next 'ident'
            break;
        }
        case TokenType::kPtr: {
            dest->kind = TypeKind::kPtr;
            lex_next(l);  // next *

            dest->ptr.inner = mem::malloc<Type>();
            if (r_parse_type(l, dest->ptr.inner)) return true;
            break;
        }
        case TokenType::kLParen: {
            dest->kind = TypeKind::kFuncTy;
            dest->func.paramTys = {};
            lex_next(l);  // next (
            for (;;) {
                Token *tkn = lex_peek(l);
                if (tkn->type == TokenType::kErr) return true;
                if (tkn->type == TokenType::kEof) {
                    dx_err(at_token(l->finfo, tkn), "Expected argument type but reached end of file\n");
                    return true;
                }
                if (tkn->type == TokenType::kRParen) {
                    lex_next(l);  // next )
                    break;
                }

                Type *argTy = mem::malloc<Type>();
                if (r_parse_type(l, argTy)) return true;
                dest->func.paramTys.add(argTy);

                if (check_peek(l, TokenType::kComma)) {
                    lex_next(l);  // next ,
                } else if (check_peek(l, TokenType::kRParen)) {
                    lex_next(l);  // next )
                    break;
                } else {
                    dx_err(at_token(l->finfo, lex_peek(l)), "Expected , to separate argment types\n");
                    return true;
                }
            }
            if (check_peek(l, TokenType::kArrow)) {
                lex_next(l);  // next ->
                dest->func.retTy = mem::malloc<Type>();
                if (r_parse_type(l, dest->func.retTy)) return true;
            } else {
                // TODO: builtin_type may/should be removed for prelude system
                dest->func.retTy = &builtin_type::none;
            }
            break;
        }
        case TokenType::kEof: dx_err(at_token(l->finfo, tkn), "Expected a type but reached end of file\n");
        case TokenType::kErr: return true;
        default:
            dx_err(at_token(l->finfo, tkn), "Expected a type but instead got %s\n", token_type_string(tkn->type));
            return true;
    }
    return false;
}

Node *parse_type(Lexer *l) {
    Node *type = create_node(l, NodeKind::kType);
    if (r_parse_type(l, &type->type)) return nullptr;
    end_node(l, type);
    return type;
}

constexpr int kLowPrecedence = 0;
constexpr int kPrefixPrecedence = 3;
constexpr int kPostfixPrecedence = 4;

int get_precedence(TokenType op) {
    switch (op) {
        case TokenType::kAddAdd:
        case TokenType::kSubSub:
        case TokenType::kLParen:  // Function call
        case TokenType::kDot: return kPostfixPrecedence;
        case TokenType::kBitAnd: return 2;
        case TokenType::kAdd:
        case TokenType::kSubNeg: return 1;
        default: return kLowPrecedence;
    }
}

Node *parse_operand(Lexer *l) {
    Token *tkn = lex_peek(l);
    switch (tkn->type) {
        case TokenType::kAddAdd:
        case TokenType::kSubNeg:
        case TokenType::kSubSub:
        case TokenType::kPtr:
        case TokenType::kDeref: {
            Node *prefix = create_node(l, NodeKind::kPrefix);
            prefix->prefix.op = tkn->type;
            lex_next(l);  // next 'op'
            prefix->prefix.inner = parse_infix(l, kPrefixPrecedence, parse_operand(l));
            end_node(l, prefix);
            if (!prefix->prefix.inner) return nullptr;
            return prefix;
        }
        case TokenType::kIntLiteral: {
            Node *intLit = create_node(l, NodeKind::kIntLit);
            intLit->intLit.intVal = tkn->intVal;
            lex_next(l);  // next 'intlit'
            end_node(l, intLit);
            return intLit;
        }
        case TokenType::kStrLiteral: {
            Node *strLit = create_node(l, NodeKind::kStrLit);
            strLit->strLit.strVal = tkn->str;
            lex_next(l);  // next 'intlit'
            end_node(l, strLit);
            return strLit;
        }
        case TokenType::kIdent: {
            Node *name = create_node(l, NodeKind::kName);
            name->name.ident = tkn->ident;
            lex_next(l);  // next 'ident'
            name->name.ref = nullptr;
            end_node(l, name);
            return name;
        }
        case TokenType::kLParen: {
            size_t lparenStartI = tkn->startI;
            lex_next(l);  // next (
            Node *expr = parse_expr(l);
            if (!expr) return nullptr;
            if (!check_peek(l, TokenType::kRParen)) {
                dx_err(at_point(l->finfo, lparenStartI), "Expected matching )\n");
                return nullptr;
            }
            lex_next(l);  // next )
            return expr;
        }
        case TokenType::kColon: {
            Node *func = create_node(l, NodeKind::kFunc);
            func->func.params = {};
            lex_next(l);  // next :
            if (!check_peek(l, TokenType::kLParen)) {
                dx_err(at_token(l->finfo, lex_peek(l)), "Expected ( for parameter list of function\n");
                return nullptr;
            }
            lex_next(l);  // next (
            for (;;) {
                Token *tkn = lex_peek(l);
                if (tkn->type == TokenType::kErr) return nullptr;
                if (tkn->type == TokenType::kEof) {
                    dx_err(at_token(l->finfo, tkn), "Expected parameter declaration but reached end of file\n");
                    return nullptr;
                }
                if (tkn->type == TokenType::kRParen) {
                    lex_next(l);  // next )
                    break;
                }

                if (!check_peek(l, TokenType::kIdent)) {
                    dx_err(at_token(l->finfo, lex_peek(l)), "Expected parameter declaration\n");
                    return nullptr;
                }
                Node *lval = parse_expr(l);
                if (!lval) return nullptr;
                Node *param = parse_decl_from_lval(l, lval);
                if (!param) return nullptr;
                func->func.params.add(param);

                if (check_peek(l, TokenType::kComma)) {
                    lex_next(l);  // next ,
                } else if (check_peek(l, TokenType::kRParen)) {
                    lex_next(l);  // next )
                    break;
                } else {
                    dx_err(at_token(l->finfo, lex_peek(l)), "Expected , to separate function parameters\n");
                    return nullptr;
                }
            }
            if (check_peek(l, TokenType::kArrow)) {
                lex_next(l);  // next ->
                func->func.retTy = parse_type(l);
                if (!func->func.retTy) return nullptr;
            } else {
                func->func.retTy = nullptr;
            }

            if (check_peek(l, TokenType::kRetArrow)) {
                lex_next(l);  // next =>
                func->func.body = parse_expr(l);
            } else if (check_peek(l, TokenType::kLCurl)) {
                func->func.body = parse_block(l);
            } else {
                dx_err(at_token(l->finfo, lex_peek(l)), "Expected { or => to define function body\n");
                return nullptr;
            }
            if (!func->func.body) return nullptr;
            end_node(l, func);
            return func;
        }
        case TokenType::kIf:
        case TokenType::kWhile:
            dx_err(at_token(l->finfo, tkn), "%s not allowed here\n", token_type_string(tkn->type));
            return nullptr;
        case TokenType::kEof: dx_err(at_eof(l), "Expected an operand but reached end of file\n");
        case TokenType::kErr: return nullptr;
        default:
            dx_err(at_token(l->finfo, tkn), "Expected an operand but instead got %s\n", token_type_string(tkn->type));
            return nullptr;
    }
}

Node *parse_infix(Lexer *l, int lprec, Node *left) {
    while (left) {
        TokenType op = lex_peek(l)->type;
        if (op == TokenType::kErr) return nullptr;

        int rprec = get_precedence(op);
        if (lprec >= rprec) break;

        switch (op) {
            case TokenType::kAdd:
            case TokenType::kSubNeg:
            case TokenType::kBitAnd:
            case TokenType::kDot: {
                Node *infix = create_node(l, NodeKind::kInfix);
                align_node_start(infix, left->startI);
                infix->infix.left = left;
                infix->infix.op = op;
                lex_next(l);  // next infix operator
                infix->infix.right = parse_infix(l, rprec, parse_operand(l));
                end_node(l, infix);
                left = infix;
                if (!infix->infix.right) return nullptr;
                break;
            }
            case TokenType::kAddAdd:
            case TokenType::kSubSub: {
                todo("Implement postfix operators at line %d\n", l->line);
                return nullptr;
            }
            case TokenType::kLParen: {
                Node *call = create_node(l, NodeKind::kCall);
                align_node_start(call, left->startI);
                call->call.args = {};
                call->call.callee = left;
                lex_next(l);  // next (
                for (;;) {
                    Token *tkn = lex_peek(l);
                    if (tkn->type == TokenType::kErr) return nullptr;
                    if (tkn->type == TokenType::kEof) {
                        dx_err(at_token(l->finfo, tkn), "Expected argument expression but reached end of file\n");
                        return nullptr;
                    }
                    if (tkn->type == TokenType::kRParen) {
                        lex_next(l);  // next )
                        break;
                    }

                    Node *arg = parse_expr(l);
                    if (!arg) return nullptr;
                    call->call.args.add(arg);

                    if (check_peek(l, TokenType::kComma)) {
                        lex_next(l);  // next ,
                    } else if (check_peek(l, TokenType::kRParen)) {
                        lex_next(l);  // next )
                        break;
                    } else {
                        dx_err(at_token(l->finfo, lex_peek(l)), "Expected , to separate call argments\n");
                        return nullptr;
                    }
                }
                end_node(l, call);
                left = call;
                break;
            }
            default: dx_err(at_token(l->finfo, lex_peek(l)), "Expected an infix operator\n"); return nullptr;
        }
    }
    return left;
}

Node *parse_expr(Lexer *l) { return parse_infix(l, kLowPrecedence, parse_operand(l)); }

Node *parse_if(Lexer *l) {
    assert(check_peek(l, TokenType::kIf));
    Node *ifstmt = create_node(l, NodeKind::kIf);
    ifstmt->ifstmt.branchLevel = (size_t)-1;
    lex_next(l);  // next if

    ifstmt->ifstmt.cond = parse_expr(l);
    if (!ifstmt->ifstmt.cond) return nullptr;

    if (!check_peek(l, TokenType::kLCurl)) {
        dx_err(at_token(l->finfo, lex_peek(l)), "Expected { to define body of if statement\n");
        return nullptr;
    }
    ifstmt->ifstmt.then = parse_block(l);
    if (!ifstmt->ifstmt.then) return nullptr;

    if (check_peek(l, TokenType::kElse)) {
        lex_next(l);  // next else
        if (check_peek(l, TokenType::kIf)) {
            ifstmt->ifstmt.alt = parse_if(l);
        } else if (check_peek(l, TokenType::kLCurl)) {
            ifstmt->ifstmt.alt = parse_block(l);
        } else {
            dx_err(at_token(l->finfo, lex_peek(l)), "Expected { to define else body or if keyword to define else if\n");
            return nullptr;
        }
        if (!ifstmt->ifstmt.alt) return nullptr;
    } else {
        ifstmt->ifstmt.alt = nullptr;
    }

    end_node(l, ifstmt);
    return ifstmt;
}

Node *parse_while(Lexer *l) {
    assert(check_peek(l, TokenType::kWhile));
    Node *whilestmt = create_node(l, NodeKind::kWhile);
    whilestmt->whilestmt.label.src = nullptr;
    whilestmt->whilestmt.branchLevel = (size_t)-1;
    lex_next(l);  // next while

    whilestmt->whilestmt.cond = parse_expr(l);
    if (!whilestmt->whilestmt.cond) return nullptr;

    if (!check_peek(l, TokenType::kLCurl)) {
        dx_err(at_token(l->finfo, lex_peek(l)), "Expected { to define body of while statement\n");
        return nullptr;
    }
    whilestmt->whilestmt.loop = parse_block(l);
    if (!whilestmt->whilestmt.loop) return nullptr;

    end_node(l, whilestmt);
    return whilestmt;
}

Node *parse_block(Lexer *l) {
    assert(check_peek(l, TokenType::kLCurl));
    Node *block = create_node(l, NodeKind::kBlock);
    block->block.stmts = {};
    block->block.branchLevel = (size_t)-1;
    lex_next(l);  // next {

    for (;;) {
        Token *tkn = lex_peek(l);
        if (tkn->type == TokenType::kRCurl) {
            lex_next(l);  // next }
            break;
        }
        switch (tkn->type) {
            case TokenType::kLCurl:
                if (Node *innerBlock = parse_block(l)) {
                    block->block.stmts.add(innerBlock);
                } else {
                    return nullptr;
                }
                break;
            case TokenType::kIf:
                if (Node *ifstmt = parse_if(l)) {
                    block->block.stmts.add(ifstmt);
                } else {
                    return nullptr;
                }
                break;
            case TokenType::kLabel: {
                lex_next(l);  // next ::
                Token label = *lex_peek(l);
                if (!check_peek(l, TokenType::kIdent)) {
                    dx_err(at_token(l->finfo, &label), "Expected label name\n");
                    return nullptr;
                }
                lex_next(l);  // next 'ident'
                if (Node *whilestmt = parse_while(l)) {
                    whilestmt->whilestmt.label = label.ident;
                    block->block.stmts.add(whilestmt);
                } else {
                    return nullptr;
                }
                break;
            }
            case TokenType::kWhile:
                if (Node *whilestmt = parse_while(l)) {
                    block->block.stmts.add(whilestmt);
                } else {
                    return nullptr;
                }
                break;
            case TokenType::kRet: {
                Node *ret = create_node(l, NodeKind::kRet);
                ret->ret.value = nullptr;  // Return values are obtained in following statements
                ret->ret.resolvedTy = nullptr;
                lex_next(l);  // next ret
                end_node(l, ret);
                block->block.stmts.add(ret);
                break;
            }
            case TokenType::kBreak:
            case TokenType::kCont: {
                Node *loopbr = create_node(l, NodeKind::kLoopBr);
                loopbr->loopbr.isBreak = check_peek(l, TokenType::kBreak);
                lex_next(l);  // next break or cont
                if (check_peek(l, TokenType::kErr)) return nullptr;
                if (check_peek(l, TokenType::kIdent)) {
                    loopbr->loopbr.label = lex_peek(l)->ident;
                    lex_next(l);  // next 'ident'
                } else {
                    loopbr->loopbr.label.src = nullptr;
                }
                end_node(l, loopbr);
                block->block.stmts.add(loopbr);
                break;
            }
            case TokenType::kEof:
                dx_err(at_eof(l), "Expected another statement but reached the end of the file\n");
                dx_note(at_node(l->finfo, block), "Block starts here\n");
                return nullptr;
            case TokenType::kErr: return nullptr;
            default:
                if (Node *declOrExpr = parse_decl_or_expr(l)) {
                    // Set return value here for better error ouput
                    if (declOrExpr->kind != NodeKind::kDecl) {
                        if (block->block.stmts.size) {
                            Node *ret = block->block.stmts.last();
                            if (ret->kind == NodeKind::kRet) {
                                ret->ret.value = declOrExpr;
                                end_node(l, ret);
                                break;
                            }
                        }
                    }
                    block->block.stmts.add(declOrExpr);
                } else {
                    return nullptr;
                }
        }
    }
    end_node(l, block);
    return block;
}

}  // namespace

Result parse_file(File *file) {
    Lexer l{};
    l.finfo = file->finfo;
    lex_next(&l);  // Grab first lexer token
    for (;;) {
        if (check_peek(&l, TokenType::kEof)) return kAccept;
        if (check_peek(&l, TokenType::kErr)) return kError;
        if (check_peek(&l, TokenType::kDirective)) {
            LStringView importDirective{"import", 6};
            if (parse_directive(&l, importDirective)) return kError;

            Node *import = parse_import(&l);
            if (!import) return kError;
            if (Node **otherImport = file->imports.try_put(import->import.alias, import)) {
                dx_err(at_node(l.finfo, import), "Duplicate import alias: %s\n", lstr_raw_str(import->import.alias));
                dx_err(at_node(l.finfo, *otherImport), "Previous import here\n");
                return kError;
            }
        } else if (Node *declOrExpr = parse_decl_or_expr(&l)) {
            if (declOrExpr->kind == NodeKind::kDecl) {
                if (!declOrExpr->decl.isDecl) {
                    dx_err(at_node(l.finfo, declOrExpr), "Assignment cannot be in global scope\n",
                           node_kind_string(declOrExpr->kind));
                    return kError;
                }
                declOrExpr->decl.file = file;

                if (declOrExpr->decl.lval->kind != NodeKind::kName) {
                    dx_err(at_node(file->finfo, declOrExpr->decl.lval),
                           "Must only declare variable names in global scope\n");
                    return kError;
                }

                LStringView &declName = declOrExpr->decl.lval->name.ident;
                if (Node **other = file->package->globalDecls.try_put(declName, declOrExpr)) {
                    Node *otherDecl = *other;
                    dx_err(at_node(file->finfo, declOrExpr->decl.lval), "Duplicate declaration\n");
                    dx_note(at_node(otherDecl->decl.file->finfo, otherDecl->decl.lval), "Previous declaration here\n");
                    return kError;
                }
                if (Node **other = file->imports.get(declName)) {
                    Node *otherImport = *other;
                    dx_err(at_node(file->finfo, otherImport),
                           "Import alias cannot have the same name as a declaration\n");
                    dx_note(at_node(declOrExpr->decl.file->finfo, declOrExpr), "Declaration found here\n");
                    return kError;
                }
                declOrExpr->decl.isBound = true;

                if (!declOrExpr->decl.rval && !declOrExpr->decl.isExtern) {
                    dx_err(at_node(file->finfo, declOrExpr),
                           "Global declaration must be defined with a value or marked as #extern\n");
                    return kError;
                }
            } else {
                dx_err(at_node(l.finfo, declOrExpr), "%s expression cannot be in global scope\n",
                       node_kind_string(declOrExpr->kind));
                return kError;
            }
        } else {
            return kError;
        }
    }
    return kAccept;
}

}  // namespace lcc
