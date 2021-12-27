#include "parse.hpp"

#include "diagnostics.hpp"
#include "file.hpp"
#include "print.hpp"

namespace lcc {

namespace {

bool check_peek(Lexer *l, TokenType type) { return lex_peek(l)->type == type; }

Node *create_node(Lexer *l, NodeType type) {
    Node *node = mem::malloc<Node>();
    node->startI = lex_peek(l)->startI;
    node->type = type;
    return node;
}

void align_node_start(Node *node, size_t startI) { node->startI = startI; }

void end_node(Lexer *l, Node *node) { node->endI = lex_peek(l)->startI - 1; }

Node *parse_decl_from_lval(Lexer *l, Node *lval);

Node *parse_type(Lexer *l);

Node *parse_operand(Lexer *l);
Node *parse_infix(Lexer *l, u8 prec, Node *left);
Node *parse_expr(Lexer *l);

Node *parse_block(Lexer *l);

Node *parse_decl_or_expr(Lexer *l) {
    Node *expr = parse_expr(l);
    if (!expr) return nullptr;

    if (check_peek(l, TokenType::kColon) || check_peek(l, TokenType::kAssign)) {
        return parse_decl_from_lval(l, expr);
    }
    return expr;
}

Node *parse_decl_from_lval(Lexer *l, Node *lval) {
    switch (lval->type) {
        case NodeType::kName: break;
        case NodeType::kPrefix:
            if (lval->data.prefix.op == TokenType::kDeref) break;
            dx_err(at_node(l->finfo, lval), "%s prefix expression not allowed as lvalue\n",
                   token_type_string(lval->data.prefix.op));
            return nullptr;
        default:
            dx_err(at_node(l->finfo, lval), "%s expression not allowed as lvalue\n", node_type_string(lval->type));
            return nullptr;
    }

    Node *decl = create_node(l, NodeType::kDecl);
    align_node_start(decl, lval->startI);
    decl->data.decl.lval = lval;
    decl->data.decl.isDecl = false;
    decl->data.decl.isChecked = false;
    decl->data.decl.resolvedTy = nullptr;

    bool hasType = check_peek(l, TokenType::kColon);
    if (hasType) {
        decl->data.decl.isDecl = true;
        lex_next(l);  // next :
        decl->data.decl.staticTy = parse_type(l);
        if (!decl->data.decl.staticTy) return nullptr;
    } else {
        decl->data.decl.staticTy = nullptr;
    }
    bool hasAssignment = check_peek(l, TokenType::kAssign);
    if (hasAssignment) {
        lex_next(l);  // next =
        decl->data.decl.rval = parse_expr(l);
        if (!decl->data.decl.rval) return nullptr;
    } else {
        decl->data.decl.rval = nullptr;
    }
    if (!hasType && !hasAssignment) {
        dx_err(at_node(l->finfo, decl->data.decl.lval), "Declaration must have either a type or value\n");
        return nullptr;
    }

    end_node(l, decl);
    return decl;
}

constexpr BaseTypeKind kBaseTypes[] = {BaseTypeKind::u16};

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
                for (k = 0; k < tkn->data.ident.len; k++) {
                    if (tkn->data.ident.src[k] != baseType[k]) break;
                }
                if (k == tkn->data.ident.len) {
                    dest->data.base.kind = kBaseTypes[i];
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

            dest->data.ptr.inner = mem::malloc<Type>();
            if (r_parse_type(l, dest->data.ptr.inner)) return true;
            break;
        }
        case TokenType::kLParen: {
            dest->kind = TypeKind::kFuncTy;
            dest->data.func.paramTys = {};
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
                dest->data.func.paramTys.add(argTy);

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
                dest->data.func.retTy = mem::malloc<Type>();
                if (r_parse_type(l, dest->data.func.retTy)) return true;
            } else {
                dest->data.func.retTy = &builtin_type::none;
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
    Node *type = create_node(l, NodeType::kType);
    if (r_parse_type(l, &type->data.type)) return nullptr;
    end_node(l, type);
    return type;
}

constexpr u8 kLowPrecedence = 0;
constexpr u8 kPrefixPrecedence = 3;
constexpr u8 kPostfixPrecedence = 4;

u8 get_precedence(TokenType op) {
    switch (op) {
        case TokenType::kAddAdd:
        case TokenType::kSubSub:
        case TokenType::kLParen: return kPostfixPrecedence;  // Function call
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
            Node *prefix = create_node(l, NodeType::kPrefix);
            prefix->data.prefix.op = tkn->type;
            lex_next(l);  // next 'op'
            prefix->data.prefix.inner = parse_infix(l, kPrefixPrecedence, parse_operand(l));
            end_node(l, prefix);
            if (!prefix->data.prefix.inner) return nullptr;
            return prefix;
        }
        case TokenType::kIntLiteral: {
            Node *intLit = create_node(l, NodeType::kIntLit);
            intLit->data.intLit.intVal = tkn->data.intVal;
            lex_next(l);  // next 'intlit'
            end_node(l, intLit);
            return intLit;
        }
        case TokenType::kStrLiteral: {
            Node *strLit = create_node(l, NodeType::kStrLit);
            strLit->data.strLit.strVal = tkn->data.str;
            lex_next(l);  // next 'intlit'
            end_node(l, strLit);
            return strLit;
        }
        case TokenType::kIdent: {
            Node *name = create_node(l, NodeType::kName);
            name->data.name.ident = tkn->data.ident;
            lex_next(l);  // next 'ident'
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
            Node *func = create_node(l, NodeType::kFunc);
            func->data.func.params = {};
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
                func->data.func.params.add(param);

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
                func->data.func.retTy = parse_type(l);
                if (!func->data.func.retTy) return nullptr;
            } else {
                func->data.func.retTy = nullptr;
            }

            if (check_peek(l, TokenType::kRetArrow)) {
                lex_next(l);  // next =>
                func->data.func.body = parse_expr(l);
            } else if (check_peek(l, TokenType::kLCurl)) {
                func->data.func.body = parse_block(l);
            } else {
                dx_err(at_token(l->finfo, lex_peek(l)), "Expected { or => to define function body\n");
                return nullptr;
            }
            if (!func->data.func.body) return nullptr;
            end_node(l, func);
            return func;
        }
        case TokenType::kEof: dx_err(at_eof(l), "Expected an operand but reached end of file\n");
        case TokenType::kErr: return nullptr;
        default:
            dx_err(at_token(l->finfo, tkn), "Expected an operand but instead got %s\n", token_type_string(tkn->type));
            return nullptr;
    }
}

Node *parse_infix(Lexer *l, u8 lprec, Node *left) {
    while (left) {
        TokenType op = lex_peek(l)->type;
        if (op == TokenType::kErr) return nullptr;

        u8 rprec = get_precedence(op);
        if (lprec >= rprec) break;

        switch (op) {
            case TokenType::kAdd:
            case TokenType::kSubNeg:
            case TokenType::kBitAnd: {
                Node *infix = create_node(l, NodeType::kInfix);
                align_node_start(infix, left->startI);
                infix->data.infix.left = left;
                infix->data.infix.op = op;
                lex_next(l);  // next infix operator
                infix->data.infix.right = parse_infix(l, rprec, parse_operand(l));
                end_node(l, infix);
                left = infix;
                if (!infix->data.infix.right) return nullptr;
                break;
            }
            case TokenType::kAddAdd:
            case TokenType::kSubSub: {
                todo("Implement postfix operators at line %d\n", l->line);
                return nullptr;
            }
            case TokenType::kLParen: {
                Node *call = create_node(l, NodeType::kCall);
                align_node_start(call, left->startI);
                call->data.call.args = {};
                call->data.call.callee = left;
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
                    call->data.call.args.add(arg);

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
    Node *ifstmt = create_node(l, NodeType::kIf);
    lex_next(l);  // next if

    ifstmt->data.ifstmt.cond = parse_expr(l);
    if (!ifstmt->data.ifstmt.cond) return nullptr;

    if (!check_peek(l, TokenType::kLCurl)) {
        dx_err(at_token(l->finfo, lex_peek(l)), "Expected { to define body of if statement\n");
        return nullptr;
    }
    ifstmt->data.ifstmt.then = parse_block(l);
    if (!ifstmt->data.ifstmt.then) return nullptr;

    if (check_peek(l, TokenType::kElse)) {
        lex_next(l);  // next else
        if (check_peek(l, TokenType::kIf)) {
            ifstmt->data.ifstmt.alt = parse_if(l);
        } else if (check_peek(l, TokenType::kLCurl)) {
            ifstmt->data.ifstmt.alt = parse_block(l);
        } else {
            dx_err(at_token(l->finfo, lex_peek(l)), "Expected { to define else body or if keyword to define else if\n");
            return nullptr;
        }
        if (!ifstmt->data.ifstmt.alt) return nullptr;
    } else {
        ifstmt->data.ifstmt.alt = nullptr;
    }

    end_node(l, ifstmt);
    return ifstmt;
}

Node *parse_while(Lexer *l) {
    assert(check_peek(l, TokenType::kWhile));
    Node *whilestmt = create_node(l, NodeType::kWhile);
    lex_next(l);  // next while

    whilestmt->data.whilestmt.cond = parse_expr(l);
    if (!whilestmt->data.whilestmt.cond) return nullptr;

    if (!check_peek(l, TokenType::kLCurl)) {
        dx_err(at_token(l->finfo, lex_peek(l)), "Expected { to define body of while statement\n");
        return nullptr;
    }
    whilestmt->data.whilestmt.loop = parse_block(l);
    if (!whilestmt->data.whilestmt.loop) return nullptr;

    end_node(l, whilestmt);
    return whilestmt;
}

Node *parse_block(Lexer *l) {
    assert(check_peek(l, TokenType::kLCurl));
    Node *block = create_node(l, NodeType::kBlock);
    block->data.block.stmts = {};
    lex_next(l);  // next {

    for (;;) {
        Token *tkn = lex_peek(l);
        if (tkn->type == TokenType::kRCurl) {
            lex_next(l);  // next }
            break;
        }
        switch (tkn->type) {
            case TokenType::kIf:
                if (Node *ifstmt = parse_if(l)) {
                    block->data.block.stmts.add(ifstmt);
                } else {
                    return nullptr;
                }
                break;
            case TokenType::kWhile:
                if (Node *whilestmt = parse_while(l)) {
                    block->data.block.stmts.add(whilestmt);
                } else {
                    return nullptr;
                }
                break;
            case TokenType::kRet: {
                Node *ret = create_node(l, NodeType::kRet);
                lex_next(l);  // next ret
                if (!check_peek(l, TokenType::kRCurl)) {
                    ret->data.ret.value = parse_expr(l);
                    if (!ret->data.ret.value) return nullptr;
                } else {
                    ret->data.ret.value = nullptr;
                }
                end_node(l, ret);
                block->data.block.stmts.add(ret);
                break;
            }
            case TokenType::kEof:
                dx_err(at_eof(l), "Expected another statement but reached the end of the file\n");
                return nullptr;
            case TokenType::kErr: return nullptr;
            default:
                if (Node *declOrExpr = parse_decl_or_expr(l)) {
                    block->data.block.stmts.add(declOrExpr);
                } else {
                    return nullptr;
                }
        }
    }
    end_node(l, block);
    return block;
}

}  // namespace

Node *parse_unit(Lexer *l) {
    Node *unit = create_node(l, NodeType::kUnit);
    unit->data.unit.imports = {};
    unit->data.unit.decls = {};
    lex_next(l);  // Grab first lexer token
    while (lex_peek(l)->type != TokenType::kEof) {
        Token *tkn = lex_peek(l);
        if (tkn->type == TokenType::kErr) return nullptr;

        if (tkn->type == TokenType::kImport) {
            lex_next(l);  // next import

            tkn = lex_peek(l);
            if (tkn->type == TokenType::kErr) return nullptr;
            if (tkn->type != TokenType::kStrLiteral) {
                dx_err(at_token(l->finfo, tkn), "Expected file name after import keyword\n");
                return nullptr;
            }
            unit->data.unit.imports.add(tkn->data.ident);
            lex_next(l);  // next ident
        } else if (Node *declOrExpr = parse_decl_or_expr(l)) {
            if (declOrExpr->type == NodeType::kDecl) {
                declOrExpr->data.decl.isDecl = true;
                unit->data.unit.decls.add(declOrExpr);
            } else {
                dx_err(at_node(l->finfo, declOrExpr), "%s expression cannot be in global scope\n",
                       node_type_string(declOrExpr->type));
                return nullptr;
            }
        } else {
            return nullptr;
        }
    }
    end_node(l, unit);
    return unit;
}

}  // namespace lcc
