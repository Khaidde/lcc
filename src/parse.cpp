#include "parse.hpp"

#include "diagnostics.hpp"

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

Node *parse_decl_from_ident(Lexer *l, Token &ident);

Node *parse_type(Lexer *l);

constexpr u8 kLowPrecedence = 0;
constexpr u8 kHighPrecedence = 0xFF;
Node *parse_operand(Lexer *l);
Node *parse_infix(Lexer *l, u8 prec, Node *left);
Node *parse_expr(Lexer *l);

Node *parse_block(Lexer *l);

Node *parse_decl_or_expr(Lexer *l) {
    assert(check_peek(l, TokenType::kIdent));
    Token ident = *lex_peek(l);
    lex_next(l);  // next 'ident'

    if (check_peek(l, TokenType::kColon) || check_peek(l, TokenType::kAssign)) {
        return parse_decl_from_ident(l, ident);
    } else {
        Node *name = create_node(l, NodeType::kName);
        align_node_start(name, ident.startI);
        name->data.name.ident = ident.data.ident;
        end_node(l, name);
        return parse_infix(l, kLowPrecedence, name);
    }
}

Node *parse_decl_from_ident(Lexer *l, Token &ident) {
    Node *decl = create_node(l, NodeType::kDecl);
    align_node_start(decl, ident.startI);
    decl->data.decl.name = ident.data.ident;

    bool hasType = check_peek(l, TokenType::kColon);
    if (hasType) {
        lex_next(l);  // next :
        decl->data.decl.type = parse_type(l);
        if (!decl->data.decl.type) return nullptr;
    }
    bool hasAssignment = check_peek(l, TokenType::kAssign);
    if (hasAssignment) {
        lex_next(l);  // next =
        decl->data.decl.value = parse_expr(l);
        if (!decl->data.decl.value) return nullptr;
    }
    if (!hasType && !hasAssignment) {
        dx_err(l, at_token(&ident), "Declaration must have either a type or value\n");
        return nullptr;
    }

    end_node(l, decl);
    return decl;
}

Node *parse_type(Lexer *l) {
    Token *tkn = lex_peek(l);
    switch (tkn->type) {
        case TokenType::kIdent: {
            Node *name = create_node(l, NodeType::kName);
            name->data.name.ident = tkn->data.ident;
            lex_next(l);  // next 'ident'
            end_node(l, name);
            return name;
        }
        case TokenType::kPtr: {
            Node *ptr = create_node(l, NodeType::kPrefix);
            lex_next(l);  // next *
            ptr->data.prefix.op = TokenType::kPtr;
            ptr->data.prefix.inner = parse_type(l);
            end_node(l, ptr);
            if (!ptr->data.prefix.inner) return nullptr;
            return ptr;
        }
        case TokenType::kLParen: {
            todo("Implement function type parsing");
            return nullptr;
        }
        case TokenType::kEof: dx_err(l, at_token(tkn), "Expected a type but reached end of file\n");
        case TokenType::kErr: return nullptr;
        default:
            dx_err(l, at_token(tkn), "Expected a type but instead got %s\n", token_type_string(tkn->type));
            return nullptr;
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
            prefix->data.prefix.inner = parse_infix(l, kHighPrecedence, parse_operand(l));
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
        case TokenType::kIdent: {
            Node *name = create_node(l, NodeType::kName);
            name->data.name.ident = tkn->data.ident;
            lex_next(l);  // next 'ident'
            end_node(l, name);
            return name;
        }
        case TokenType::kColon: {
            Node *func = create_node(l, NodeType::kFunc);
            lex_next(l);  // next :
            if (!check_peek(l, TokenType::kLParen)) {
                dx_err(l, at_token(lex_peek(l)), "Expected ( for parameter list of function\n");
                return nullptr;
            }
            lex_next(l);  // next (
            for (;;) {
                Token *tkn = lex_peek(l);
                if (tkn->type == TokenType::kErr) return nullptr;
                if (tkn->type == TokenType::kEof) {
                    dx_err(l, at_token(lex_peek(l)), "Expected parameter declaration but reached end of file\n");
                    return nullptr;
                }
                if (tkn->type == TokenType::kRParen) {
                    lex_next(l);  // next )
                    break;
                }

                if (!check_peek(l, TokenType::kIdent)) {
                    dx_err(l, at_token(lex_peek(l)), "Expected parameter declaration\n");
                    return nullptr;
                }

                Token ident = *lex_peek(l);
                lex_next(l);  // next 'ident'
                Node *param = parse_decl_from_ident(l, ident);
                if (!param) return nullptr;
                func->data.func.params.add(param);

                if (check_peek(l, TokenType::kComma)) {
                    lex_next(l);  // next ,
                } else if (check_peek(l, TokenType::kRParen)) {
                    lex_next(l);  // next )
                    break;
                } else {
                    dx_err(l, at_token(lex_peek(l)), "Expected , to separate function parameters\n");
                    return nullptr;
                }
            }
            if (check_peek(l, TokenType::kArrow)) {
                lex_next(l);  // next ->
                func->data.func.retType = parse_type(l);
                if (!func->data.func.retType) return nullptr;
            }

            if (check_peek(l, TokenType::kRetArrow)) {
                lex_next(l);  // next =>
                func->data.func.body = parse_expr(l);
            } else if (check_peek(l, TokenType::kLCurl)) {
                func->data.func.body = parse_block(l);
            } else {
                dx_err(l, at_token(lex_peek(l)), "Expected { or => to define function body\n");
                return nullptr;
            }
            if (!func->data.func.body) return nullptr;
            end_node(l, func);
            return func;
        }
        case TokenType::kEof: dx_err(l, at_eof(l), "Expected an operand but reached end of file\n");
        case TokenType::kErr: return nullptr;
        default:
            dx_err(l, at_token(tkn), "Expected an operand but instead got %s\n", token_type_string(tkn->type));
            return nullptr;
    }
}

u8 get_precedence(TokenType op) {
    switch (op) {
        case TokenType::kBitAnd: return 2;
        case TokenType::kAdd:
        case TokenType::kSubNeg: return 1;
        default: return kLowPrecedence;
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
            default: dx_err(l, at_token(lex_peek(l)), "Expected an infix operator\n"); return nullptr;
        }
    }
    return left;
}

Node *parse_expr(Lexer *l) { return parse_infix(l, kLowPrecedence, parse_operand(l)); }

Node *parse_block(Lexer *l) {
    assert(check_peek(l, TokenType::kLCurl));
    Node *block = create_node(l, NodeType::kBlock);
    lex_next(l);  // next {

    for (;;) {
        Token *tkn = lex_peek(l);
        if (tkn->type == TokenType::kRCurl) {
            lex_next(l);  // next }
            break;
        }
        switch (tkn->type) {
            case TokenType::kIdent:
                if (Node *declOrExpr = parse_decl_or_expr(l)) {
                    block->data.block.stmts.add(declOrExpr);
                    break;
                }
                return nullptr;
            case TokenType::kIf: todo("Implement if statments\n"); return nullptr;
            case TokenType::kWhile: todo("Implement while statments\n"); return nullptr;
            case TokenType::kRet: {
                Node *ret = create_node(l, NodeType::kRet);
                lex_next(l);  // next ret
                if (!check_peek(l, TokenType::kRCurl)) {
                    ret->data.ret.value = parse_expr(l);
                    if (!ret->data.ret.value) return nullptr;
                }
                end_node(l, ret);
                block->data.block.stmts.add(ret);
                break;
            }
            case TokenType::kEof:
                dx_err(l, at_eof(l), "Expected another statement but reached the end of the file\n");
                return nullptr;
            case TokenType::kErr: return nullptr;
            default: return parse_expr(l);
        }
    }
    end_node(l, block);
    return block;
}

}  // namespace

Node *parse(Lexer *l) {
    Node *unit = create_node(l, NodeType::kUnit);
    while (lex_peek(l)->type != TokenType::kEof) {
        switch (lex_peek(l)->type) {
            case TokenType::kIdent:
                if (Node *declOrExpr = parse_decl_or_expr(l)) {
                    if (declOrExpr->type == NodeType::kDecl) {
                        unit->data.unit.decls.add(declOrExpr);
                    } else {
                        dx_err(l, at_node(l, declOrExpr), "%s expression cannot be in global scope\n",
                               node_type_string(declOrExpr->type));
                        return nullptr;
                    }
                } else {
                    return nullptr;
                }
                break;
            case TokenType::kErr: return nullptr;
            default: dx_err(l, at_token(lex_peek(l)), "Expected name for declaration\n"); return nullptr;
        }
    }
    end_node(l, unit);
    return unit;
}

}  // namespace lcc
