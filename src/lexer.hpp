#ifndef LCC_LEXER_HPP
#define LCC_LEXER_HPP

#include "lstring.hpp"
#include "util.hpp"

namespace lcc {

enum class TokenType {
    kImport,
    kAdd,
    kAddAdd,
    kAddEq,
    kSubNeg,
    kSubSub,
    kSubEq,
    kBitAnd,
    kBitAndEq,
    kIntLiteral,
    kStrLiteral,
    kIdent,
    kPtr,
    kDeref,
    kLParen,
    kRParen,
    kLCurl,
    kRCurl,
    kAssign,
    kColon,
    kComma,
    kArrow,
    kRetArrow,
    kIf,
    kElse,
    kWhile,
    kRet,
    kEof,
    kErr,
};

constexpr const char *token_type_string(TokenType type) {
    switch (type) {
        case TokenType::kImport: return "import";
        case TokenType::kAdd: return "+";
        case TokenType::kAddAdd: return "++";
        case TokenType::kAddEq: return "+=";
        case TokenType::kSubNeg: return "-";
        case TokenType::kSubSub: return "--";
        case TokenType::kSubEq: return "-=";
        case TokenType::kBitAnd: return "&";
        case TokenType::kBitAndEq: return "&=";
        case TokenType::kIntLiteral: return "'integer'";
        case TokenType::kStrLiteral: return "'string'";
        case TokenType::kIdent: return "'identifier'";
        case TokenType::kPtr: return "*";
        case TokenType::kDeref: return "@";
        case TokenType::kLParen: return "(";
        case TokenType::kRParen: return ")";
        case TokenType::kLCurl: return "{";
        case TokenType::kRCurl: return "}";
        case TokenType::kAssign: return "=";
        case TokenType::kColon: return ":";
        case TokenType::kComma: return ",";
        case TokenType::kArrow: return "->";
        case TokenType::kRetArrow: return "=>";
        case TokenType::kIf: return "if";
        case TokenType::kElse: return "else";
        case TokenType::kWhile: return "while";
        case TokenType::kRet: return "ret";
        case TokenType::kEof: return "'eof'";
        case TokenType::kErr: return "'error'";
    }
}

struct Token {
    TokenType type;
    size_t line;
    size_t startI;
    size_t len;

    union {
        u16 intVal;

        LStringView str;
        LStringView ident;
    } data;
};

struct Lexer {
    LString *src;

    size_t line;
    size_t curI;
    size_t curLen;
    Token curToken;
};

Lexer *lexer_init(LString *src);
bool is_eof(Lexer *l);
bool is_whitespace(char c);
Token *lex_next(Lexer *l);
Token *lex_peek(Lexer *l);

}  // namespace lcc

#endif
