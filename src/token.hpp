#ifndef LCC_LEXER_HPP
#define LCC_LEXER_HPP

#include <cstdint>

#include "file.hpp"
#include "lstring.hpp"

namespace lcc {

enum class TokenType {
    kDirective,
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
    kDot,
    kLParen,
    kRParen,
    kLCurl,
    kRCurl,
    kColon,
    kType,
    kAssign,
    kComma,
    kArrow,
    kRetArrow,
    kIf,
    kElse,
    kLabel,
    kWhile,
    kRet,
    kBreak,
    kCont,
    kEof,
    kErr,
};

constexpr const char *token_type_string(TokenType type) {
    switch (type) {
        case TokenType::kDirective: return "#";
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
        case TokenType::kDot: return ".";
        case TokenType::kLParen: return "(";
        case TokenType::kRParen: return ")";
        case TokenType::kLCurl: return "{";
        case TokenType::kRCurl: return "}";
        case TokenType::kColon: return ":";
        case TokenType::kType: return "type";
        case TokenType::kAssign: return "=";
        case TokenType::kComma: return ",";
        case TokenType::kArrow: return "->";
        case TokenType::kRetArrow: return "=>";
        case TokenType::kIf: return "if";
        case TokenType::kElse: return "else";
        case TokenType::kLabel: return "::";
        case TokenType::kWhile: return "while";
        case TokenType::kRet: return "ret";
        case TokenType::kBreak: return "break";
        case TokenType::kCont: return "continue";
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
        uint16_t intVal;

        LStringView str;
        LStringView ident;
    };
};

struct Lexer {
    FileInfo *finfo;

    size_t line{1};
    size_t curI{0};
    size_t curLen{0};
    Token curToken;
};

bool is_eof(Lexer *l);

bool is_whitespace(char c);

Token *lex_next(Lexer *l);

Token *lex_peek(Lexer *l);

}  // namespace lcc

#endif
