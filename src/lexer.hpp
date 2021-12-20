#ifndef LCC_LEXER_HPP
#define LCC_LEXER_HPP

#include "lstring.hpp"
#include "util.hpp"

namespace lcc {

enum class TokenType {
    kAdd,
    kAddAdd,
    kAddEq,
    kSubNeg,
    kSubSub,
    kSubEq,
    kBitAnd,
    kBitAndEq,
    kIntLiteral,
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
    kWhile,
    kRet,
    kEof,
    kErr,
};

const char *token_type_string(TokenType type);

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
