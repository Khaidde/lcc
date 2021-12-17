#ifndef LCC_LEXER_HPP
#define LCC_LEXER_HPP

#include "lstring.hpp"
#include "util.hpp"

namespace lcc {

enum class TokenType {
    kLParen,
    kRParen,
    kLCurl,
    kRCurl,
    kAssign,
    kIf,
    kVoid,
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
    kErr,
};

const char *token_type_string(TokenType type);

struct Token {
    TokenType type;
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

    size_t curI{0};
    size_t curLen{0};
    Token *peekToken{nullptr};
};

Lexer *lexer_init(LString *src);
bool is_eof(Lexer *lex);
Token *lex_eat(Lexer *lex);
Token *lex_peek(Lexer *lex);

}  // namespace lcc

#endif
