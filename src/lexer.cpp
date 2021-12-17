#include "lexer.hpp"

#include <cstdarg>

#include "print.hpp"

namespace lcc {

namespace {

void end_token(Lexer *l) {
    l->curI += l->curLen;
    l->curLen = 0;
}

Token *create_token(Lexer *l, TokenType type) {
    Token *token = mem::malloc<Token>();
    token->type = type;
    token->startI = l->curI;
    token->len = l->curLen;
    end_token(l);
    return token;
}

Token *create_ident_token(Lexer *l) {
    Token *token = mem::malloc<Token>();
    token->type = TokenType::kIdent;
    token->data.ident = lstr_view(l->src->data, l->curI, l->curLen);
    token->startI = l->curI;
    token->len = l->curLen;
    end_token(l);
    return token;
}

Token *err_token(size_t startI, size_t len) {
    Token *err = mem::malloc<Token>();
    err->type = TokenType::kErr;
    err->startI = startI;
    err->len = len;
    return err;
}

char peek_char(Lexer *l) {
    if (is_eof(l)) return 0;
    return l->src->get(l->curI + l->curLen);
}

char peek_peek_char(Lexer *l) {
    if (l->curI + l->curLen + 1 >= l->src->size) return 0;
    return l->src->get(l->curI + l->curLen + 1);
}

bool is_whitespace(char c) { return c == ' ' || c == '\t' || c == '\r' || c == '\n'; }

void lex_single_line_comment(Lexer *l) {
    while (char c = peek_char(l)) {
        if (c == '\n') break;
        l->curLen++;
    }
    end_token(l);
}

Token *lex_multi_line_comment(Lexer *l) {
    while (char c0 = peek_char(l)) {
        l->curLen++;
        char c1 = peek_char(l);
        if (c0 == '*' && c1 == '/') {
            l->curLen++;
            end_token(l);
            return nullptr;
        }
        if (c0 == '/' && c1 == '*') {
            if (Token *rv = lex_multi_line_comment(l)) {
                end_token(l);
                return rv;
            }
        }
    }
    err("Could not find matching */ for multiline comment\n");
    return err_token(l->curI + l->curLen, 1);
}

bool is_letter(char c) { return ('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z'); }

bool is_letter_or_underscore(char c) { return is_letter(c) || c == '_'; }

bool is_number(char c) { return '0' <= c && c <= '9'; }

bool is_number_or_underscore(char c) { return is_number(c) || c == '_'; }

bool is_bindigit(char c) { return c == '1' || c == '0'; }

bool is_bindigit_or_underscore(char c) { return is_bindigit(c) || c == '_'; }

bool is_hexletter(char c) { return ('a' <= c && c <= 'f') || ('A' <= c && c <= 'F'); }

bool is_hexdigit_or_underscore(char c) { return is_hexletter(c) || c == '_'; }

bool is_hexdigit(char c) { return is_number(c) || is_hexletter(c); }

struct Keyword {
    const char *str;
    TokenType type;
};
constexpr Keyword kKeywords[]{
    {"if", TokenType::kIf},
    {"void", TokenType::kVoid},
};
constexpr size_t kNumKeywords = sizeof(kKeywords) / sizeof(Keyword);

constexpr int strings_cmp(char const *a, char const *b) {
    return (*a != *b || *a == '\0') ? *a - *b : strings_cmp(a + 1, b + 1);
}

constexpr bool is_sorted(size_t ndx) {
    if (ndx >= kNumKeywords - 1) return true;
    return strings_cmp(kKeywords[ndx].str, kKeywords[ndx + 1].str) <= 0 && is_sorted(ndx + 1);
}

static_assert(is_sorted(0), "Keywords must be listed alphabetically");

Token *lex_keyword_or_ident(Lexer *l) {
    int ksi = 0;
    int kei = kNumKeywords - 1;
    while (char c = peek_char(l)) {
        if (!is_letter_or_underscore(c) && !is_number(c)) break;
        while (ksi <= kei && kKeywords[ksi].str[l->curLen] < c) ksi++;
        while (kei >= ksi && kKeywords[kei].str[l->curLen] > c) kei--;
        l->curLen++;
    }
    if (ksi == kei) {
        return create_token(l, kKeywords[kei].type);
    }
    return create_ident_token(l);
}

Token *create_overflow_token(Lexer *l) {
    err("Int literal cannot fit in 16-bit value: %s\n", lstr_raw_view(*l->src, l->curI, l->curLen));
    return err_token(l->curI, l->curLen);
}

Token *lex_create_int_literal(Lexer *l, bool pos, u32 val) {
    Token *rv = create_token(l, TokenType::kIntLiteral);
    rv->data.intVal = pos ? (u16)val : -(u16)val;
    return rv;
}

Token *lex_binary(Lexer *l, bool pos) {
    if (!is_bindigit(peek_char(l))) {
        l->curLen++;
        err("Invalid binary digit: %s\n", lstr_raw_view(*l->src, l->curI, l->curLen));
        return err_token(l->curI, l->curLen);
    }
    u32 val = 0;
    while (char c = peek_char(l)) {
        if (!c || !is_bindigit_or_underscore(c)) break;
        l->curLen++;
        if (c != '_') {
            val = (val << 1) | (c & 1);
            if (val & 0x10000) return create_overflow_token(l);
        }
    }
    return lex_create_int_literal(l, pos, val);
}

Token *lex_hexadecimal(Lexer *l, bool pos) {
    if (!is_hexdigit(peek_char(l))) {
        l->curLen++;
        err("Invalid hexadecimal digit: %s\n", lstr_raw_view(*l->src, l->curI, l->curLen));
        return err_token(l->curI, l->curLen);
    }
    u32 val = 0;
    while (char c = peek_char(l)) {
        if (!c || !is_hexdigit_or_underscore(c)) break;
        l->curLen++;
        if (c != '_') {
            val <<= 4;
            if (c <= '9') {
                val |= (u8)(c - '0');
            } else {
                val |= (u8)(c - (c <= 'F' ? 'A' : 'a')) + 0xA;
            }
            if (val & 0xF0000) return create_overflow_token(l);
        }
    }
    return lex_create_int_literal(l, pos, val);
}

Token *lex_decimal(Lexer *l, bool pos) {
    u32 val = 0;
    while (char c = peek_char(l)) {
        if (!c || !is_number_or_underscore(c)) break;
        l->curLen++;
        if (c != '_') {
            val = val * 10 + (u8)(c - '0');
            if ((pos && val > 0x7FFF) || (!pos && val > 0x8000)) {
                return create_overflow_token(l);
            }
        }
    }
    return lex_create_int_literal(l, pos, val);
}

Token *lex_integer(Lexer *l) {
    bool pos = true;
    switch (peek_char(l)) {
        case '+': l->curLen++; break;
        case '-': l->curLen++; pos = false;
    }
    if (peek_char(l) == '0') {
        switch (peek_peek_char(l)) {
            case 'b': l->curLen += 2; return lex_binary(l, pos);
            case 'x': l->curLen += 2; return lex_hexadecimal(l, pos);
        }
    }
    return lex_decimal(l, pos);
}

}  // namespace

const char *token_type_string(TokenType type) {
    switch (type) {
        case TokenType::kLParen: return "(";
        case TokenType::kRParen: return ")";
        case TokenType::kLCurl: return "{";
        case TokenType::kRCurl: return "}";
        case TokenType::kAssign: return "=";
        case TokenType::kIf: return "if";
        case TokenType::kVoid: return "void";
        case TokenType::kAdd: return "+";
        case TokenType::kAddAdd: return "++";
        case TokenType::kAddEq: return "+=";
        case TokenType::kSubNeg: return "-";
        case TokenType::kSubSub: return "--";
        case TokenType::kSubEq: return "-=";
        case TokenType::kBitAnd: return "&";
        case TokenType::kBitAndEq: return "&=";
        case TokenType::kIntLiteral: return "'integer'";
        case TokenType::kIdent: return "'ident'";
        case TokenType::kErr: return "'error'";
    }
}

Lexer *lexer_init(LString *src) {
    Lexer *lex = mem::malloc<Lexer>();
    lex->src = src;
    return lex;
}

bool is_eof(Lexer *lex) { return lex->curI + lex->curLen >= lex->src->size; }

Token *lex_eat(Lexer *l) {
    if (l->peekToken) {
        Token *rv = l->peekToken;
        l->peekToken = nullptr;
        return rv;
    }

    while (is_whitespace(peek_char(l))) l->curI++;

    switch (char c = peek_char(l)) {
        case '/':
            l->curLen++;
            switch (peek_char(l)) {
                case '/':
                    l->curLen++;
                    lex_single_line_comment(l);
                    return lex_eat(l);
                case '*':
                    l->curLen++;
                    if (Token *rv = lex_multi_line_comment(l)) return rv;
                    return lex_eat(l);
                default: todo("Division not yet supported\n"); return nullptr;
            }
        case '(': l->curLen++; return create_token(l, TokenType::kLParen);
        case ')': l->curLen++; return create_token(l, TokenType::kRParen);
        case '{': l->curLen++; return create_token(l, TokenType::kLCurl);
        case '}': l->curLen++; return create_token(l, TokenType::kRCurl);
        case '=': l->curLen++; return create_token(l, TokenType::kAssign);
        case '+':
            if (is_number(peek_peek_char(l))) return lex_integer(l);
            l->curLen++;
            switch (char c = peek_char(l)) {
                case '+': l->curLen++; return create_token(l, TokenType::kAddAdd);
                case '=': l->curLen++; return create_token(l, TokenType::kAddEq);
                default: return create_token(l, TokenType::kAdd);
            }
        case '-':
            if (is_number(peek_peek_char(l))) return lex_integer(l);
            l->curLen++;
            switch (char c = peek_char(l)) {
                case '-': l->curLen++; return create_token(l, TokenType::kSubSub);
                case '=': l->curLen++; return create_token(l, TokenType::kSubEq);
                default: return create_token(l, TokenType::kSubNeg);
            }
        case '&':
            l->curLen++;
            if (peek_char(l) == '=') {
                l->curLen++;
                return create_token(l, TokenType::kBitAndEq);
            } else {
                return create_token(l, TokenType::kBitAnd);
            }
            break;
        case 0: return nullptr;
        default:
            if (is_letter_or_underscore(c)) return lex_keyword_or_ident(l);
            if (is_number(c)) return lex_integer(l);

            err("Unexpected character: '%c'\n", c);
            return err_token(l->curI, 1);
    }
}

Token *lex_peek(Lexer *l) {
    if (!l->peekToken) {
        l->peekToken = lex_eat(l);
    }
    return l->peekToken;
}

}  // namespace lcc
