#include "token.hpp"

#include "diagnostic.hpp"

namespace lcc {

namespace {

void end_token(Lexer *l) {
    l->curI += l->curLen;
    l->curLen = 0;
}

Token *create_token(Lexer *l, TokenType type) {
    l->curToken.type = type;
    l->curToken.line = l->line;
    l->curToken.startI = l->curI;
    l->curToken.len = l->curLen;
    end_token(l);
    return &l->curToken;
}

Token *create_str_literal_token(Lexer *l) {
    l->curToken.type = TokenType::kStrLiteral;
    l->curToken.line = l->line;
    l->curToken.str = lstr_view(l->finfo->src.data, l->curI + 1, l->curLen - 2);
    l->curToken.startI = l->curI;
    l->curToken.len = l->curLen;
    end_token(l);
    return &l->curToken;
}

Token *create_ident_token(Lexer *l) {
    l->curToken.type = TokenType::kIdent;
    l->curToken.line = l->line;
    l->curToken.ident = lstr_view(l->finfo->src.data, l->curI, l->curLen);
    l->curToken.startI = l->curI;
    l->curToken.len = l->curLen;
    end_token(l);
    return &l->curToken;
}

Token *create_eof_token(Lexer *l) {
    l->curToken.type = TokenType::kEof;
    l->curToken.line = l->line;
    l->curToken.startI = l->curI;
    l->curToken.len = 1;
    return &l->curToken;
}

Token *ret_err(Lexer *l) {
    l->curToken.type = TokenType::kErr;
    return &l->curToken;
}

char peek_char(Lexer *l) {
    if (is_eof(l)) return 0;
    return l->finfo->src.get(l->curI + l->curLen);
}

char peek_peek_char(Lexer *l) {
    if (l->curI + l->curLen + 1 >= l->finfo->src.size) return 0;
    return l->finfo->src.get(l->curI + l->curLen + 1);
}

void lex_single_line_comment(Lexer *l) {
    while (char c = peek_char(l)) {
        if (c == '\n') break;
        l->curLen++;
    }
    end_token(l);
}

Token *lex_multi_line_comment(Lexer *l) {
    while (char c0 = peek_char(l)) {
        if (c0 == '\n') l->line++;
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
    dx_err(at_eof(l), "Could not find matching */ for multiline comment\n");
    return ret_err(l);
}

bool is_letter(char c) { return ('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z'); }

bool is_letter_or_underscore(char c) { return is_letter(c) || c == '_'; }

bool is_number(char c) { return '0' <= c && c <= '9'; }

bool is_number_or_underscore(char c) { return is_number(c) || c == '_'; }

bool is_bindigit(char c) { return c == '1' || c == '0'; }

bool is_bindigit_or_underscore(char c) { return is_bindigit(c) || c == '_'; }

bool is_hexletter(char c) { return ('a' <= c && c <= 'f') || ('A' <= c && c <= 'F'); }

bool is_hexdigit(char c) { return is_number(c) || is_hexletter(c); }

bool is_hexdigit_or_underscore(char c) { return is_hexdigit(c) || c == '_'; }

// clang-format off
constexpr TokenType kKeywords[]{
    TokenType::kBreak,
    TokenType::kCont,
    TokenType::kElse,
    TokenType::kIf,
    TokenType::kImport,
    TokenType::kRet,
    TokenType::kWhile,
};
// clang-format on
constexpr size_t kNumKeywords = sizeof(kKeywords) / sizeof(TokenType);

ASSERT_ALPHABETIC(kNumKeywords, token_type_string(kKeywords[ndx]))

Token *lex_keyword_or_ident(Lexer *l) {
    int ksi = 0;
    int kei = kNumKeywords - 1;
    while (char c = peek_char(l)) {
        if (!is_letter_or_underscore(c) && !is_number(c)) break;
        while (ksi <= kei && token_type_string(kKeywords[ksi])[l->curLen] < c) ksi++;
        while (kei >= ksi && token_type_string(kKeywords[kei])[l->curLen] > c) kei--;
        l->curLen++;
    }
    if (ksi == kei && token_type_string(kKeywords[ksi])[l->curLen] == '\0') {
        return create_token(l, kKeywords[kei]);
    }
    return create_ident_token(l);
}

Token *create_overflow_token(Lexer *l) {
    dx_err(curr(l), "Int literal cannot fit in 16-bit value: %s\n", lstr_raw_view(l->finfo->src, l->curI, l->curLen));
    return ret_err(l);
}

Token *lex_create_int_literal(Lexer *l, bool pos, uint16_t val) {
    Token *rv = create_token(l, TokenType::kIntLiteral);
    rv->intVal = pos ? val : -val;
    return rv;
}

Token *lex_binary(Lexer *l, bool pos) {
    if (!is_bindigit(peek_char(l))) {
        char digit = peek_char(l);
        l->curLen++;
        dx_err(curr(l), "Invalid binary digit: %c\n", digit);
        return ret_err(l);
    }
    uint64_t val = 0;
    while (char c = peek_char(l)) {
        if (!is_bindigit(c) && is_number(c)) {
            char digit = peek_char(l);
            l->curLen++;
            dx_err(curr(l), "Invalid binary digit: %c\n", digit);
            return ret_err(l);
        }
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
        char digit = peek_char(l);
        l->curLen++;
        dx_err(curr(l), "Invalid hexadecimal digit: %c\n", digit);
        return ret_err(l);
    }
    uint64_t val = 0;
    while (char c = peek_char(l)) {
        if (!c || !is_hexdigit_or_underscore(c)) break;
        l->curLen++;
        if (c != '_') {
            val <<= 4;
            if (c <= '9') {
                val |= (unsigned)(c - '0');
            } else {
                val |= (unsigned)(c - (c <= 'F' ? 'A' : 'a')) + 0xA;
            }
            if (val & 0xF0000) return create_overflow_token(l);
        }
    }
    return lex_create_int_literal(l, pos, val);
}

Token *lex_decimal(Lexer *l, bool pos) {
    uint64_t val = 0;
    while (char c = peek_char(l)) {
        if (!c || !is_number_or_underscore(c)) break;
        l->curLen++;
        if (c != '_') {
            val = val * 10 + (unsigned)(c - '0');
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

Token *lex_string(Lexer *l) {
    l->curLen++;
    while (char c = peek_char(l)) {
        if (c == '"') {
            l->curLen++;
            return create_str_literal_token(l);
        }
        if (c == '\n') {
            dx_err(curr(l), "Multiline string literal is not supported\n",
                   lstr_raw_view(l->finfo->src, l->curI, l->curLen));
            return ret_err(l);
        }
        l->curLen++;
    }
    dx_err(at_eof(l), "Could not find matching \" for string\n");
    return ret_err(l);
}

}  // namespace

bool is_whitespace(char c) { return c == ' ' || c == '\t' || c == '\r' || c == '\n'; }

bool is_eof(Lexer *lex) { return lex->curI + lex->curLen >= lex->finfo->src.size; }

Token *lex_next(Lexer *l) {
    while (is_whitespace(peek_char(l))) {
        if (peek_char(l) == '\n') l->line++;
        l->curI++;
    }

    switch (char c = peek_char(l)) {
        case '/':
            l->curLen++;
            switch (peek_char(l)) {
                case '/':
                    l->curLen++;
                    lex_single_line_comment(l);
                    return lex_next(l);
                case '*':
                    l->curLen++;
                    if (Token *rv = lex_multi_line_comment(l)) return rv;
                    return lex_next(l);
                default: dx_err(curr(l), "Division not yet supported\n"); return ret_err(l);
            }
        case '+':
            if (is_number(peek_peek_char(l))) return lex_integer(l);
            l->curLen++;
            switch (peek_char(l)) {
                case '+': l->curLen++; return create_token(l, TokenType::kAddAdd);
                case '=': l->curLen++; return create_token(l, TokenType::kAddEq);
                default: return create_token(l, TokenType::kAdd);
            }
        case '-':
            if (is_number(peek_peek_char(l))) return lex_integer(l);
            l->curLen++;
            switch (peek_char(l)) {
                case '-': l->curLen++; return create_token(l, TokenType::kSubSub);
                case '=': l->curLen++; return create_token(l, TokenType::kSubEq);
                case '>': l->curLen++; return create_token(l, TokenType::kArrow);
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
        case '"': return lex_string(l);
        case '*':
            l->curLen++;
            if (peek_char(l) == '/') {
                l->curLen++;
                dx_err(curr(l), "Unexpected */ with no matching /* for multiline comment\n", l->line, c);
                return ret_err(l);
            } else {
                return create_token(l, TokenType::kPtr);
            }
        case '@': l->curLen++; return create_token(l, TokenType::kDeref);
        case '.': l->curLen++; return create_token(l, TokenType::kDot);
        case '(': l->curLen++; return create_token(l, TokenType::kLParen);
        case ')': l->curLen++; return create_token(l, TokenType::kRParen);
        case '{': l->curLen++; return create_token(l, TokenType::kLCurl);
        case '}': l->curLen++; return create_token(l, TokenType::kRCurl);
        case '=':
            l->curLen++;
            if (peek_char(l) == '>') {
                l->curLen++;
                return create_token(l, TokenType::kRetArrow);
            } else {
                return create_token(l, TokenType::kAssign);
            }
        case ':':
            l->curLen++;
            if (peek_char(l) == ':') {
                l->curLen++;
                return create_token(l, TokenType::kLabel);
            } else {
                return create_token(l, TokenType::kColon);
            }
        case ',': l->curLen++; return create_token(l, TokenType::kComma);
        case 0: return create_eof_token(l);
        default:
            if (is_letter_or_underscore(c)) return lex_keyword_or_ident(l);
            if (is_number(c)) return lex_integer(l);

            l->curLen++;
            dx_err(curr(l), "Unknown character: '%c'\n", c);
            return ret_err(l);
    }
}

Token *lex_peek(Lexer *l) { return &l->curToken; }

}  // namespace lcc
