#ifndef LCC_DIAGNOSTICS_HPP
#define LCC_DIAGNOSTICS_HPP

#include <stdio.h>

#include "astnode.hpp"
#include "lexer.hpp"
#include "print.hpp"

namespace lcc {

struct DxInfo {
    size_t line;
    size_t startI;
    size_t len;
};

DxInfo at_point(size_t startI);

DxInfo curr(Lexer *l);

DxInfo at_token(Token *token);

DxInfo at_node(LString *src, Node *node);

DxInfo at_eof(Lexer *l);

extern u8 numContextLines;

void display_context(LString *src, DxInfo &dxinfo);

template <typename... Args>
void dx_err(LString *src, DxInfo dxinfo, const char *error, Args... args) {
    display_context(src, dxinfo);
    err(error, args...);
}

}  // namespace lcc

#endif
