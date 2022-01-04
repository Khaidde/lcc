#ifndef LCC_DIAGNOSTIC_HPP
#define LCC_DIAGNOSTIC_HPP

#include <cstdint>

#include "ast.hpp"
#include "compilation.hpp"
#include "token.hpp"
#include "util.hpp"

namespace lcc {

struct DxInfo {
    FileInfo *finfo;
    size_t line;
    size_t startI;
    size_t len;
};

DxInfo curr(Lexer *l);

DxInfo at_eof(Lexer *l);

DxInfo at_point(FileInfo *finfo, size_t startI);

DxInfo at_token(FileInfo *finfo, Token *token);

DxInfo at_node(FileInfo *finfo, Node *node);

extern size_t numContextLines;

void dx_err(DxInfo dxinfo, const char *format, ...);

void dx_note(DxInfo dxinfo, const char *format, ...);

}  // namespace lcc

#endif
