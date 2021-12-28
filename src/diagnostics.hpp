#ifndef LCC_DIAGNOSTICS_HPP
#define LCC_DIAGNOSTICS_HPP

#include "astnode.hpp"
#include "file.hpp"
#include "lexer.hpp"

namespace lcc {

struct DxInfo {
    file::FileInfo *finfo;
    size_t line;
    size_t startI;
    size_t len;
};

DxInfo curr(Lexer *l);

DxInfo at_eof(Lexer *l);

DxInfo at_point(file::FileInfo *finfo, size_t startI);

DxInfo at_token(file::FileInfo *finfo, Token *token);

DxInfo at_node(file::FileInfo *finfo, Node *node);

extern u8 numContextLines;

void dx_err(DxInfo dxinfo, const char *format, ...);

void dx_note(DxInfo dxinfo, const char *format, ...);

}  // namespace lcc

#endif
