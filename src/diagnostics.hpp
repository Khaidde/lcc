#ifndef LCC_DIAGNOSTICS_HPP
#define LCC_DIAGNOSTICS_HPP

#include "astnode.hpp"
#include "file.hpp"
#include "lexer.hpp"
#include "print.hpp"

namespace lcc {

struct DxInfo {
    file::FileInfo *fileinfo;
    size_t line;
    size_t startI;
    size_t len;
};

DxInfo curr(Lexer *l);

DxInfo at_eof(Lexer *l);

DxInfo at_point(file::FileInfo *fileinfo, size_t startI);

DxInfo at_token(file::FileInfo *fileinfo, Token *token);

DxInfo at_node(file::FileInfo *fileinfo, Node *node);

extern u8 numContextLines;

void display_context(DxInfo &dxinfo);

template <typename... Args>
void dx_err(DxInfo dxinfo, const char *error, Args... args) {
    display_context(dxinfo);
    printf(error, args...);
}

}  // namespace lcc

#endif
