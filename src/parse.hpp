#ifndef LCC_PARSE_HPP
#define LCC_PARSE_HPP

#include "astnode.hpp"
#include "lexer.hpp"

namespace lcc {

Node *parse_source(LString *source);

FileInfo *parse_file(LString &filepath);

}  // namespace lcc

#endif
