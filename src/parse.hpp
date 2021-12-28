#ifndef LCC_PARSE_HPP
#define LCC_PARSE_HPP

#include "astnode.hpp"
#include "lexer.hpp"

namespace lcc {

Node *parse_unit(Lexer *l);

}  // namespace lcc

#endif
