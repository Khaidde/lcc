#ifndef LCC_PARSE_HPP
#define LCC_PARSE_HPP

#include "token.hpp"
#include "types.hpp"

namespace lcc {

Node *parse_global(Lexer *l);

}  // namespace lcc

#endif
