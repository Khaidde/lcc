#ifndef LCC_PARSE_HPP
#define LCC_PARSE_HPP

#include "astnode.hpp"
#include "file.hpp"
#include "lexer.hpp"

namespace lcc {

struct FileUnit {
    file::FileInfo *fileinfo;
    Node *unit;
};

Node *parse_source(LString &source);

FileUnit *parse_file(LString &filepath);

}  // namespace lcc

#endif
