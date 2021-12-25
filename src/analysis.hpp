#ifndef LCC_ANALYSIS_HPP
#define LCC_ANALYSIS_HPP

#include "list.hpp"
#include "parse.hpp"

namespace lcc {

// Return true if error occured
bool analyze_package(LList<FileUnit *> &files);

};  // namespace lcc

#endif
