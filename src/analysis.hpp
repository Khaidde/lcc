#ifndef LCC_ANALYSIS_HPP
#define LCC_ANALYSIS_HPP

#include "astnode.hpp"

namespace lcc {

// Return true if error occured
bool analyze_package(LList<FileInfo *> &files);

};  // namespace lcc

#endif
