#ifndef LCC_ANALYSIS_HPP
#define LCC_ANALYSIS_HPP

#include "scope.hpp"

namespace lcc {

enum AnalysisResult {
    kSuccess = 0,
    kFailure = 1,
};

AnalysisResult analyze_file(CompilationContext *cmp);

};  // namespace lcc

#endif
