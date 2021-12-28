#ifndef LCC_ANALYSIS_HPP
#define LCC_ANALYSIS_HPP

#include "list.hpp"
#include "parse.hpp"
#include "types.hpp"

namespace lcc {

enum AnalysisResult {
    kSuccess = 0,
    kFailure = 1,
};

AnalysisResult analyze_file(ExecutionContext *ctx);

};  // namespace lcc

#endif
