#ifndef LCC_OPTIMIZE_HPP
#define LCC_OPTIMIZE_HPP

#include "gen.hpp"

namespace lcc {

namespace dominator {

bool is_dom_valid(Function &fn);
bool is_pdom_valid(Function &fn);
bool is_domfrontier_valid(Function &fn);

void invalidate_domtree(Function &fn);
void invalidate_pdomtree(Function &fn);
void invalidate_domfrontier(Function &fn);

void compute_domtree(Function &fn);
void compute_pdomtree(Function &fn);
void compute_domfrontier(Function &fn);

void print_domtree(Function &fn);
void print_dominates(Function &fn);
void print_domfrontier(Function &fn);

}  // namespace dominator

void optimize(Opt &opt, Function &fn);

}  // namespace lcc

#endif
