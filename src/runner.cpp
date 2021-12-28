#include <cstdio>

#include "lcc.hpp"

using namespace lcc;

int main(int argc, char **argv) {
    if (command_line(argc, argv) == ErrCode::kFailure) {
        fprintf(stderr, "  Failed to compile\n");
        return EXIT_FAILURE;
    }
    printf("  Compilation done\n");

    return EXIT_SUCCESS;
}
