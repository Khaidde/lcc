#include <stdio.h>
#include <stdlib.h>

#include <exception>

#include "lcc.hpp"
#include "util.hpp"

int main(int argc, char** argv) {
    try {
        if (lcc::command_line(argc, argv) == lcc::ErrCode::kFailure) {
            fprintf(stderr, "  Failed to compile\n");
            return EXIT_FAILURE;
        }

        printf("  Compilation done\n");

        return EXIT_SUCCESS;
    } catch (const std::exception&) {
        return EXIT_FAILURE;
    }
}
