#include <cstdio>
#include <exception>
#include <string>

#include "lcc.hpp"
#include "list.hpp"
#include "lstring.hpp"
#include "util.hpp"

using namespace lcc;

int main(int argc, char **argv) {
    try {
        if (command_line(argc, argv) == ErrCode::kFailure) {
            fprintf(stderr, "  Failed to compile\n");
            return EXIT_FAILURE;
        }
        printf("  Compilation done\n");

        return EXIT_SUCCESS;
    } catch (const std::exception &) {
        return EXIT_FAILURE;
    }
}
