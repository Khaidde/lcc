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
        (void)argc;
        (void)argv;
        const char *args[2] = {"NULL", "C:/Users/berkx/Desktop/lcc/test/suite/test2.tc"};
        if (command_line(2, const_cast<char **>(args)) == ErrCode::kFailure) {
            // if (command_line(argc, argv) == ErrCode::kFailure) {
            fprintf(stderr, "  Failed to compile\n");
            return EXIT_FAILURE;
        }
        printf("  Compilation done\n");

        return EXIT_SUCCESS;
    } catch (const std::exception &) {
        return EXIT_FAILURE;
    }
}
