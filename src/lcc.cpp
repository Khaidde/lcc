#include "lcc.hpp"

#include <cstring>

#include "list.hpp"
#include "print.hpp"
#include "util.hpp"

using namespace lcc;

namespace {

struct FlagParserInfo {
    char **argv;
    int argc;
    int index{1};

    const char *outputFile{"a.out"};
    LList<char *> inputFiles{};
};

struct Flag {
    const char *name;
    ErrCode (*callBack)(FlagParserInfo &);
};

ErrCode set_output_file(FlagParserInfo &finfo) {
    if (++finfo.index == finfo.argc) {
        err("-o must be followed by an output file name\n");
        return ErrCode::kFailure;
    }
    finfo.outputFile = finfo.argv[finfo.index];
    return ErrCode::kSuccess;
}

static constexpr Flag kFlags[]{
    {"-o", set_output_file},
};
static constexpr size_t kNumFlags = sizeof(kFlags) / sizeof(Flag);

ErrCode parse_args(FlagParserInfo &finfo) {
    while (finfo.index < finfo.argc) {
        if (finfo.argv[finfo.index][0] == '-') {
            size_t i = 0;
            while (i < kNumFlags) {
                if (strcmp(kFlags[i].name, finfo.argv[finfo.index]) == 0) {
                    if (kFlags[i].callBack(finfo) == ErrCode::kFailure) {
                        return ErrCode::kFailure;
                    }
                    break;
                }
                i++;
            }
            if (i == kNumFlags) {
                err("Unknown flag: %s\n", finfo.argv[finfo.index]);
                return ErrCode::kFailure;
            }
        } else if (finfo.inputFiles.size) {
            todo("Handle multiple input files\n");
            return ErrCode::kFailure;
        } else {
            finfo.inputFiles.add(finfo.argv[finfo.index]);
        }
        finfo.index++;
    }
    if (finfo.inputFiles.size == 0) {
        err("No input files\n");
        return ErrCode::kFailure;
    }
    return ErrCode::kSuccess;
}

}  // namespace

ErrCode lcc::command_line(int argc, char **argv) {
    mem::allocator.new_block();

    FlagParserInfo finfo{argv, argc};
    if (parse_args(finfo) == ErrCode::kFailure) {
        return ErrCode::kFailure;
    }

    compile(finfo.inputFiles.get(0));

    info("output-file: %s\n", finfo.outputFile);

    return ErrCode::kSuccess;
}

void lcc::compile(const char *path) { debug("Compiling %s...\n", path); }
