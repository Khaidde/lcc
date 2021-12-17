#include "lcc.hpp"

#include <cstring>

#include "file.hpp"
#include "lexer.hpp"
#include "list.hpp"
#include "lstring.hpp"
#include "print.hpp"
#include "util.hpp"

namespace lcc {

namespace {

struct FlagParserInfo {
    char **argv;
    int argc;
    int ndx{1};

    const char *outputFile{"a.out"};
    bool hasOutputFileChanged{false};

    LList<char *> inputFiles{};
};

static constexpr size_t kFlagStrLen = 32;
struct Flag {
    const char name[kFlagStrLen];
    ErrCode (*callBack)(FlagParserInfo &);
};

ErrCode set_output_file(FlagParserInfo &finfo) {
    if (++finfo.ndx == finfo.argc) {
        err("-o must be followed by an output file name\n");
        return ErrCode::kFailure;
    }
    if (finfo.hasOutputFileChanged) {
        err("Cannot have multiple output files: %s\n", finfo.argv[finfo.ndx]);
        return ErrCode::kFailure;
    }
    finfo.hasOutputFileChanged = true;
    finfo.outputFile = finfo.argv[finfo.ndx];
    return ErrCode::kSuccess;
}

ErrCode set_no_color(FlagParserInfo &) {
    printUseColor = false;
    return ErrCode::kSuccess;
}

ErrCode no_op(FlagParserInfo &) { return ErrCode::kSuccess; }

constexpr Flag kPrimaryFlags[]{
    {"-no-color", set_no_color},
};
constexpr size_t kNumPrimaryFlags = sizeof(kPrimaryFlags) / sizeof(Flag);

constexpr Flag kSecondaryFlags[]{
    {"-o", set_output_file},
    {"-no-color", no_op},
};
constexpr size_t kNumSecondaryFlags = sizeof(kSecondaryFlags) / sizeof(Flag);

ErrCode parse_args(FlagParserInfo &finfo) {
    // First pass - primarily to disable color
    while (finfo.ndx < finfo.argc) {
        if (finfo.argv[finfo.ndx][0] == '-') {
            size_t i = 0;
            while (i < kNumPrimaryFlags) {
                if (strncmp(kPrimaryFlags[i].name, finfo.argv[finfo.ndx], kFlagStrLen) == 0) {
                    if (kPrimaryFlags[i].callBack(finfo) == ErrCode::kFailure) {
                        return ErrCode::kFailure;
                    }
                    break;
                }
                i++;
            }
        }
        finfo.ndx++;
    }
    finfo.ndx = 1;

    // Second pass
    while (finfo.ndx < finfo.argc) {
        if (finfo.argv[finfo.ndx][0] == '-') {
            size_t i = 0;
            while (i < kNumSecondaryFlags) {
                if (strcmp(kSecondaryFlags[i].name, finfo.argv[finfo.ndx]) == 0) {
                    if (kSecondaryFlags[i].callBack(finfo) == ErrCode::kFailure) {
                        return ErrCode::kFailure;
                    }
                    break;
                }
                i++;
            }
            if (i == kNumSecondaryFlags) {
                err("Unknown flag: %s\n", finfo.argv[finfo.ndx]);
                return ErrCode::kFailure;
            }
        } else if (finfo.inputFiles.size) {
            todo("Handle multiple input files\n");
            return ErrCode::kFailure;
        } else {
            finfo.inputFiles.add(finfo.argv[finfo.ndx]);
        }
        finfo.ndx++;
    }
    if (finfo.inputFiles.size == 0) {
        err("No input files\n");
        return ErrCode::kFailure;
    }
    return ErrCode::kSuccess;
}

}  // namespace

ErrCode command_line(int argc, char **argv) {
    FlagParserInfo finfo{argv, argc};
    if (parse_args(finfo) == ErrCode::kFailure) {
        return ErrCode::kFailure;
    }

    if (compile(finfo.inputFiles.get(0)) != ErrCode::kSuccess) {
        return ErrCode::kFailure;
    }

    info("output-file: %s\n", finfo.outputFile);

    return ErrCode::kSuccess;
}

ErrCode compile(const char *path) {
    LString output;
    if (file::read_file(output, path) != file::FileErrCode::kSuccess) {
        err("Failed to read file: %s\n", path);
        return ErrCode::kFailure;
    }

    debug("Compiling %s...\n", path);

    Lexer *lexer = lexer_init(&output);
    while (Token *token = lex_eat(lexer)) {
        if (token->type == TokenType::kErr) break;
        switch (token->type) {
            case TokenType::kIntLiteral: info("val: %04x(%hi)\n", token->data.intVal, token->data.intVal); break;
            case TokenType::kIdent: info("str: %s\n", lstr_create(token->data.ident).data); break;
            default: info("%s\n", token_type_string(token->type));
        }
    }

    return ErrCode::kSuccess;
}

}  // namespace lcc
