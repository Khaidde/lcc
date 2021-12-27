#include "lcc.hpp"

#include <cstring>

#include "analysis.hpp"
#include "diagnostics.hpp"
#include "file.hpp"
#include "lexer.hpp"
#include "list.hpp"
#include "lstring.hpp"
#include "parse.hpp"
#include "print.hpp"
#include "scope.hpp"
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

ErrCode set_num_context_lines(FlagParserInfo &finfo) {
    if (++finfo.ndx == finfo.argc) {
        err("-num-context-lines must be followed by a number\n");
        return ErrCode::kFailure;
    }
    const char *num = finfo.argv[finfo.ndx];
    u16 val = 0;
    while (*num) {
        if (*num < '0' || *num > '9') {
            err("%s expected to be a positive number\n", finfo.argv[finfo.ndx]);
            return ErrCode::kFailure;
        }
        val = val * 10 + *num - '0';
        if (val > 0xFF) {
            err("%s exceeds maximum number of context lines displayable\n", finfo.argv[finfo.ndx]);
            return ErrCode::kFailure;
        }
        num++;
    }
    numContextLines = val;
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
    {"-num-context-lines", set_num_context_lines},
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

void replace_backslashes(LString &out) {
    char *data = out.data;
    while (*data) {
        if (*data == '\\') {
            *data = '/';
        }
        data++;
    }
}

struct Package {
    ScopeStack *scopes;
    LList<FileUnit *> files;
};

using PackageMap = LMap<LStringView, Package, lstr_hash, lstr_equal>;

struct ImportContext {
    file::FileInfo *finfo;
    LStringView importName;
};

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
    if (!file::is_regular_file(path)) {
        err("Input path must link to a regular file\n");
        return ErrCode::kFailure;
    }

    PackageMap pkgMap;
    pkgMap.init();

    LList<ImportContext> importStack{};

    LString rootDir = file::get_dir(path);
    ImportContext rootDirCtx = {nullptr, {".", 1}};
    importStack.add(rootDirCtx);

    LList<LString> filenames{};
    while (importStack.size) {
        // Pop next import to resolve
        ImportContext importCtx = importStack.get(importStack.size - 1);
        importStack.size--;

        // If import already resolved then go next
        if (pkgMap.get(importCtx.importName)) continue;

        // Find all files in the package if package directory exists
        LString pkgDir = lstr_create(rootDir.data);
        if (importCtx.importName.len != 1 || importCtx.importName.src[0] != '.') {
            lstr_cat(pkgDir, "/");
            lstr_cat(pkgDir, importCtx.importName);
        }
        if (file::file_in_dir(filenames, pkgDir) == file::FileErrCode::kNotFound) {
            err("Could not find package '%s' imported by '%s'\n", lstr_create(importCtx.importName).data,
                importCtx.finfo->path);
            break;
        }

        Package pkg;
        pkg.files = {};
        pkg.scopes = scope_init();
        scope_enter(pkg.scopes);
        pkgMap.try_put(importCtx.importName, pkg);

        for (size_t i = 0; i < filenames.size; i++) {
            replace_backslashes(filenames.get(i));
            info("Compiling %s ...\n", filenames.get(i).data);

            // Parse file
            file::FileInfo *fileinfo;
            if (file::read_file(&fileinfo, filenames.get(i).data) != file::FileErrCode::kSuccess) {
                err("Failed to read file: %s\n", filenames.get(i).data);
                return ErrCode::kFailure;
            }

            FileUnit *fileunit;
            Node *unit;
            {
                Lexer lexer{};
                lexer.finfo = fileinfo;
                unit = parse_unit(&lexer);

                fileunit = mem::malloc<FileUnit>();
                fileunit->finfo = fileinfo;
                fileunit->unit = unit;

                if (!fileunit->unit) return ErrCode::kFailure;
                pkg.files.add(fileunit);
            }

            // Add all declarations in current file to package symbol table
            for (size_t k = 0; k < unit->data.unit.decls.size; k++) {
                TableEntry entry{fileinfo, unit->data.unit.decls.get(k)};

                if (entry.decl->data.decl.lval->type != NodeType::kName) {
                    dx_err(at_node(fileinfo, entry.decl->data.decl.lval),
                           "Must only declare variable names in global scope\n");
                    return ErrCode::kFailure;
                }

                if (TableEntry *other = scope_bind(pkg.scopes, entry)) {
                    dx_err(at_node(entry.finfo, entry.decl->data.decl.lval), "Found duplicate declaration\n");
                    dx_err(at_node(other->finfo, other->decl->data.decl.lval), "Previous declaration found here\n");
                    return ErrCode::kFailure;
                }
            }

            // Add pending imports in current file to import stack
            for (size_t k = 0; k < unit->data.unit.imports.size; k++) {
                ImportContext import{fileinfo, unit->data.unit.imports.get(k)};
                if (!pkgMap.get(import.importName)) importStack.add(import);
            }

            print_ast(unit);
        }

        filenames.size = 0;
    }

    return ErrCode::kSuccess;
}

}  // namespace lcc
