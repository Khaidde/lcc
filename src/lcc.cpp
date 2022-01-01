#include "lcc.hpp"

#include <cstdio>
#include <cstring>

#include "analysis.hpp"
#include "diagnostic.hpp"
#include "lstring.hpp"
#include "parse.hpp"
#include "print.hpp"
#include "scope.hpp"
#include "types.hpp"

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
    size_t val = 0;
    while (*num) {
        if (*num < '0' || *num > '9') {
            err("%s expected to be a positive number\n", finfo.argv[finfo.ndx]);
            return ErrCode::kFailure;
        }
        val = val * 10 + (unsigned)*num - '0';
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

}  // namespace

CompilationContext resolve_packages(const char *mainFile) {
    CompilationContext cmp;
    cmp.packageMap.init();

    struct ImportContext {
        File *srcFile;
        LStringView importName;
    };
    LList<ImportContext> importStack{};

    LString rootDir = file::split_dir(mainFile);
    importStack.add({nullptr, {".", 1}});

    LList<LString> filenames{};
    while (importStack.size) {
        // Get next import to resolve
        ImportContext &importCtx = importStack.last();
        importStack.size--;

        if (!cmp.packageMap.get(importCtx.importName)) {
            // Find all files in the package if package directory exists
            LString pkgDir = lstr_create(rootDir.data);
            if (importCtx.importName.len != 1 || importCtx.importName.src[0] != '.') {
                lstr_cat(pkgDir, "/");
                lstr_cat(pkgDir, importCtx.importName);
            }
            if (file::file_in_dir(filenames, pkgDir) == file::FileErrCode::kNotFound) {
                err("Could not find package '%s' imported by '%s'\n", lstr_raw_str(importCtx.importName),
                    importCtx.srcFile->finfo->path);
                cmp.isPackageResolutionSuccesful = false;
                return cmp;
            }
            info("Importing package '%s' ...\n", lstr_raw_str(importCtx.importName));

            Package *pkg = mem::malloc<Package>();
            pkg->files = {};
            pkg->globalDecls.init();
            cmp.packageMap.try_put(importCtx.importName, pkg);

            for (size_t i = 0; i < filenames.size; i++) {
                // Parse file
                File *file = mem::malloc<File>();
                if (file::read_file(&file->finfo, filenames.get(i).data) != file::FileErrCode::kSuccess) {
                    err("Failed to read file: %s\n", filenames.get(i).data);
                    cmp.isPackageResolutionSuccesful = false;
                    return cmp;
                }
                file->package = pkg;
                file->imports.init();
                pkg->files.add(file);
                if (parse_file(file)) {
                    cmp.isPackageResolutionSuccesful = false;
                    return cmp;
                }

                // Add pending imports in current file to import stack
                for (size_t k = 0; k < file->imports.capacity; k++) {
                    if (file->imports.table[k].psl) {
                        Node *import = file->imports.table[k].val;
                        if (!cmp.packageMap.get(import->import.package)) {
                            importStack.add({file, import->import.package});
                        }
                    }
                }
            }

            // Make sure package aliases don't confict with declaration names
            for (size_t i = 0; i < pkg->files.size; i++) {
                File *file = pkg->files.get(i);
                for (size_t k = 0; k < file->imports.capacity; k++) {
                    auto &entry = file->imports.table[k];
                    if (entry.psl) {
                        if (Node **decl = pkg->globalDecls.get(entry.val->import.alias)) {
                            dx_err(at_node(file->finfo, entry.val),
                                   "Import alias cannot have the same name as a declaration\n");
                            dx_note(at_node((*decl)->decl.file->finfo, *decl), "Declaration found here\n");
                            cmp.isPackageResolutionSuccesful = false;
                            return cmp;
                        }
                    }
                }
            }
        }  // end of package resolution

        filenames.size = 0;
    }
    cmp.isPackageResolutionSuccesful = true;
    return cmp;
}

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

    CompilationContext cmp = resolve_packages(path);
    if (!cmp.isPackageResolutionSuccesful) {
        return ErrCode::kFailure;
    }

    // Prepare stacks for analysis of entire program
    cmp.scopeStack = scope_init();
    cmp.resolveFuncBodyStack = {};
    cmp.currNumPendingFunc = 0;

    LStringView root{".", 1};
    Package *pkg = *cmp.packageMap.get(root);
    File *file = pkg->files.get(0);
    info("Compiling %s ...\n", file->finfo->path);

    // Semantic analysis of the file
    cmp.currFile = file;
    if (analyze_file(&cmp)) return ErrCode::kFailure;

    pkg = *cmp.packageMap.get(root);
    // Print all declarations in the current package
    for (size_t i = 0; i < pkg->globalDecls.capacity; i++) {
        auto &entry = pkg->globalDecls.table[i];
        if (entry.psl) {
            print_ast(entry.val);
        }
    }

    return ErrCode::kSuccess;
}

}  // namespace lcc
