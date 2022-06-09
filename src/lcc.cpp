#include "lcc.hpp"

#include <cstdio>
#include <cstring>

#include "analysis.hpp"
#include "compilation.hpp"
#include "diagnostic.hpp"
#include "lstring.hpp"
#include "optimize.hpp"
#include "parse.hpp"
#include "print.hpp"
#include "scope.hpp"
#include "translate.hpp"

namespace lcc {

namespace {

struct FlagParserInfo {
    char **argv;
    int argc;
    int ndx{1};

    const char *outputFile{nullptr};
    const char *inputFile{nullptr};
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
    if (finfo.outputFile) {
        err("Cannot have multiple output files: %s\n", finfo.argv[finfo.ndx]);
        return ErrCode::kFailure;
    }
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
        } else if (finfo.inputFile) {
            err("Multiple input files not supported\n");
            return ErrCode::kFailure;
        } else {
            finfo.inputFile = finfo.argv[finfo.ndx];
        }
        finfo.ndx++;
    }
    if (!finfo.outputFile) {
        finfo.outputFile = "a.out";
    }
    if (!finfo.inputFile) {
        err("No input files\n");
        return ErrCode::kFailure;
    }
    return ErrCode::kSuccess;
}

ErrCode resolve_file(CompilationContext &cmp, File *file, const char *filename) {
    if (file::read_file(&file->finfo, filename) != file::FileErrCode::kSuccess) {
        err("Failed to read file: %s\n", filename);
        return ErrCode::kFailure;
    }
    file->importListHead = nullptr;
    file->imports.init();

    Lexer l{};
    l.finfo = file->finfo;
    lex_next(&l);  // Grab first lexer token
    for (;;) {
        if (lex_peek(&l)->type == TokenType::kEof) return ErrCode::kSuccess;
        if (lex_peek(&l)->type == TokenType::kErr) return ErrCode::kFailure;
        Node *global = parse_global(&l);
        if (!global) return ErrCode::kFailure;
        if (global->kind == NodeKind::kImport) {
            if (Node **otherImport = file->imports.try_put(global->import.alias, global)) {
                dx_err(at_node(l.finfo, global), "Duplicate package alias: %s\n", lstr_raw_str(global->import.alias));
                dx_err(at_node(l.finfo, *otherImport), "Previous import here\n");
                return ErrCode::kFailure;
            }

            if (file->importListHead) global->import.nextImport = file->importListHead;
            file->importListHead = global;
        } else if (global->kind == NodeKind::kDecl) {
            DeclInfo *declInfo = mem::malloc<DeclInfo>();
            declInfo->isResolving = false;
            declInfo->nextDecl = nullptr;
            declInfo->declNode = global;
            declInfo->file = file;

            LStringView &declName = global->decl.lval->name.ident;
            if (DeclInfo **preload = cmp.preloadPkg->globalDecls[declName]) {
                dx_err(at_node(file->finfo, global->decl.lval), "Declaration cannot have the name '%s'\n",
                       lstr_raw_str((*preload)->declNode->decl.lval->name.ident));
                return ErrCode::kFailure;
            }
            if (DeclInfo **other = file->package->globalDecls.try_put(declName, declInfo)) {
                DeclInfo *otherInfo = *other;
                dx_err(at_node(file->finfo, global->decl.lval), "Duplicate declaration\n");
                dx_note(at_node(otherInfo->file->finfo, otherInfo->declNode->decl.lval), "Previous declaration here\n");
                return ErrCode::kFailure;
            }
            file->package->globalDeclList.add(declInfo);
        }
    }
}

CompilationContext preload(const char *preloadFilePath) {
    CompilationContext cmp;
    info("Compiling %s ...\n", preloadFilePath);

    // Prepare stacks for analysis of entire program
    cmp.scopeStack = scope_init();
    cmp.resolveFuncBodyStack = {};
    cmp.currNumPendingFunc = 0;

    assert(file::is_regular_file(preloadFilePath));

    File *preloadFile = mem::malloc<File>();
    file::FileErrCode errCode = file::read_file(&preloadFile->finfo, preloadFilePath);
    if (errCode != file::FileErrCode::kSuccess) assert(false);

    cmp.preloadPkg = mem::malloc<Package>();
    cmp.preloadPkg->globalDeclList = {};
    cmp.preloadPkg->files.init(1);
    cmp.preloadPkg->files.add(preloadFile);
    cmp.preloadPkg->globalDecls.init();
    preloadFile->package = cmp.preloadPkg;

    File *file = mem::malloc<File>();
    file->package = cmp.preloadPkg;
    cmp.preloadPkg->files.add(file);
    ErrCode parseRes = resolve_file(cmp, file, preloadFilePath);
    if (parseRes != ErrCode::kSuccess) assert(false);

    builtin_type::none = mem::malloc<Type>();
    builtin_type::none->kind = TypeKind::kNone;

    builtin_type::u16 = mem::malloc<Type>();
    builtin_type::u16->kind = TypeKind::kNamed;
    builtin_type::u16->name.ident = {"u16", 3};

    builtin_type::string = mem::malloc<Type>();
    builtin_type::string->kind = TypeKind::kNamed;
    builtin_type::string->name.ident = {"string", 6};

    cmp.currFile = preloadFile;
    if (analyze_package(&cmp, cmp.preloadPkg) != kAccept) assert(false);

    DeclInfo **u16Ref = cmp.preloadPkg->globalDecls[builtin_type::u16->name.ident];
    assert(u16Ref);
    builtin_type::u16->name.ref = (*u16Ref)->declNode;

    DeclInfo **stringRef = cmp.preloadPkg->globalDecls[builtin_type::string->name.ident];
    assert(stringRef);
    builtin_type::string->name.ref = (*stringRef)->declNode;

    return cmp;
}

void join_path(LString &dir, LStringView &file) {
    if (file.len != 1 || file.src[0] != '.') {
        lstr_cat(dir, "/");
        lstr_cat(dir, file);
    } else {
        dir.data[dir.size - 1] = '\0';
    }
}

ErrCode resolve_packages(CompilationContext &cmp, const char *mainFile) {
    cmp.packageMap.init();

    struct ImportContext {
        File *srcFile;
        LStringView importName;
    };
    LList<ImportContext> importStack{};

    // TODO: handle better way of hardcoding path to standard lib files
    // const char *libDir = "./lib";
    const char *libDir = "C:/Users/berkx/Desktop/lcc/lib";
    size_t libDirLen = 30;
    LString libDirBuf = lstr_create(libDir);

    LStringView rootDir = file::split_dir(mainFile);
    importStack.add({nullptr, {".", 1}});
    LString rootDirBuf = lstr_create(rootDir);

    LList<LString> filenames{};
    while (importStack.size) {
        // Get next import to resolve
        ImportContext &importCtx = importStack.last();
        importStack.size--;

        if (!cmp.packageMap[importCtx.importName]) {
            // Look through all possible directories for package name
            rootDirBuf.size = rootDir.len + 1;
            join_path(rootDirBuf, importCtx.importName);
            if (file::file_in_dir(filenames, rootDirBuf) == file::FileErrCode::kNotFound) {
                libDirBuf.size = libDirLen + 1;
                join_path(libDirBuf, importCtx.importName);
                if (file::file_in_dir(filenames, libDirBuf) == file::FileErrCode::kNotFound) {
                    err("Could not find package '%s' imported by '%s'\n", lstr_raw_str(importCtx.importName),
                        importCtx.srcFile->finfo->path);
                    printf("Checked directories:\n");
                    printf("\t  %s\n", lstr_raw_str(rootDir));
                    printf("\t  %s\n", libDir);
                    return ErrCode::kFailure;
                }
            }
            info("Importing package '%s' ...\n", lstr_raw_str(importCtx.importName));

            // Find all files in the package
            Package *pkg = mem::malloc<Package>();
            pkg->globalDeclList = {};
            pkg->files = {};
            pkg->globalDecls.init();
            cmp.packageMap.try_put(importCtx.importName, pkg);
            for (size_t i = 0; i < filenames.size; i++) {
                File *file = mem::malloc<File>();
                file->package = pkg;
                pkg->files.add(file);
                if (resolve_file(cmp, file, filenames[i].data) == ErrCode::kFailure) return ErrCode::kFailure;
            }

            // Make sure package aliases don't confict with declaration names
            for (size_t i = 0; i < pkg->files.size; i++) {
                File *file = pkg->files[i];
                Node *curr = file->importListHead;
                while (curr) {
                    if (!cmp.packageMap[curr->import.package]) importStack.add({file, curr->import.package});
                    if (DeclInfo **declInfo = pkg->globalDecls[curr->import.alias]) {
                        dx_err(at_node(file->finfo, curr),
                               "Package alias cannot have the same name as a declaration\n");
                        dx_note(at_node((*declInfo)->file->finfo, (*declInfo)->declNode), "Declaration found here\n");
                        return ErrCode::kFailure;
                    }
                    curr = curr->import.nextImport;
                }
            }
        }  // end of package resolution

        filenames.size = 0;
    }
    return ErrCode::kSuccess;
}

}  // namespace

ErrCode command_line(int argc, char **argv) {
    FlagParserInfo finfo{argv, argc};
    if (parse_args(finfo) == ErrCode::kFailure) {
        return ErrCode::kFailure;
    }

    if (compile(finfo.inputFile) != ErrCode::kSuccess) {
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

    // TODO: figure out a better way to hardcode the preload file
    // CompilationContext cmp = preload("./lib/preload.tc");
    CompilationContext cmp = preload("C:/Users/berkx/Desktop/lcc/lib/preload.tc");
    if (resolve_packages(cmp, path) == ErrCode::kFailure) return ErrCode::kFailure;

    LStringView root{".", 1};
    Package *pkg = *cmp.packageMap[root];
    File *file = pkg->files[0];
    info("Compiling %s ...\n", file->finfo->path);

    // Semantic analysis of the file
    cmp.currFile = file;
    if (analyze_package(&cmp, pkg)) return ErrCode::kFailure;

    LList<DeclInfo *> &globalDecls = (*cmp.packageMap[root])->globalDeclList;
    for (size_t i = 0; i < globalDecls.size; i++) {
        print_ast(globalDecls[i]->declNode);
    }

    // Translation into cfg and intraprocedural optimizations
    Package *package = *cmp.packageMap[root];
    for (size_t i = 0; i < package->globalDeclList.size; i++) {
        Node *decl = package->globalDeclList[i]->declNode;
        if (!decl->decl.rval) continue;
        if (decl->decl.rval->kind != NodeKind::kFunc) continue;
        if (decl->decl.rval->func.body->kind == NodeKind::kBlock) {
            CFG cfg;
            translate_function(cfg, decl->decl.rval);
            optimize(cfg);
        }
    }

    return ErrCode::kSuccess;
}

}  // namespace lcc
