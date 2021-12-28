#include "scope.hpp"

#include "diagnostics.hpp"
#include "parse.hpp"
#include "print.hpp"

namespace lcc {

ScopeStack *scope_init() {
    ScopeStack *table = mem::malloc<ScopeStack>();
    table->size = 0;
    return table;
}

void scope_enter(ScopeStack *stack) {
    assert(stack->size < kMaxScopeDepth && "Max scope depth exceeded: 8");
    stack->scopes[stack->size++].init();
}

void scope_exit(ScopeStack *stack) { mem::c_free(stack->scopes[--stack->size].table); }

Node *scope_bind(ScopeStack *stack, Node *decl) {
    assert(decl->data.decl.lval->type == NodeType::kName && "Lvalue of decl should be a name");
    if (Node **other = stack->scopes[stack->size - 1].try_put(decl->data.decl.lval->data.name.ident, decl)) {
        return *other;
    } else {
        return nullptr;
    }
}

Node *scope_lookup(Context *ctx, LStringView &symbol) {
    for (int i = ctx->currScopeStack->size - 1; i >= 0; i--) {
        if (Node **decl = ctx->currScopeStack->scopes[i].get(symbol)) {
            return *decl;
        }
    }
    return nullptr;
}

DeclContext *scope_lookup_global(CompilationContext *cmp, LStringView &symbol) {
    if (DeclContext *declCtx = cmp->ctx.currPackage->globalDecls.get(symbol)) {
        return declCtx;
    }
    for (size_t i = 0; i < cmp->ctx.currFile->unit->data.unit.imports.size; i++) {
        LStringView &importName = cmp->ctx.currFile->unit->data.unit.imports.get(i);
        if (Package **pkg = cmp->packageMap.get(importName)) {
            if (DeclContext *ctx = (*pkg)->globalDecls.get(symbol)) {
                return ctx;
            }
        }
    }
    return nullptr;
}

CompilationContext resolve_packages(const char *mainFile) {
    CompilationContext cmp;
    cmp.packageMap.init();

    struct ImportContext {
        FileUnit *fileUnit;
        LStringView importName;
    };
    LList<ImportContext> importStack{};

    LString rootDir = file::get_dir(mainFile);
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
                err("Could not find package '%s' imported by '%s'\n", lstr_create(importCtx.importName).data,
                    importCtx.fileUnit->finfo->path);
                break;
            }
            info("Importing package '%s' ...\n", lstr_create(importCtx.importName).data);

            Package *pkg = mem::malloc<Package>();
            pkg->files = {};
            pkg->globalDecls.init();
            cmp.packageMap.try_put(importCtx.importName, pkg);

            for (size_t i = 0; i < filenames.size; i++) {
                // Parse file
                file::FileInfo *fileinfo;
                Node *unit;
                if (file::read_file(&fileinfo, filenames.get(i).data) != file::FileErrCode::kSuccess) {
                    err("Failed to read file: %s\n", filenames.get(i).data);
                    cmp.isPackageResolutionSuccesful = false;
                    return cmp;
                }
                {
                    Lexer lexer{};
                    lexer.finfo = fileinfo;
                    unit = parse_unit(&lexer);
                    if (!unit) {
                        cmp.isPackageResolutionSuccesful = false;
                        return cmp;
                    }
                }
                pkg->files.add({fileinfo, unit});

                FileUnit *fileUnit = &pkg->files.last();

                // Add all declarations in current file to package symbol table
                for (size_t k = 0; k < unit->data.unit.decls.size; k++) {
                    DeclContext declCtx{pkg, fileUnit, unit->data.unit.decls.get(k)};

                    if (declCtx.decl->data.decl.lval->type != NodeType::kName) {
                        dx_err(at_node(fileinfo, declCtx.decl->data.decl.lval),
                               "Must only declare variable names in global scope\n");
                        cmp.isPackageResolutionSuccesful = false;
                        return cmp;
                    }

                    LStringView &declName = declCtx.decl->data.decl.lval->data.name.ident;
                    if (DeclContext *other = pkg->globalDecls.try_put(declName, declCtx)) {
                        dx_err(at_node(fileUnit->finfo, declCtx.decl->data.decl.lval), "Found duplicate declaration\n");
                        dx_err(at_node(other->fileUnit->finfo, other->decl->data.decl.lval),
                               "Previous declaration found here\n");
                        cmp.isPackageResolutionSuccesful = false;
                        return cmp;
                    }
                }

                // Add pending imports in current file to import stack
                for (size_t k = 0; k < unit->data.unit.imports.size; k++) {
                    LStringView &importName = unit->data.unit.imports.get(k);
                    if (!cmp.packageMap.get(importName)) {
                        importStack.add({fileUnit, importName});
                    }
                }
            }
        }  // end of package resolution

        filenames.size = 0;
    }
    cmp.isPackageResolutionSuccesful = true;
    return cmp;
}

}  // namespace lcc
