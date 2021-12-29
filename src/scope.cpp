#include "scope.hpp"

#include "diagnostic.hpp"
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
    assert(decl->decl.lval->kind == NodeKind::kName && "Lvalue of decl should be a name");
    if (Node **other = stack->scopes[stack->size - 1].try_put(decl->decl.lval->name.ident, decl)) {
        return *other;
    } else {
        return nullptr;
    }
}

Node *decl_lookup(CompilationContext *cmp, LStringView &symbol) {
    if (cmp->ctx.file->scopeStack) {
        for (int i = cmp->ctx.file->scopeStack->size - 1; i >= 0; i--) {
            if (Node **decl = cmp->ctx.file->scopeStack->scopes[i].get(symbol)) {
                return *decl;
            }
        }
    }
    if (Node **globalDecl = cmp->ctx.package->globalDecls.get(symbol)) {
        return *globalDecl;
    }
    if (Node **import = cmp->ctx.file->unit->unit.imports.get(symbol)) {
        return *import;
    }
    return nullptr;
}

CompilationContext resolve_packages(const char *mainFile) {
    CompilationContext cmp;
    cmp.packageMap.init();

    struct ImportContext {
        File *file;
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
                    importCtx.file->finfo->path);
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
                file::FileInfo *finfo;
                Node *unit;
                if (file::read_file(&finfo, filenames.get(i).data) != file::FileErrCode::kSuccess) {
                    err("Failed to read file: %s\n", filenames.get(i).data);
                    cmp.isPackageResolutionSuccesful = false;
                    return cmp;
                }
                {
                    Lexer lexer{};
                    lexer.finfo = finfo;
                    unit = parse_unit(&lexer);
                    if (!unit) {
                        cmp.isPackageResolutionSuccesful = false;
                        return cmp;
                    }
                }
                File *file = mem::malloc<File>();
                file->finfo = finfo;
                file->unit = unit;
                file->scopeStack = nullptr;
                pkg->files.add(file);

                // Add all declarations in current file to package symbol table
                for (size_t k = 0; k < unit->unit.decls.size; k++) {
                    Node *decl = unit->unit.decls.get(k);
                    decl->decl.package = pkg;
                    decl->decl.file = file;

                    if (decl->decl.lval->kind != NodeKind::kName) {
                        dx_err(at_node(finfo, decl->decl.lval), "Must only declare variable names in global scope\n");
                        cmp.isPackageResolutionSuccesful = false;
                        return cmp;
                    }

                    LStringView &declName = decl->decl.lval->name.ident;
                    if (Node **other = pkg->globalDecls.try_put(declName, decl)) {
                        dx_err(at_node(file->finfo, decl->decl.lval), "Duplicate declaration\n");
                        dx_note(at_node((*other)->decl.file->finfo, (*other)->decl.lval),
                                "Previous declaration here\n");
                        cmp.isPackageResolutionSuccesful = false;
                        return cmp;
                    }
                }

                // Add pending imports in current file to import stack
                for (size_t k = 0; k < unit->unit.imports.capacity; k++) {
                    if (unit->unit.imports.table[k].psl) {
                        Node *import = unit->unit.imports.table[k].val;
                        if (!cmp.packageMap.get(import->import.package)) {
                            importStack.add({file, import->import.package});
                        }
                    }
                }
            }

            // Make sure package aliases don't confict with declaration names
            for (size_t i = 0; i < pkg->files.size; i++) {
                File *file = pkg->files.get(i);
                Node *unit = file->unit;
                for (size_t k = 0; k < unit->unit.imports.capacity; k++) {
                    if (unit->unit.imports.table[k].psl) {
                        Node *import = unit->unit.imports.table[k].val;
                        if (Node **decl = pkg->globalDecls.get(import->import.alias)) {
                            dx_err(at_node(file->finfo, import),
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

}  // namespace lcc
