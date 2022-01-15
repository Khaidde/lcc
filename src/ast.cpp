#include "ast.hpp"

#include "print.hpp"

namespace lcc {

namespace builtin_type {

Type *none{nullptr};
Type *u16{nullptr};
Type *string{nullptr};

}  // namespace builtin_type

const char *type_string(Type *type) {
    switch (type->kind) {
        case TypeKind::kNone: return "none";
        case TypeKind::kType: return "type";
        case TypeKind::kNamed: {
            LString res = lstr_create(type->name.ident);
            if (type->name.ref && !lstr_equal(type->name.ident, type->name.ref->decl.lval->name.ident)) {
                lstr_cat(res, "|aka ");
                lstr_cat(res, type->name.ref->decl.lval->name.ident);
                lstr_cat(res, "|");
            }
            return res.data;
        }
        case TypeKind::kPtr: {
            LString res = lstr_create("*");
            lstr_cat(res, type_string(type->ptr.inner));
            return res.data;
        }
        case TypeKind::kFuncTy: {
            LString res = lstr_create("(");
            for (size_t i = 0; i < type->funcTy.paramTys.size; i++) {
                lstr_cat(res, type_string(type->funcTy.paramTys.get(i)));
                if (i + 1 < type->funcTy.paramTys.size) {
                    lstr_cat(res, ", ");
                }
            }
            lstr_cat(res, ") -> ");
            if (type->funcTy.retTy) {
                lstr_cat(res, type_string(type->funcTy.retTy));
            } else {
                lstr_cat(res, "none");
            }
            return res.data;
        }
    }
}

namespace {

void r_print_ast(Node *node, size_t depth) {
    print_color(kAnsiColorGreen);
    printf("%0*s%s", depth * 2, "", node_kind_string(node->kind));
    reset_print_color();
    switch (node->kind) {
        case NodeKind::kImport:
            print_color(kAnsiColorBlue);
            printf(" '%s' as %s\n", lstr_raw_str(node->import.package), lstr_raw_str(node->import.alias));
            reset_print_color();
            break;
        case NodeKind::kDecl:
            print_color(kAnsiColorGrey);

            print_color(kAnsiColorMagenta);
            if (node->decl.isDecl) {
                printf(" <declare>");
            } else {
                printf(" <assign>");
            }

            if (node->decl.resolvedTy) {
                print_color(kAnsiColorYellow);
                printf(" '%s'", type_string(node->decl.resolvedTy));
            } else if (node->decl.staticTy) {
                print_color(kAnsiColorRed);
                printf(" '%s'", type_string(&node->decl.staticTy->type));
            } else if (node->decl.isDecl) {
                printf(" unknown");
            }
            if (node->decl.isExtern) {
                reset_print_color();
                printf(" #extern");
            }
            printf("\n");
            reset_print_color();

            r_print_ast(node->decl.lval, depth + 1);
            if (node->decl.rval) r_print_ast(node->decl.rval, depth + 1);
            break;
        case NodeKind::kType:
            print_color(kAnsiColorRed);
            printf("'%s'\n", type_string(&node->type));
            reset_print_color();
            break;
        case NodeKind::kIntLit:
            print_color(kAnsiColorBlue);
            printf(" %d\n", node->intLit.intVal);
            reset_print_color();
            break;
        case NodeKind::kStrLit:
            print_color(kAnsiColorBlue);
            printf(" %s\n", lstr_raw_str(node->strLit.strVal));
            reset_print_color();
            break;
        case NodeKind::kName:
            print_color(kAnsiColorBlue);
            printf(" '%.*s'\n", node->name.ident.len, node->name.ident.src);
            reset_print_color();
            break;
        case NodeKind::kPrefix:
            print_color(kAnsiColorBlue);
            printf(" '%s'\n", token_type_string(node->prefix.op));
            reset_print_color();

            r_print_ast(node->prefix.inner, depth + 1);
            break;
        case NodeKind::kInfix:
            print_color(kAnsiColorBlue);
            printf(" '%s'\n", token_type_string(node->infix.op));
            reset_print_color();

            r_print_ast(node->infix.left, depth + 1);
            r_print_ast(node->infix.right, depth + 1);
            break;
        case NodeKind::kCall:
            printf("\n");

            r_print_ast(node->call.callee, depth + 1);
            for (size_t i = 0; i < node->call.args.size; i++) {
                r_print_ast(node->call.args.get(i), depth + 1);
            }
            break;
        case NodeKind::kFunc:
            if (node->func.staticRetTy) {
                print_color(kAnsiColorYellow);
                printf(" '%s'", type_string(&node->func.staticRetTy->type));
                reset_print_color();
            }
            printf("\n");

            for (size_t i = 0; i < node->func.params.size; i++) {
                r_print_ast(node->func.params.get(i), depth + 1);
            }
            r_print_ast(node->func.body, depth + 1);
            break;
        case NodeKind::kBlock:
            if (node->block.branchLevel + 1 != 0) {  // Check that branchLevel != SIZE_MAX
                printf(" <level:%d>", node->block.branchLevel);
            }
            printf("\n");

            for (size_t i = 0; i < node->block.stmts.size; i++) {
                r_print_ast(node->block.stmts.get(i), depth + 1);
            }
            break;
        case NodeKind::kIf:
            if (node->ifstmt.branchLevel + 1 != 0) {  // Check that branchLevel != SIZE_MAX
                printf(" <level:%d>", node->ifstmt.branchLevel);
            }
            printf("\n");

            r_print_ast(node->ifstmt.cond, depth + 1);
            r_print_ast(node->ifstmt.then, depth + 1);
            if (node->ifstmt.alt) r_print_ast(node->ifstmt.alt, depth + 1);
            break;
        case NodeKind::kWhile:
            if (node->whilestmt.info->branchLevel + 1 != 0) {  // Check that branchLevel != SIZE_MAX
                printf(" <level:%d>", node->whilestmt.info->branchLevel);
            }
            if (node->whilestmt.label.src) {
                printf(" <%s>", lstr_raw_str(node->whilestmt.label));
            }
            printf("\n");

            r_print_ast(node->whilestmt.cond, depth + 1);
            r_print_ast(node->whilestmt.loop, depth + 1);
            break;
        case NodeKind::kRet:
            printf("\n");

            if (node->ret.value) {
                r_print_ast(node->ret.value, depth + 1);
            }
            break;
        case NodeKind::kLoopBr:
            printf(" <%s>", node->loopbr.isBreak ? "break" : "continue");
            if (node->loopbr.label.src) {
                print_color(kAnsiColorYellow);
                printf(" %s", lstr_raw_str(node->loopbr.label));
                reset_print_color();
            }
            printf("\n");
            break;
    }
}

}  // namespace

const char *node_kind_string(NodeKind kind) {
    switch (kind) {
        case NodeKind::kImport: return "Import";
        case NodeKind::kDecl: return "Decl";
        case NodeKind::kType: return "Type";
        case NodeKind::kIntLit: return "IntLit";
        case NodeKind::kStrLit: return "StrLit";
        case NodeKind::kName: return "Name";
        case NodeKind::kPrefix: return "Prefix";
        case NodeKind::kInfix: return "Infix";
        case NodeKind::kCall: return "Call";
        case NodeKind::kFunc: return "Func";
        case NodeKind::kBlock: return "Block";
        case NodeKind::kIf: return "If";
        case NodeKind::kWhile: return "While";
        case NodeKind::kRet: return "Ret";
        case NodeKind::kLoopBr: return "LoopBranch";
    }
}

void print_ast(Node *node) { r_print_ast(node, 0); }

}  // namespace lcc
