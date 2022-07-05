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
                lstr_cat(res, type_string(type->funcTy.paramTys[i]));
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

size_t get_byte_size(Type *type) {
    switch (type->kind) {
        case TypeKind::kNone: return 0;
        case TypeKind::kType: return 0;
        case TypeKind::kNamed:
            if (type->name.ref == builtin_type::u16->name.ref) return 2;
            assert(!"TODO: calculate size of type referred to by name");
        case TypeKind::kPtr: return 2;
        case TypeKind::kFuncTy: assert(!"TODO: calculate size of funcTy");
    }
    unreachable();
}

namespace {

void r_print_ast(Node *node, size_t depth) {
    set_color(kColorGreen);
    printf("%0*s%s", depth * 2, "", node_kind_string(node->kind));
    reset_color();
    switch (node->kind) {
        case NodeKind::kImport:
            set_color(kColorBlue);
            printf(" '%s' as %s\n", lstr_raw_str(node->import.package), lstr_raw_str(node->import.alias));
            reset_color();
            break;
        case NodeKind::kDecl:
            set_color(kColorWhite);

            set_color(kColorMagenta);
            if (node->decl.isDecl) {
                printf(" <declare>");
            } else {
                printf(" <assign>");
            }

            if (node->decl.resolvedTy) {
                set_color(kColorYellow);
                printf(" '%s'", type_string(node->decl.resolvedTy));
            } else if (node->decl.staticTy) {
                set_color(kColorRed);
                printf(" '%s'", type_string(&node->decl.staticTy->type));
            } else if (node->decl.isDecl) {
                printf(" unknown");
            }
            if (node->decl.isExtern) {
                reset_color();
                printf(" #extern");
            }
            printf("\n");
            reset_color();

            r_print_ast(node->decl.lval, depth + 1);
            if (node->decl.rval) r_print_ast(node->decl.rval, depth + 1);
            break;
        case NodeKind::kType:
            set_color(kColorRed);
            printf("'%s'\n", type_string(&node->type));
            reset_color();
            break;
        case NodeKind::kIntLit:
            set_color(kColorBlue);
            printf(" %d\n", node->intLit.intVal);
            reset_color();
            break;
        case NodeKind::kStrLit:
            set_color(kColorBlue);
            printf(" %s\n", lstr_raw_str(node->strLit.strVal));
            reset_color();
            break;
        case NodeKind::kName:
            set_color(kColorBlue);
            printf(" '%.*s'\n", node->name.ident.len, node->name.ident.src);
            reset_color();
            break;
        case NodeKind::kPrefix:
            set_color(kColorBlue);
            printf(" '%s'\n", token_type_string(node->prefix.op));
            reset_color();

            r_print_ast(node->prefix.inner, depth + 1);
            break;
        case NodeKind::kInfix:
            set_color(kColorBlue);
            printf(" '%s'\n", token_type_string(node->infix.op));
            reset_color();

            r_print_ast(node->infix.left, depth + 1);
            r_print_ast(node->infix.right, depth + 1);
            break;
        case NodeKind::kCall:
            printf("\n");

            r_print_ast(node->call.callee, depth + 1);
            for (size_t i = 0; i < node->call.args.size; i++) {
                r_print_ast(node->call.args[i], depth + 1);
            }
            break;
        case NodeKind::kFunc:
            if (node->func.staticRetTy) {
                set_color(kColorYellow);
                printf(" '%s'", type_string(&node->func.staticRetTy->type));
                reset_color();
            }
            printf("\n");

            for (size_t i = 0; i < node->func.params.size; i++) {
                r_print_ast(node->func.params[i], depth + 1);
            }
            r_print_ast(node->func.body, depth + 1);
            break;
        case NodeKind::kBlock:
            if (node->block.branchLevel + 1 != 0) {  // Check that branchLevel != SIZE_MAX
                printf(" <level:%d>", node->block.branchLevel);
            }
            printf("\n");

            for (StatementListNode *stmtNode = node->block.start; stmtNode; stmtNode = stmtNode->next) {
                r_print_ast(stmtNode->stmt, depth + 1);
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
            if (node->whilestmt.branchLevel + 1 != 0) {  // Check that branchLevel != SIZE_MAX
                printf(" <level:%d>", node->whilestmt.branchLevel);
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
                set_color(kColorYellow);
                printf(" %s", lstr_raw_str(node->loopbr.label));
                reset_color();
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
