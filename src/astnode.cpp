#include "astnode.hpp"

#include "print.hpp"

namespace lcc {

namespace {

void print_type(Node *node) {
    switch (node->type) {
        case NodeType::kFuncTy:
            printf("(");
            for (size_t i = 0; i < node->data.funcTy.argTys.size; i++) {
                print_type(node->data.funcTy.argTys.data[i]);
                if (i + 1 < node->data.funcTy.argTys.size) {
                    printf(", ");
                }
            }
            printf(") -> ");
            if (node->data.funcTy.retTy) {
                print_type(node->data.funcTy.retTy);
            } else {
                printf("void");
            }
            break;
        case NodeType::kName: printf("%.*s", node->data.name.ident.len, node->data.name.ident.src); break;
        case NodeType::kPrefix:
            printf("%s", token_type_string(node->data.prefix.op));
            print_type(node->data.prefix.inner);
            break;
        default: printf("[TODO TYPE]");
    }
}

void r_print_ast(Node *node, size_t depth) {
    print_color(kAnsiColorGreen);
    printf("%0*s%s", depth * 2, "", node_type_string(node->type));
    reset_print_color();
    switch (node->type) {
        case NodeType::kUnit:
            printf("\n");
            for (size_t i = 0; i < node->data.unit.decls.size; i++) {
                r_print_ast(node->data.unit.decls.data[i], depth + 1);
            }
            break;
        case NodeType::kDecl:
            print_color(kAnsiColorMagenta);
            printf(" <%s>", node->data.decl.isDecl ? "declare" : "assign");
            reset_print_color();

            printf(":");

            if (node->data.decl.ty) {
                print_color(kAnsiColorYellow);
                printf("'");
                print_type(node->data.decl.ty);
                printf("'");
                reset_print_color();
            } else {
                printf("unknown");
            }
            printf("\n");

            r_print_ast(node->data.decl.lval, depth + 1);
            if (node->data.decl.rval) r_print_ast(node->data.decl.rval, depth + 1);
            break;
        case NodeType::kFuncTy:
            print_color(kAnsiColorYellow);
            printf(" '");
            print_type(node);
            printf("'\n");
            reset_print_color();
            break;
        case NodeType::kIntLit:
            print_color(kAnsiColorBlue);
            printf(" %d\n", node->data.intLit.intVal);
            reset_print_color();
            break;
        case NodeType::kName:
            print_color(kAnsiColorBlue);
            printf(" '%.*s'\n", node->data.name.ident.len, node->data.name.ident.src);
            reset_print_color();
            break;
        case NodeType::kPrefix:
            print_color(kAnsiColorBlue);
            printf(" '%s'\n", token_type_string(node->data.prefix.op));
            reset_print_color();

            r_print_ast(node->data.prefix.inner, depth + 1);
            break;
        case NodeType::kInfix:
            print_color(kAnsiColorBlue);
            printf(" '%s'\n", token_type_string(node->data.infix.op));
            reset_print_color();

            r_print_ast(node->data.infix.left, depth + 1);
            r_print_ast(node->data.infix.right, depth + 1);
            break;
        case NodeType::kCall:
            printf("\n");

            r_print_ast(node->data.call.callee, depth + 1);
            for (size_t i = 0; i < node->data.call.args.size; i++) {
                r_print_ast(node->data.call.args.data[i], depth + 1);
            }
            break;
        case NodeType::kFunc:
            if (node->data.func.retTy) {
                print_color(kAnsiColorYellow);
                printf(" '");
                print_type(node->data.func.retTy);
                printf("'");
                reset_print_color();
            }
            printf("\n");

            for (size_t i = 0; i < node->data.func.params.size; i++) {
                r_print_ast(node->data.func.params.data[i], depth + 1);
            }
            r_print_ast(node->data.func.body, depth + 1);
            break;
        case NodeType::kBlock:
            printf("\n");

            for (size_t i = 0; i < node->data.block.stmts.size; i++) {
                r_print_ast(node->data.block.stmts.data[i], depth + 1);
            }
            break;
        case NodeType::kIf:
            printf("\n");

            r_print_ast(node->data.ifstmt.cond, depth + 1);
            r_print_ast(node->data.ifstmt.then, depth + 1);
            if (node->data.ifstmt.alt) r_print_ast(node->data.ifstmt.alt, depth + 1);
            break;
        case NodeType::kWhile:
            printf("\n");

            r_print_ast(node->data.whilestmt.cond, depth + 1);
            r_print_ast(node->data.whilestmt.loop, depth + 1);
            break;
        case NodeType::kRet:
            printf("\n");
            if (node->data.ret.value) {
                r_print_ast(node->data.ret.value, depth + 1);
            }
            break;
    }
}

}  // namespace

const char *node_type_string(NodeType type) {
    switch (type) {
        case NodeType::kUnit: return "Unit";
        case NodeType::kDecl: return "Decl";
        case NodeType::kFuncTy: return "FuncTy";
        case NodeType::kIntLit: return "IntLit";
        case NodeType::kName: return "Name";
        case NodeType::kPrefix: return "Prefix";
        case NodeType::kInfix: return "Infix";
        case NodeType::kCall: return "Call";
        case NodeType::kFunc: return "Func";
        case NodeType::kBlock: return "Block";
        case NodeType::kIf: return "If";
        case NodeType::kWhile: return "While";
        case NodeType::kRet: return "Ret";
    }
}

void print_ast(Node *node) { r_print_ast(node, 0); }

}  // namespace lcc
