#include "astnode.hpp"

#include "print.hpp"

namespace lcc {

namespace {

void print_type(Node *node) {
    switch (node->type) {
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
                Node *decl = node->data.unit.decls.data[i];
                r_print_ast(decl, depth + 1);
            }
            break;
        case NodeType::kDecl:
            print_color(kAnsiColorMagenta);
            printf(" '%.*s'", node->data.decl.name.len, node->data.decl.name.src);
            reset_print_color();

            printf(":");

            if (node->data.decl.type) {
                print_color(kAnsiColorYellow);
                printf("'");
                print_type(node->data.decl.type);
                printf("'");
                reset_print_color();
            } else {
                printf("unknown");
            }
            printf("\n");

            if (node->data.decl.value) r_print_ast(node->data.decl.value, depth + 1);
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
        case NodeType::kFunc:
            if (node->data.func.retType) {
                print_color(kAnsiColorYellow);
                printf(" '");
                print_type(node->data.func.retType);
                printf("'");
                print_color(kAnsiColorYellow);
            }
            printf("\n");

            for (size_t i = 0; i < node->data.func.params.size; i++) {
                Node *decl = node->data.func.params.data[i];
                r_print_ast(decl, depth + 1);
            }
            r_print_ast(node->data.func.body, depth + 1);
            break;
        case NodeType::kBlock:
            printf("\n");

            for (size_t i = 0; i < node->data.block.stmts.size; i++) {
                Node *stmt = node->data.block.stmts.data[i];
                r_print_ast(stmt, depth + 1);
            }
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
        case NodeType::kIntLit: return "IntLit";
        case NodeType::kName: return "Name";
        case NodeType::kPrefix: return "Prefix";
        case NodeType::kInfix: return "Infix";
        case NodeType::kFunc: return "Func";
        case NodeType::kBlock: return "Block";
        case NodeType::kRet: return "Ret";
    }
}

void print_ast(Node *node) { r_print_ast(node, 0); }

}  // namespace lcc
