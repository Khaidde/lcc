#include "astnode.hpp"

#include "print.hpp"

namespace lcc {

namespace builtin_type {

Type none = {TypeKind::kNone, {}};
Type u16 = {TypeKind::kBase, {{BaseTypeKind::u16}}};

}  // namespace builtin_type

const char *base_type_string(BaseTypeKind kind) {
    switch (kind) {
        case BaseTypeKind::u16: return "u16";
    }
}

// TODO: optimize this, particularly look at all string creations in function type
const char *type_string(Type *type) {
    switch (type->kind) {
        case TypeKind::kNone: return "none";
        case TypeKind::kBase: return base_type_string(type->data.base.kind);
        case TypeKind::kPtr: {
            LString res = lstr_create("*");
            lstr_cat(res, type_string(type->data.ptr.inner));
            return res.data;
        }
        case TypeKind::kFuncTy: {
            LString res = lstr_create("(");
            for (size_t i = 0; i < type->data.func.paramTys.size; i++) {
                lstr_cat(res, type_string(type->data.func.paramTys.data[i]));
                if (i + 1 < type->data.func.paramTys.size) {
                    lstr_cat(res, ", ");
                }
            }
            lstr_cat(res, ") -> ");
            if (type->data.func.retTy) {
                lstr_cat(res, type_string(type->data.func.retTy));
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
    printf("%0*s%s", depth * 2, "", node_type_string(node->type));
    reset_print_color();
    switch (node->type) {
        case NodeType::kUnit:
            for (size_t i = 0; i < node->data.unit.imports.size; i++) {
                printf(" %s", lstr_create(node->data.unit.imports.data[i]).data);
            }
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

            if (node->data.decl.resolvedTy) {
                print_color(kAnsiColorYellow);
                printf("'%s'", type_string(node->data.decl.resolvedTy));
                reset_print_color();
            } else if (node->data.decl.staticTy) {
                print_color(kAnsiColorRed);
                printf("'%s'", type_string(&node->data.decl.staticTy->data.type));
                reset_print_color();
            } else {
                printf("unknown");
            }
            printf("\n");

            r_print_ast(node->data.decl.lval, depth + 1);
            if (node->data.decl.rval) r_print_ast(node->data.decl.rval, depth + 1);
            break;
        case NodeType::kType:
            print_color(kAnsiColorRed);
            printf("'%s'", type_string(&node->data.type));
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
                print_color(kAnsiColorRed);
                printf(" '%s'", type_string(&node->data.func.retTy->data.type));
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
        case NodeType::kType: return "Type";
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