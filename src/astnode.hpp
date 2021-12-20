#ifndef LCC_ASTNODE_HPP
#define LCC_ASTNODE_HPP

#include "lexer.hpp"
#include "list.hpp"

namespace lcc {

enum class NodeType {
    kUnit,
    kDecl,
    kIntLit,
    kName,
    kPrefix,
    kInfix,
    kFunc,
    kBlock,
    kRet,
};

struct Node;

struct UnitNode {
    LList<Node *> decls;
};

struct DeclNode {
    LStringView name;
    Node *type;
    Node *value;
};

struct IntLitNode {
    u16 intVal;
};

struct NameNode {
    LStringView ident;
};

struct PrefixNode {
    TokenType op;
    Node *inner;
};

struct InfixNode {
    Node *left;
    TokenType op;
    Node *right;
};

struct FuncNode {
    LList<Node *> params;
    Node *retType;
    Node *body;
};

struct BlockNode {
    LList<Node *> stmts;
};

struct RetNode {
    Node *value;
};

struct Node {
    NodeType type;
    size_t startI;
    size_t endI;

    union {
        UnitNode unit;
        DeclNode decl;
        IntLitNode intLit;
        NameNode name;
        PrefixNode prefix;
        InfixNode infix;
        FuncNode func;
        BlockNode block;
        RetNode ret;
    } data;
};

const char *node_type_string(NodeType type);

void print_ast(Node *node);

}  // namespace lcc

#endif
