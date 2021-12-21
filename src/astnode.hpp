#ifndef LCC_ASTNODE_HPP
#define LCC_ASTNODE_HPP

#include "lexer.hpp"
#include "list.hpp"

namespace lcc {

enum class NodeType {
    kUnit,
    kDecl,
    kFuncTy,
    kIntLit,
    kName,
    kPrefix,
    kInfix,
    kCall,
    kFunc,
    kBlock,
    kIf,
    kWhile,
    kRet,
};

struct Node;

struct UnitNode {
    LList<Node *> decls;
};

struct DeclNode {
    Node *lval;
    Node *ty;
    Node *rval;
    bool isDecl;
};

struct FuncTypeNode {
    LList<Node *> argTys;
    Node *retTy;
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

struct CallNode {
    Node *callee;
    LList<Node *> args;
};

struct FuncNode {
    LList<Node *> params;
    Node *retTy;
    Node *body;
};

struct BlockNode {
    LList<Node *> stmts;
};

struct IfNode {
    Node *cond;
    Node *then;
    Node *alt;
};

struct WhileNode {
    Node *cond;
    Node *loop;
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
        FuncTypeNode funcTy;
        IntLitNode intLit;
        NameNode name;
        PrefixNode prefix;
        InfixNode infix;
        CallNode call;
        FuncNode func;
        BlockNode block;
        IfNode ifstmt;
        WhileNode whilestmt;
        RetNode ret;
    } data;
};

const char *node_type_string(NodeType type);

void print_ast(Node *node);

}  // namespace lcc

#endif
