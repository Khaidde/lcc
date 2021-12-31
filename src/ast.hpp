#ifndef LCC_AST_HPP
#define LCC_AST_HPP

#include <cstdint>

#include "list.hpp"
#include "lstring.hpp"
#include "token.hpp"

namespace lcc {

enum class TypeKind {
    kNone,
    kBase,
    kPtr,
    kFuncTy,
};

enum class BaseTypeKind {
    u16,
    string,
};

const char *base_type_string(BaseTypeKind kind);

struct Type;
struct Node;

struct BaseType {
    BaseTypeKind kind;
};

struct PtrType {
    Type *inner;
};

struct FuncType {
    LList<Type *> paramTys;
    Type *retTy;
};

struct Type {
    TypeKind kind;
    union {
        BaseType base;
        PtrType ptr;
        FuncType func;
    };
};

const char *type_string(Type *type);

namespace builtin_type {

extern Type none;
extern Type u16;
extern Type string;

}  // namespace builtin_type

enum class NodeKind {
    kImport,
    kDecl,
    kType,
    kIntLit,
    kStrLit,
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

struct ImportNode {
    LStringView alias;
    LStringView package;
};

struct DeclNode {
    Node *lval;
    Node *staticTy;
    Node *rval;

    struct File *file;

    Type *resolvedTy;

    bool isDecl : 1;
    bool isVisited : 1;
    bool isBound : 1;
};

struct IntLitNode {
    uint16_t intVal;
};

struct StrLitNode {
    LStringView strVal;
};

struct NameNode {
    LStringView ident;
    Node *ref;
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
    NodeKind kind;
    size_t startI;
    size_t endI;

    union {
        ImportNode import;
        DeclNode decl;
        Type type;
        IntLitNode intLit;
        StrLitNode strLit;
        NameNode name;
        PrefixNode prefix;
        InfixNode infix;
        CallNode call;
        FuncNode func;
        BlockNode block;
        IfNode ifstmt;
        WhileNode whilestmt;
        RetNode ret;
    };
};

const char *node_kind_string(NodeKind kind);

void print_ast(Node *node);

}  // namespace lcc

#endif
