#ifndef LCC_AST_HPP
#define LCC_AST_HPP

#include <cstdint>

#include "list.hpp"
#include "lstring.hpp"
#include "token.hpp"

namespace lcc {

enum class TypeKind {
    kNone,
    kType,
    kNamed,
    kPtr,
    kFuncTy,
};

struct Type;

struct NamedType {
    LStringView ident;
    Node *ref;
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
        NamedType name;
        PtrType ptr;
        FuncType funcTy;
    };
};

namespace builtin_type {

extern Type *none;
extern Type *u16;
extern Type *string;

}  // namespace builtin_type

const char *type_string(Type *type);

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
    kLoopBr,
};

struct Node;

struct ImportNode {
    LStringView alias;
    LStringView package;

    Node *nextImport;
};

struct DeclInfo {
    struct File *file;

    Type *resolvedTy;

    Node *nextDecl;

    bool isDecl : 1;
    bool isExtern : 1;
    bool isResolving : 1;
    bool isBound : 1;
    bool isUsed : 1;
};

struct DeclNode {
    Node *lval;
    Node *staticTy;
    Node *rval;

    DeclInfo *info;
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
    Node *staticRetTy;
    Node *body;
};

struct BlockNode {
    LList<Node *> stmts;
    size_t branchLevel;
};

struct IfNode {
    Node *cond;
    Node *then;
    Node *alt;
    size_t branchLevel;
};

struct WhileNode {
    LStringView label;
    Node *cond;
    Node *loop;
    size_t branchLevel;
};

struct RetNode {
    Node *value;
};

struct LoopBranchNode {
    LStringView label;
    bool isBreak;
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
        LoopBranchNode loopbr;
    };
};

const char *node_kind_string(NodeKind kind);

void print_ast(Node *node);

void print_decl_list(Node *decl);

}  // namespace lcc

#endif
