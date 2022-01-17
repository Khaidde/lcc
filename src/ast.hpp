#ifndef LCC_AST_HPP
#define LCC_AST_HPP

#include <cstdint>

#include "list.hpp"
#include "lstring.hpp"
#include "token.hpp"
#include "translate.hpp"

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

struct DeclNode {
    Node *lval;
    Node *staticTy;
    Node *rval;

    Type *resolvedTy;

    bool isDecl : 1;
    union {
        bool isGlobal : 1;
        bool isAssignToGlobal : 1;
    };
    bool isExtern : 1;
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

struct StatementListNode {
    Node *stmt;
    StatementListNode *next;
};

struct BlockNode {
    StatementListNode *start;
    StatementListNode *head;
    size_t branchLevel;
};

struct IfNode {
    Node *cond;
    Node *then;
    Node *alt;
    size_t branchLevel;
};

struct WhileInfo {
    size_t branchLevel;
    BasicBlock *entry;
    BasicBlock *exit;
};

struct WhileNode {
    LStringView label;
    Node *cond;
    Node *loop;

    WhileInfo *info;
};

struct RetNode {
    Node *value;
};

struct LoopBranchNode {
    LStringView label;
    bool isBreak;
    Node *ref;
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
