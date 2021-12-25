#ifndef LCC_ASTNODE_HPP
#define LCC_ASTNODE_HPP

#include "lexer.hpp"
#include "list.hpp"

namespace lcc {

struct Type;
struct Node;

enum class TypeKind {
    kNone,
    kBase,
    kPtr,
    kFuncTy,
};

enum class BaseTypeKind {
    u16,
};

const char *base_type_string(BaseTypeKind kind);

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
    } data;
};

const char *type_string(Type *type);

namespace builtin_type {

extern Type none;
extern Type u16;

}  // namespace builtin_type

struct FileInfo {
    const char *path;
    LString src;
    Node *unit;
};

enum class NodeType {
    kUnit,
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

struct UnitNode {
    LList<LStringView> *imports;
    LList<Node *> *decls;
};

struct DeclNode {
    Node *lval;
    Node *staticTy;
    Node *rval;

    bool isDecl;
    bool isChecked;
    Type *resolvedTy;
};

struct IntLitNode {
    u16 intVal;
};

struct StrLitNode {
    LStringView strVal;
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
    } data;
};

const char *node_type_string(NodeType type);

void print_ast(Node *node);

}  // namespace lcc

#endif
