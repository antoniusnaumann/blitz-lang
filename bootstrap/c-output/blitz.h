#ifndef BLITZ_H
#define BLITZ_H

#include "blitz_types.h"

typedef enum {
    Definition_Pub,
    Definition_Fn,
    Definition_Struct,
    Definition_Union,
    Definition_Alias,
    Definition_Actor,
    Definition_Test
} Definition;

typedef struct {
    Box_Definition item;
} Pub;

typedef struct {
    Ident name;
    List_Arg args;
    Option_Type type;
    Block body;
    Span span;
} Fn;

typedef struct {
    Ident name;
    Type type;
    bool is_mut;
    Span span;
} Arg;

typedef struct {
    Type sig;
    List_Field fields;
    Span span;
} Struct;

typedef struct {
    Ident name;
    Type type;
    Span span;
} Field;

typedef struct {
    Type sig;
    List_Case cases;
    Span span;
} Union;

typedef struct {
    Option_Ident label;
    Option_Type type;
} Case;

typedef struct { char _dummy; } Alias;

typedef struct { char _dummy; } Actor;

typedef struct { char _dummy; } Test;

typedef struct {
    char* name;
    List_Type params;
    Span span;
} Type;

typedef struct {
    Span span;
    char* msg;
} Error;

typedef enum {
    Statement_Declaration,
    Statement_Expression
} Statement;

typedef struct {
    Ident name;
    Option_Type type;
    bool is_mut;
    Expression init;
    Span span;
} Declaration;

typedef enum {
    Expression_Constructor,
    Expression_Call,
    Expression_Member,
    Expression_Ident,
    Expression_Assignment,
    Expression_BinaryOp,
    Expression_UnaryOp,
    Expression_Assert,
    Expression_Mut,
    Expression_For,
    Expression_While,
    Expression_If,
    Expression_Switch,
    Expression_Block,
    Expression_Return,
    Expression_List_,
    Expression_Index,
    Expression_Group,
    Expression_Continue,
    Expression_Break,
    Expression_Lit,
    Expression_Lit,
    Expression_Lit,
    Expression_Lit,
    Expression_Lit
} Expression;

typedef struct {
    char* name;
    Span span;
} Ident;

typedef struct {
    Box_Expression parent;
    Ident member;
    Span span;
} Member;

typedef struct {
    Type type;
    List_CallArg args;
    bool ufcs;
    Span span;
} Constructor;

typedef struct {
    Ident name;
    List_CallArg args;
    bool ufcs;
    Span span;
} Call;

typedef struct {
    Option_Ident label;
    Expression init;
} CallArg;

typedef struct {
    Box_Expression left;
    Box_Expression right;
} Assignment;

typedef struct {
    Box_Expression iter;
    Ident elem;
    Block body;
    Span span;
} For;

typedef struct {
    Box_Expression cond;
    Block body;
    Span span;
} While;

typedef struct {
    Box_Expression cond;
    Block body;
    Span span;
} If;

typedef struct {
    Box_Expression cond;
    List_SwitchCase cases;
    Span span;
} Switch;

typedef struct {
    SwitchLabel label;
    Block body;
    Span span;
} SwitchCase;

typedef enum {
    SwitchLabel_Type,
    SwitchLabel_Ident,
    SwitchLabel_default
} SwitchLabel;

typedef struct {
    Box_Expression expr;
    Span span;
} Return;

typedef struct {
    Span span;
} Continue;

typedef struct {
    Span span;
} Break;

typedef struct {
    List_Statement statements;
    Span span;
} Block;

typedef struct {
    List_Expression elems;
    Span span;
} List_;

typedef struct {
    Box_Expression target;
    Box_Expression index;
    Span span;
} Index;

typedef struct {
    Box_Expression expr;
    Span span;
} Group;

typedef struct {
    BinaryOperator op;
    Box_Expression left;
    Box_Expression right;
    Span span;
} BinaryOp;

typedef struct {
    UnaryOperator op;
    Box_Expression expr;
    Span span;
} UnaryOp;

typedef struct {
    Box_Expression cond;
    Span span;
    bool ufcs;
} Assert;

typedef struct {
    Box_Expression expr;
    Span span;
    bool ufcs;
} Mut;

// TODO: Generic struct Lit - NOT IMPLEMENTED YET

typedef enum {
    Operator_BinaryOperator,
    Operator_UnaryOperator
} Operator;

typedef enum {
    BinaryOperator_add,
    BinaryOperator_sub,
    BinaryOperator_mul,
    BinaryOperator_div,
    BinaryOperator_rem,
    BinaryOperator_concat,
    BinaryOperator_dot,
    BinaryOperator_eq,
    BinaryOperator_ne,
    BinaryOperator_gt,
    BinaryOperator_ge,
    BinaryOperator_lt,
    BinaryOperator_le,
    BinaryOperator_and_,
    BinaryOperator_or_,
    BinaryOperator_else_
} BinaryOperator;

typedef enum {
    UnaryOperator_not,
    UnaryOperator_neg
} UnaryOperator;

typedef struct {
    Range range;
    char* path;
} Span;

typedef struct {
    int64_t line;
    int64_t col;
} Pos;


#endif // BLITZ_H
