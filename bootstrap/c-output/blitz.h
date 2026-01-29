#ifndef BLITZ_H
#define BLITZ_H

#include "blitz_types.h"

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


#endif // BLITZ_H
