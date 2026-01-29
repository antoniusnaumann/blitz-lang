# Test Case: Type Name Collision

## Problem
Two files define types with the same name:
- compiler/ast/expression.blitz:72 defines `struct Assignment`
- compiler/parser/expression.blitz:166 defines `union Assignment` (enum)

When transpiled to C, both become `typedef ... Assignment` causing redefinition error.

## Solution
Add a hash-based suffix to type names when collisions are detected:
- `struct Assignment` -> `Assignment_ast` (from AST module)
- `union Assignment` -> `Assignment_parser` (from parser module)

Or simpler: use a counter:
- First `Assignment` -> `Assignment`
- Second `Assignment` -> `Assignment_1`

