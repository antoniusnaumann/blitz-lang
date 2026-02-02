# C Bootstrap Backend Implementation Plan

## Overview

Build a minimal C transpiler for the Blitz self-hosted compiler (~1676 lines in `compiler/`).

**Goal**: Generate compilable C code. Nothing more.

**Constraints**:
- Leak all memory (no memory management)
- Skip test blocks entirely
- Only support features actually used by the compiler
- Assume well-formed input (no error handling)
- Transpiler only runs on the self-hosted compiler (not a general-purpose tool)

---

## Current Status (Updated: Feb 2, 2026)

### Quick Start

```bash
cd /Users/anaumann/Development/blitz-lang/bootstrap/interpreter

# Build the transpiler
cargo build --release --bin interpreter

# Transpile Blitz to C
cargo run --release --bin interpreter -- --transpile-c ../../compiler/**/*.blitz

# Test compilation
gcc -std=c11 -ferror-limit=1000 -I c-out -c c-out/blitz.c 2>&1 | grep "error:" | wc -l
# Result: 19 errors

# Categorize errors
gcc -std=c11 -ferror-limit=1000 -I c-out -c c-out/blitz.c 2>&1 | grep "error:" | \
  sed 's/blitz.c:[0-9]*:[0-9]*:/blitz.c:XXX:/' | sort | uniq -c | sort -rn
```

### Compilation Results

| Target | Status | Errors |
|--------|--------|--------|
| Header (`blitz.h`) | **WORKING** | 0 |
| Implementation (`blitz.c`) | Failing | 19 |

### Error Summary (19 errors)

```
2 - returning 'List_Int' from function with incompatible result type 'List_Type'
2 - initializing 'Option_Ident_Tag' with expression of incompatible type 'Option_For'
1 - use of undeclared identifier 'none'
1 - unknown type name 'Option_void'
1 - statement requires expression of integer type ('Operator *' invalid)
1 - passing 'List_Definition' to parameter of incompatible type 'void *'
1 - initializing 'Statement **' with expression of incompatible type 'List_Int'
1 - initializing 'List_Type *' with expression of incompatible type 'List_Type'
1 - initializing 'List_CallArg *' with expression of incompatible type 'List_Int'
1 - initializing 'CallArg **' with expression of incompatible type 'Option_List_CallArg'
1 - initializing 'Arg **' with expression of incompatible type 'List_Int'
1 - incompatible pointer to integer conversion initializing 'Option_Ident_Tag' with 'Ident *'
1 - incompatible pointer to integer conversion assigning to 'Int' from 'Int *'
1 - assigning to 'Option_Type' from incompatible type 'Type *'
1 - assigning to 'Option_For' from incompatible type 'Option_Ident'
1 - assigning to 'List_Type' from incompatible type 'List_Int'
1 - assigning to 'int64_t' from incompatible type 'void'
```

---

## Remaining Issues (Prioritized)

### 1. Operator Switch Statement (1 error)

**Problem**: The `precedence(Operator* operator)` function has:
```c
switch (operator) {  // ERROR: can't switch on Operator* (pointer to tagged union)
    case TokenKind_else_:  // WRONG: should be Operator variant, not TokenKind
```

**Root Cause**:
- `Operator` is a tagged union (struct with tag + data), passed as pointer `Operator*`
- C `switch` requires an integer type, not a pointer to a struct
- Case labels are `TokenKind_` prefixed when they should use `BinaryOperator` or `Operator` variants

**Blitz Source** (`compiler/parser/precedence.blitz`):
```blitz
fn precedence(operator Operator) Precedence {
    switch operator {
        else_ { Precedence(left: 2, right: 2) }
        or_ { Precedence(left: 7, right: 8) }
        ...
    }
}
```

**Fix Needed**: 
- Detect when switch condition is a tagged union type
- Generate if-else chain checking `.tag` instead of C switch, OR
- Switch on `operator->tag` with proper `Operator_Tag_*` values
- Fix `qualify_identifier` to use correct enum prefix for Operator variants

### 2. Empty List Type Inference (6 errors)

**Problem**: Empty lists `[]` default to `List_Int` but context requires different types.

**Examples**:
- `Statement **` expects list of statements, gets `List_Int`
- `List_Type` expected, gets `List_Int`
- `Arg **` expected, gets `List_Int`

**Root Cause**: `preanalyze_empty_lists` doesn't have enough context to infer the correct element type in all cases.

**Fix Needed**: Improve context propagation for:
- Function parameters
- Return statements
- Variable declarations with explicit type annotations

### 3. Option Type Issues (5 errors)

**Problems**:
1. `Option_void` type name generated but doesn't exist
2. `none` identifier used but not qualified
3. `Option_For` vs `Option_Ident` type confusion
4. `Option_Ident_Tag` initialized with wrong types

**Specific Issue at line 1715**:
```c
Option_void label = none;  // Option_void doesn't exist, 'none' not qualified
```

**Root Cause**: 
- When Blitz has `Option(Ident)` but the variable is named `label`, the codegen looks for `Option_label` or similar
- The identifier `none` needs to be qualified as `Option_T_tag_none` for the specific type

**Fix Needed**:
- Track Option type instantiations properly
- Qualify `none`/`some` identifiers with correct type prefix
- Fix Option type detection in variable declarations

### 4. Pointer vs Value Confusion (3 errors)

**Problems**:
- `Int *` assigned to `Int` (line 62: `blitz_list_append` return)
- `Type *` assigned to `Option_Type` (missing Option wrapper)
- `List_Type` (value) assigned to `List_Type *` (pointer)

**Root Cause**: Inconsistent handling of when types should be values vs pointers.

### 5. print() Macro (1 error)

**Problem**: `print(List_Definition)` doesn't match `void*` parameter.

**Fix**: Add `List_Definition` and other List types to `_Generic` macro in `blitz_types.h`.

---

## Key Data Structures in c_codegen.rs

```rust
struct CCodegen {
    // Type tracking
    enum_variants: HashMap<String, HashSet<String>>,      // EnumName -> {variant1, ...}
    variant_to_union: HashMap<String, HashSet<String>>,   // VariantType -> {ParentUnion, ...}
    tagged_union_labels: HashMap<String, HashSet<String>>, // UnionName -> {label1, ...}
    enum_types: HashSet<String>,                          // Simple enums (no data)
    tagged_union_types: HashSet<String>,                  // Tagged unions (have data)
    struct_field_types: HashMap<String, HashMap<String, String>>,
    
    // Context tracking
    current_return_type: Option<Type>,
    variable_types: HashMap<String, String>,              // VarName -> CType
    variable_name_mappings: HashMap<String, String>,      // OriginalName -> MangledName
    function_signatures: HashMap<String, Vec<(Vec<String>, String)>>,
    function_return_types: HashMap<String, Type>,
    // ... more fields
}
```

## Blitz Type Mappings

| Blitz Type | C Type | Notes |
|------------|--------|-------|
| `Int` | `int64_t` | |
| `Bool` | `bool` | |
| `String` | `char*` | Leaked, no management |
| `List(T)` | `List_T` struct | `.data`, `.len`, `.cap` fields |
| `Option(T)` | `Option_T` struct | `.tag`, `.value` fields |
| `Box(T)` | `T*` | Simple pointer alias |
| Simple union | C enum | e.g., `BinaryOperator` |
| Tagged union | C struct | Tag enum + data union |
| Struct | C struct | Passed as pointers |

---

## What Works

### Type System (Complete)
- Struct transpilation with all field types
- Empty structs with dummy field
- Simple unions as C enums
- Tagged unions with enum + data union
- Generic type monomorphization (Box, List, Option, Lit)
- Forward declarations for recursive types
- Topological sort for type ordering
- Type collision resolution (counter-based naming)

### Functions (Complete)
- Function signatures with parameters
- Return types mapped correctly
- Function bodies generated
- Function overloading via name mangling
- UFCS method calls

### Expressions (Complete)
- All literals (Int, Bool, Float, String, Rune)
- Binary and unary operations
- Function calls (simple, nested)
- Member access with proper -> vs .
- Index operations with .data accessor
- Constructor calls with C99 compound literals
- Control flow (if, while)
- Switch expressions (C switch or if-else chains)
- Assignments

### Statements (Complete)
- Variable declarations
- Assignments
- Return statements
- Expression statements
- Break/continue

---

## What's Broken

### Critical Issues
1. **Tagged union switch** - Can't use C switch on `Operator*`
2. **Empty list inference** - Defaults to `List_Int` without context
3. **Option type handling** - `Option_void`, unqualified `none`

### Known Technical Debt
1. HashMap iteration order for enum variants (non-deterministic)
2. Hardcoded handling of keywords (`for_`, `if_`, `switch_`)
3. String heuristics for type detection
4. Inconsistent Option unwrapping

---

## Estimated Remaining Work

| Task | Effort | Complexity |
|------|--------|------------|
| Fix Operator switch (tagged union) | 2-3 hours | High |
| Fix empty list type inference | 2-3 hours | Medium |
| Fix Option type issues | 2-3 hours | Medium |
| Fix pointer/value confusion | 1-2 hours | Low |
| Fix print() macro | 0.5 hours | Low |

**Total to compiling C: 8-12 hours**

---

## File Structure

```
bootstrap/
  interpreter/
    src/
      c_codegen.rs         # Main C code generation (~7000+ lines)
      c_codegen_patch.rs   # Type name collision registry
      main.rs              # --transpile-c flag handling
      lib.rs               # Module exports
    c-out/                 # Generated C files
      blitz_types.h        # Runtime type definitions
      blitz.h              # Generated types + function declarations
      blitz.c              # Function implementations
```

---

## LLM Agent Instructions

### Progress Updates
- Be completely honest about what works and what doesn't
- Never claim something compiles if you haven't verified it
- Report errors immediately and accurately
- Mark tasks completed ONLY when verified working

### Testing
```bash
# Always run these after changes:
cargo build --release --bin interpreter
cargo run --release --bin interpreter -- --transpile-c ../../compiler/**/*.blitz
gcc -std=c11 -ferror-limit=1000 -I c-out -c c-out/blitz.c 2>&1 | grep "error:" | wc -l
```

### Key Files to Understand
1. `c_codegen.rs` - Main transpiler (~7000 lines)
2. `blitz_types.h` - Runtime type definitions
3. `compiler/ast/*.blitz` - Blitz AST type definitions
4. `compiler/parser/*.blitz` - Parser implementation

### Common Pitfalls
1. Don't assume C switch works on tagged unions
2. Empty lists need type context from surrounding code
3. Option types need qualified identifiers for `none`/`some`
4. HashMap iteration order is non-deterministic

---

## Commit History (Recent)

```
2c7ad33 Fix C codegen: List type inference, type collision for Option, enum disambiguation
68dfd55 Fix C codegen: enum qualification, pointer semantics, type inference
40f0f98 Fix C codegen: tagged union pointers, enum qualification, and Option unwrapping
61c9b13 Improve C codegen: type inference, union span helpers, and Option wrapping
1a0215a Fix C codegen: Add union span helpers, enforce pointer types for AST nodes
bb8ef7e Fix C codegen: Option type unwrapping, pointer semantics, and enum qualification
```

---

## Success Criteria

All files in `compiler/` transpile to valid C code that compiles:
```bash
cargo run --bin interpreter -- --transpile-c compiler/**/*.blitz
gcc -c -I c-out c-out/*.c
# 0 errors
```

That's it. Valid C code that compiles is success.
