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

## Current Status (Updated: Feb 2, 2025)

### ✅ MILESTONE: C Code Compiles and Links

| Stage | Status | Details |
|-------|--------|---------|
| Compilation | ✅ **PASS** | 0 errors, 29 warnings |
| Linking | ✅ **PASS** | Executable created |
| Runtime | ⚠️ **INCOMPLETE** | Program runs but hangs (likely infinite loop or missing logic) |

### Quick Start

```bash
cd /Users/anaumann/Development/blitz-lang/bootstrap/interpreter

# Build the transpiler
cargo build --release --bin interpreter

# Transpile Blitz to C
cargo run --release --bin interpreter -- --transpile-c ../../compiler/**/*.blitz

# Compile to executable (0 errors)
gcc -std=c11 -I c-out -o c-out/blitz c-out/blitz.c

# Run (from compiler directory to find source files)
cd ../../compiler && ../bootstrap/interpreter/c-out/blitz
# Note: Currently hangs - likely infinite loop or runtime bug
```

### What Works

- ✅ All Blitz source files transpile to C
- ✅ Generated C compiles without errors
- ✅ Generated C links into executable
- ✅ Executable starts and runs initial code

### What's Broken

- ❌ Runtime hangs (infinite loop suspected)
- ⚠️ 29 compiler warnings (mostly harmless but some indicate real issues)
- ⚠️ Some type mismatches (Ident* vs Expression* in parse_args)

---

## Remaining Warnings Analysis (29 total)

### Likely Harmless (cosmetic)
- Extraneous parentheses in equality comparisons (3)
- Braces around scalar initializers (4)
- Expression result unused in error-handling code (7)

### May Indicate Bugs
- **Incompatible pointer types** (4): `Ident*` vs `Expression*` in parse_args - this is a type system issue where the generated code assigns wrong pointer types
- **Missing return in non-void function** (1): `parse_float` doesn't return a value on all paths
- **Switch case values not in enumerated type** (8): Assignment_1 switch uses TokenKind values

### Specific Issues

```
c-out/blitz.c:737: incompatible pointer types assigning to 'Ident *' from 'Expression *'
c-out/blitz.c:741: incompatible pointer types initializing 'Expression *' with 'Ident *'
c-out/blitz.c:754: incompatible pointer types initializing 'Ident *' with 'Expression *'
c-out/blitz.c:755: incompatible pointer types initializing 'Expression *' with 'Ident *'
c-out/blitz.c:1437: non-void function does not return a value
```

---

## Recent Fixes (This Session)

### 1. Fixed void-returning functions in if-else expressions
**Problem**: Functions like `skip_newlines(parser)` return void, but were used as the last expression in if-else bodies, causing `_if_result = skip_newlines(parser)` which is invalid C.

**Solution**:
- Added `void_functions: HashSet<String>` to track functions without return types
- Updated `infer_expr_type` to check `void_functions` and return "void"  
- When if-else result type is void, generate plain if-else statement instead of expression wrapper

### 2. Fixed print(List_Definition) type mismatch
**Problem**: The `print` macro mapped `List_Definition` to `print_unknown(void*)` but passed the struct by value.

**Solution**:
- Added forward declaration for `List_Definition` in `blitz_types.h`
- Added `print_list_definition(List_Definition)` function
- Updated `_Generic` macro to route `List_Definition` to proper function

### 3. Added runtime function implementations
Added missing functions in generated C:
- `panic(const char*)` - prints error and aborts
- `print_str/int/bool/float/list_rune/list_definition/unknown()` - all print variants
- `blitz_time()` - returns milliseconds since epoch
- `blitz_read(char* path)` - reads file into `Option_String`

---

## Key Files

```
bootstrap/interpreter/
  src/
    c_codegen.rs         # Main transpiler (~7500 lines)
    c_codegen_patch.rs   # Type name collision registry
    main.rs              # CLI and --transpile-c flag
  c-out/                 # Generated output
    blitz_types.h        # Runtime types (List_Rune, Option_String, etc.)
    blitz.h              # Generated type declarations
    blitz.c              # Generated function implementations
```

---

## Next Steps (Prioritized)

1. **Debug runtime hang** - Add debug prints or run in debugger to find where it loops
2. **Fix Ident*/Expression* type confusion** - In parse_args, wrong types are being assigned
3. **Fix parse_float missing return** - Add return statement for all code paths
4. **Clean up switch/enum warnings** - Use correct enum values

---

## Testing Commands

```bash
# Full test cycle
cd /Users/anaumann/Development/blitz-lang/bootstrap/interpreter
cargo build --release --bin interpreter
cargo run --release --bin interpreter -- --transpile-c ../../compiler/**/*.blitz
gcc -std=c11 -I c-out -o c-out/blitz c-out/blitz.c

# Count errors (should be 0)
gcc -std=c11 -I c-out -c c-out/blitz.c 2>&1 | grep -c "error:"

# List warnings by type
gcc -std=c11 -I c-out -c c-out/blitz.c 2>&1 | grep "warning:" | sed 's/blitz.c:[0-9]*:[0-9]*://' | sort | uniq -c | sort -rn
```

---

## Success Criteria

**Current Goal (ACHIEVED)**: Generate C code that compiles without errors.

**Next Goal**: Generate C code that runs correctly (parses Blitz source files).

---

## LLM Agent Notes

### Honesty Policy
- Report actual error/warning counts after testing
- Don't claim "it works" without running it
- Document what's broken, not just what's fixed

### Key Patterns in c_codegen.rs
- `void_functions` HashSet tracks functions returning void
- `infer_expr_type()` determines C type of expressions
- `generate_if_else_branch_body_with_type()` handles if-else branch code generation
- When result_type is "void", generate plain statement instead of expression wrapper
