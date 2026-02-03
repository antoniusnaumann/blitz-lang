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

### ✅ MILESTONE: Lexer and Parser Partially Working

| Stage | Status | Details |
|-------|--------|---------|
| Compilation | ✅ **PASS** | 0 errors, 25 warnings |
| Linking | ✅ **PASS** | Executable created |
| Lexing | ✅ **PASS** | Successfully lexes 297 tokens from main.blitz |
| Parsing (first file) | ✅ **PASS** | Successfully parses main.blitz, prints AST |
| Parsing (second file) | ❌ **FAIL** | Panics with "Unexpected token!" on parser/parser.blitz |

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
cd ../../compiler && ../bootstrap/interpreter/c-out/blitz main.blitz
# Output: Lexes and parses first file, then panics on second file
```

### What Works

- ✅ All Blitz source files transpile to C
- ✅ Generated C compiles without errors (0 errors)
- ✅ Generated C links into executable
- ✅ Executable reads files successfully
- ✅ Lexer works correctly (297 tokens from main.blitz)
- ✅ Parser works for simple files (main.blitz parsed successfully)
- ✅ AST is printed correctly

### What's Broken

- ❌ Parsing complex files fails with "Unexpected token!" (expected token 60, got token 4)
- ⚠️ 25 compiler warnings (mostly harmless)

---

## Recent Fixes (This Session)

### 1. Fixed break semantics in switch inside while loops (CRITICAL)
**Problem**: In Blitz, `break` inside a `switch` that's inside a `while` loop should break the while loop. In C, `break` only exits the switch statement. This caused infinite loops in `skip_str`, `skip_ch`, etc.

**Solution**:
- Added `loop_label_stack: Vec<String>` to track current loop labels
- Added `in_switch_depth: usize` to track if we're inside a switch
- When generating `break` inside a switch inside a loop, generate `goto _loop_exit_N` instead
- Added loop exit labels after while/for loops: `_loop_exit_N:;`

### 2. Fixed Ident*/Expression* type confusion in parse_args
**Problem**: Variables were declared as `Ident*` but assigned `Expression*` values.

**Solution**: Updated type inference to correctly identify when variables should be `Expression*`.

### 3. Fixed missing return in infinite while loops
**Problem**: Functions ending with `while true { ... }` had no return statement after the loop, causing compiler warnings.

**Solution**: Added unreachable return statements after detected infinite while loops.

### 4. Fixed void-returning functions in if-else expressions
**Problem**: Functions like `skip_newlines(parser)` return void, but were used as the last expression in if-else bodies.

**Solution**: Track void functions and generate plain if-else statements instead of expression wrappers.

---

## Remaining Warnings Analysis (25 total)

### Likely Harmless (cosmetic)
- Extraneous parentheses in equality comparisons (2)
- Braces around scalar initializers (3)
- Expression result unused in error-handling code (6)

### May Indicate Bugs
- **Switch case values not in enumerated type** (8): Assignment_1 switch uses TokenKind values - this is a known issue where the generated code switches on an enum but uses values from a different enum

---

## Key Files

```
bootstrap/interpreter/
  src/
    c_codegen.rs         # Main transpiler (~7700 lines)
    c_codegen_patch.rs   # Type name collision registry
    main.rs              # CLI and --transpile-c flag
  c-out/                 # Generated output
    blitz_types.h        # Runtime types (List_Rune, Option_String, etc.)
    blitz.h              # Generated type declarations
    blitz.c              # Generated function implementations
```

---

## Next Steps (Prioritized)

1. **Debug parsing error** - Token kind 60 expected vs 4 received. Need to understand what tokens these are and why parsing fails.
2. **Clean up switch/enum warnings** - Use correct enum values in switch statements
3. **Test more complex Blitz source files** - Ensure the transpiler handles all language features

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

# Count warnings
gcc -std=c11 -I c-out -c c-out/blitz.c 2>&1 | grep -c "warning:"

# Run and see output
cd ../../compiler && ../bootstrap/interpreter/c-out/blitz main.blitz
```

---

## Success Criteria

**Current Goal (IN PROGRESS)**: Generate C code that runs correctly (parses all Blitz source files).

**Achieved**:
- ✅ C code compiles without errors
- ✅ Lexer works correctly
- ✅ Parser works for simple files

**Not Yet Achieved**:
- ❌ Parser works for all files

---

## LLM Agent Notes

### Honesty Policy
- Report actual error/warning counts after testing
- Don't claim "it works" without running it
- Document what's broken, not just what's fixed

### Key Patterns in c_codegen.rs
- `void_functions` HashSet tracks functions returning void
- `loop_label_stack` tracks current loop labels for break semantics
- `in_switch_depth` tracks if we're inside a switch for break semantics
- `infer_expr_type()` determines C type of expressions
- When generating `break` inside switch inside loop, use `goto` instead
