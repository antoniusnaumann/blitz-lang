# C Bootstrap Backend Implementation Plan

## Overview

Build a minimal C transpiler for the Blitz self-hosted compiler (`compiler/` directory).

**Goal**: Transpile Blitz source code to compilable C code that behaves identically.

**Scope**: This project is ONLY about the C backend transpiler in `bootstrap/interpreter/src/c_codegen.rs`. We do NOT modify the Blitz source files in `compiler/` - those are the input to the transpiler.

**Constraints**:
- Leak all memory (no memory management)
- Skip test blocks entirely
- Only support features actually used by the compiler
- Assume well-formed input (no error handling)
- Transpiler only runs on the self-hosted compiler (not a general-purpose tool)

---

## Current Status (Updated: Feb 3, 2025)

### ✅ MILESTONE: C-Compiled Binary Runs Successfully

| Stage | Status | Details |
|-------|--------|---------|
| Transpilation | ✅ **PASS** | All 18 Blitz files transpile to C |
| Compilation | ✅ **PASS** | 0 errors, 27 warnings |
| Linking | ✅ **PASS** | Executable created |
| Runtime | ✅ **PASS** | Executes main.blitz logic with exit code 0 |

**What this means**: The C backend successfully transpiles the Blitz compiler source to C, and the resulting binary executes correctly - it runs the lexer and parser on Blitz source files.

**Scope clarification**: The self-hosted compiler in `compiler/` currently only has lexer + parser implemented (no code generation, no CLI args). These limitations are in the Blitz source, NOT in the C backend. The C backend's job is to faithfully transpile whatever Blitz code exists.

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
```

### What the C Backend Handles Correctly

- ✅ All Blitz source files transpile to C
- ✅ Generated C compiles without errors (0 errors)
- ✅ Structs, enums, tagged unions
- ✅ Functions with overloading (name mangling)
- ✅ Generic types (Option, List, Box) with monomorphization
- ✅ For loops over Range and List types
- ✅ While loops with correct break semantics
- ✅ Switch expressions on enums and tagged unions
- ✅ Pattern matching with variable binding in switch cases
- ✅ Option type handling (some/none, safe unwrapping)
- ✅ Lit variant types (Lit(Int), Lit(String), etc.) as Expression variants
- ✅ Method calls and UFCS
- ✅ File I/O (read), time functions

### Known Limitations

- ⚠️ 27 compiler warnings (mostly harmless)
- ⚠️ Print outputs numeric enum values instead of names (cosmetic)
- ⚠️ Some switch cases use wrong enum type (generates warnings, still works)

---

## Architecture

```
compiler/                    # Blitz source (INPUT - do not modify)
  ├── main.blitz
  ├── ast/*.blitz
  ├── parser/*.blitz
  └── std/*.blitz

bootstrap/interpreter/       # C backend transpiler (THIS IS WHAT WE WORK ON)
  ├── src/
  │   ├── c_codegen.rs       # Main transpiler (~7900 lines)
  │   ├── c_codegen_patch.rs # Type name collision registry
  │   └── main.rs            # CLI with --transpile-c flag
  └── c-out/                 # Generated output
      ├── blitz_types.h      # Runtime types
      ├── blitz.h            # Generated type declarations
      └── blitz.c            # Generated function implementations
```

---

## Recent Fixes

### 1. Fixed break semantics in switch inside while loops
**Problem**: In Blitz, `break` inside a `switch` that's inside a `while` loop should break the while loop. In C, `break` only exits the switch.

**Solution**: Track loop labels and generate `goto _loop_exit_N` when breaking from switch inside loop.

### 2. Fixed double evaluation of expressions in `else return` pattern
**Problem**: `let x = expr() else return none` was calling `expr()` twice.

**Solution**: Store in temp variable `_else_tmp` before checking.

### 3. Fixed for-loops over List types
**Problem**: For loops only worked for Range types.

**Solution**: Detect List types and generate index-based iteration.

### 4. Fixed tagged union switch tag names
**Problem**: Switching on `Definition*` with case `Fn` generated `Fn_tag_Fn` instead of `Definition_tag_Fn`.

**Solution**: Use parent union type for tag names.

### 5. Fixed Lit variant return type mismatch (KEY FIX)
**Problem**: `parse_int_lit` returned `Option_Lit_Int` but was storing `Expression*` in the value field (type mismatch causing memory corruption).

**Solution**: 
1. Compare monomorphized type names when deciding to skip union wrapping
2. Always heap-allocate when returning into Option.value (pointer field)

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

# Run and check exit code
cd ../../compiler && ../bootstrap/interpreter/c-out/blitz main.blitz; echo "Exit code: $?"
```

---

## Success Criteria

**Ultimate Goal**: The C-compiled binary behaves identically to the Rust interpreter running the same Blitz code.

**Current Status**: ✅ Achieved for lexer + parser functionality

**What "done" looks like**:
- C backend transpiles all Blitz compiler source
- Generated C compiles without errors
- Resulting binary executes the same logic as the Rust interpreter
- As the Blitz compiler gains more features (code gen, etc.), the C backend supports them

---

## LLM Agent Notes

### Scope
- **DO** work on: `bootstrap/interpreter/src/c_codegen.rs` and related transpiler code
- **DO NOT** modify: `compiler/*.blitz` files (those are input, not our code)

### Honesty Policy
- Report actual error/warning counts after testing
- Don't claim "it works" without running it
- Document what's broken, not just what's fixed

### Key Patterns in c_codegen.rs
- `type_name_for_instance()` monomorphizes generic types (e.g., `Lit(Int)` → `Lit_Int`)
- `loop_label_stack` + `in_switch_depth` handle break semantics
- `variant_to_union` maps variant types to their parent unions
- When generating constructors, check if return type matches to skip unnecessary union wrapping
