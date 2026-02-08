# AGENTS.md — Blitz Language

Blitz is a programming language built with a bootstrapping approach. A Rust-based bootstrap interpreter/transpiler (`bootstrap/`) runs and transpiles the self-hosted compiler written in Blitz (`compiler/`). The Rust code can also transpile Blitz to C for native execution.

## Project Structure

```
bootstrap/                  # Rust workspace (edition 2024)
  parser/                   # Parser crate — hand-written lexer + Pratt parser
    src/ast/*.blitz         # AST types defined in Blitz syntax (read by type-macro)
    src/tokens.blitz        # TokenKind union (also read by type-macro)
  interpreter/              # Interpreter + C codegen crate
    src/c_codegen.rs        # C transpiler (~8k lines, the largest file)
    src/instruction.rs      # Tree-walking interpreter
    src/registry.rs         # Type/function registry, overload resolution
    src/runtime.rs          # Built-in functions (print, read, time, etc.)
  type-macro/               # Proc-macro: reads .blitz files → generates Rust types
compiler/                   # Self-hosted compiler in Blitz
  main.blitz                # Entry point
  ast/                      # AST definitions
  parser/                   # Lexer, parser, tests
  std/                      # Standard library (string, list, range, result, bool)
  hir/                      # High-level IR definitions + lowering
```

**Key architecture detail:** AST types are defined in `.blitz` files (`parser/src/ast/`, `parser/src/tokens.blitz`) and the `type-macro` proc-macro reads these at compile time to generate Rust structs/enums. Changes to AST types must be made in the `.blitz` files, not in Rust code.

## Build / Run / Test Commands

All commands are run from `bootstrap/interpreter/` unless noted otherwise.
Prefer the C transpiler backend if possible, especially for running tests.

```bash
# Build the Rust workspace
cargo build                              # debug build
cargo build --release                    # release build (has debug symbols)

# Run the Blitz compiler via interpreter
cargo run -- run ../../compiler/
cargo run -- run -c ../../compiler/      # via C transpiler backend

# Run Blitz test suite (*.test.blitz files in compiler/)
cargo run -- test ../../compiler/        # via interpreter
cargo run -- test -c ../../compiler/     # via C backend (fast, ~6ms)

# Build to C without running
cargo run -- build -c ../../compiler/

# Run Rust unit tests (parser + codegen_patch tests)
cargo test                               # from bootstrap/ (workspace root)

# Run a single Rust test
cargo test test_name                     # e.g. cargo test test_add_assign
cargo test parser::tests::test_name      # fully qualified

# Enable debug output from C codegen
BLITZ_DEBUG=1 cargo run -- test -c ../../compiler/
```

## Code Style — Rust (bootstrap/)

### Formatting and Imports
- Standard `rustfmt` formatting
- Import order: `std` → external crates (`parser`) → `crate` imports
- Prefer nested `use` paths: `use std::{collections::HashMap, path::Path};`
- Modules declared in `lib.rs`, public API re-exported with `pub use module::*`

### Naming
- Standard Rust conventions: `snake_case` functions/variables, `CamelCase` types
- Use `r#type` for fields named after Rust keywords
- Prefix unused variables with `_`: `let _inferred_type = ...`

### Error Handling
- Public API uses `Result<(), String>` (the C codegen boundary)
- Use `?` operator for propagating errors within codegen functions
- `panic!()` and `.unwrap()` are acceptable in the interpreter (bootstrap code)
- `UserPanic` struct for Blitz-level panics (caught in test runner)

### Debug Output
- Use `debug_println!(self, ...)` macro in `c_codegen.rs` — gated behind `BLITZ_DEBUG` env var
- Keep `eprintln!("WARNING: ...")` and `eprintln!("ERROR: ...")` always-on
- Never add bare `eprintln!` for debug messages; always use the macro

### C Compilation
- GCC is invoked with `-w` flag (suppress warnings from generated code)
- On compilation failure, only `error:` lines are shown (warnings/notes filtered)
- Generated files go into `c-out/` (gitignored)

### Cargo Warnings
- Keep the build **warning-free** at all times
- Prefix unused variables with `_`, use `#[allow(dead_code)]` sparingly for future-use methods
- Remove unreachable patterns and duplicate match arms

## Code Style — Blitz (compiler/)
In the Blitz codebase, never use hacks or shortcuts. Try to implement the feature completely and cleanly. In case a feature is too large to implement it in one session, use the builtin todo("reason") function, where reason is one short sentence describing what needs to be done to implement the feature in full.

### File Organization
- One concept per file: `lexer.blitz`, `parser.blitz`, `tokens.blitz`
- Test files use `.test.blitz` suffix: `lexer.test.blitz`, `string.test.blitz`
- Directories mirror compiler phases: `ast/`, `parser/`, `std/`, `hir/`

### Formatting
- **Tab indentation** (not spaces)
- No semicolons — statements are newline-terminated
- No trailing commas required in struct/union definitions

### Naming
- Types: `CamelCase` — `Parser`, `TokenKind`, `Expression`
- Functions/variables: `snake_case` — `new_parser`, `skip_newlines`
- Union labels for reserved words get trailing underscore: `struct_`, `fn_`, `else_`
- Generic type params: single uppercase letter `T`, `E`, `U`, `V`

### Functions
- Free functions with explicit receiver (no methods): `fn parse(mut parser Parser)`
- Mutability explicitly marked on params: `mut parser Parser`
- Mutation syntax: `parser.mut.expect(ident)` (UFCS with `.mut.`)
- Overloading by parameter types is supported
- Constructor shorthand: `Parser(tokens:, errors: [])` — `field:` when var name matches

### Control Flow
- `switch` with implicit `it` binding: `switch token.kind { ident { it.name } }`
- `else` as binary operator: `parse_ident() else break`
- `try` / `.try` for Option unwrapping (returns `none` on failure)
- `for list |elem| { ... }` iteration with pipe-delimited binding
- `if`/`while` without parentheses around condition

### Types and Data
- `Option(T)` with `some`/`none`, `Result(T, E)` with `ok`/`err`
- `Bool` is `union { false, true }` (not a primitive)
- `++` concatenation, `++=` append-assign
- `List(T)` for dynamic arrays, `Box(T)` for heap allocation
- variable declarations use either `let` (immutable) or `mut` (mutable)

### Tests
- `test "descriptive name" { ... }` blocks at top level
- Use `assert expr` (keyword, not function call)
- Tests can read source files: `read("parser/parser.blitz").unwrap()`
- Test helpers are regular functions in the same or other files

## Important Notes

- **Do not manually edit generated files** in `c-out/` or Rust types generated by `type-macro`
- **AST type changes** go in `bootstrap/parser/src/ast/*.blitz` or `tokens.blitz`
- The `compiler/` directory is both the source for the self-hosted compiler AND the input to the bootstrap interpreter/transpiler
- The C codegen (`c_codegen.rs`) is the most complex file — test changes with `cargo run -- test -c ../../compiler/` to verify all 32 Blitz tests pass
- The interpreter and C backend should produce equivalent results
