#!/bin/bash
# Build and run the C-compiled Blitz compiler
#
# Usage:
#   ./build-and-run.sh          # Build and run
#   ./build-and-run.sh --run    # Just run (skip build)
#   ./build-and-run.sh --build  # Just build (skip run)

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
COMPILER_DIR="$SCRIPT_DIR/../../compiler"
C_OUT_DIR="$SCRIPT_DIR/c-out"

# Parse arguments
BUILD=true
RUN=true

if [[ "$1" == "--run" ]]; then
    BUILD=false
elif [[ "$1" == "--build" ]]; then
    RUN=false
fi

if $BUILD; then
    echo "=== Building Rust transpiler ==="
    cd "$SCRIPT_DIR"
    cargo build --release --bin interpreter

    echo ""
    echo "=== Transpiling Blitz to C ==="
    # Use find to get all .blitz files to avoid glob expansion issues
    BLITZ_FILES=$(find "$COMPILER_DIR" -name "*.blitz" | sort | tr '\n' ' ')
    cargo run --release --bin interpreter -- --transpile-c $BLITZ_FILES

    echo ""
    echo "=== Compiling C to executable ==="
    gcc -std=c11 -I "$C_OUT_DIR" -o "$C_OUT_DIR/blitz" "$C_OUT_DIR/blitz.c"
    echo "Compiled successfully: $C_OUT_DIR/blitz"
fi

if $RUN; then
    if [[ ! -f "$C_OUT_DIR/blitz" ]]; then
        echo "Error: $C_OUT_DIR/blitz not found. Run with --build first."
        exit 1
    fi
    
    echo ""
    echo "=== Running C-compiled Blitz ==="
    cd "$COMPILER_DIR"
    "$C_OUT_DIR/blitz" main.blitz
    EXIT_CODE=$?
    echo ""
    echo "Exit code: $EXIT_CODE"
fi
