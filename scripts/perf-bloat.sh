#!/bin/bash
set -e
LINES="${1:-50}"
cargo llvm-lines -p ron2 --lib --release 2>/dev/null | head -n "$LINES"
