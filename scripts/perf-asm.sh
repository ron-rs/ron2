#!/bin/bash
set -e
FUNC="${1:-next_token}"
cargo asm -p ron2 --lib "$FUNC" --release
