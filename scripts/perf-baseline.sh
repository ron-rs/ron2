#!/bin/bash
set -e
NAME="${1:-before}"
cargo bench --bench core -- --save-baseline "$NAME"
echo "Baseline '$NAME' saved"
