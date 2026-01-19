#!/bin/bash
set -e
BASELINE="${1:-before}"
cargo bench --bench core -- --baseline "$BASELINE"
