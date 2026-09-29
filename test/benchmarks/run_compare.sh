#!/usr/bin/env sh
# Compares the micro-benchmark with encoding memoization disabled and enabled.
#
# Usage: ./run_compare.sh
set -eu

here=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
cd "$here/../.."

run() {
  echo "### SMTML_MAX_MEMO_ENTRIES=$1"
  SMTML_MAX_MEMO_ENTRIES="$1" dune exec --profile benchmark \
    test/benchmarks/bench_micro.exe
  echo
}

run 0
run 1000000
