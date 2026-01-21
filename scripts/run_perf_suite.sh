#!/bin/bash
set -e

# Ensure pychess is available
./scripts/setup_pychess.sh

echo "Running PyChess Performance Test..."
python3 scripts/bench_pychess.py

echo ""
echo "Running Haskell Performance Test..."
cabal exec -- ghc -O2 -isrc scripts/BenchHaskell.hs -o scripts/bench_haskell > /dev/null 2>&1
scripts/bench_haskell
