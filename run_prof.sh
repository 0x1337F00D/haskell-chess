#!/bin/bash
source /home/jules/.ghcup/env
cabal build --enable-profiling bench-core
# Find the executable
EXE=$(find dist-newstyle -name bench-core -type f -executable)
echo "Running $EXE"
$EXE +RTS -p
