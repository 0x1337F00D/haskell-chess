#!/bin/bash
source /home/jules/.ghcup/env
cabal build --enable-profiling --ghc-options="-fprof-auto -fprof-cafs" bench-report
EXE=$(find dist-newstyle -name bench-report -type f -executable)
echo "Running $EXE"
$EXE +RTS -p
