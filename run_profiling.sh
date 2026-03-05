#!/bin/bash
source /home/jules/.ghcup/env
cabal build --enable-profiling bench-core
./dist-newstyle/build/x86_64-linux/ghc-9.6.5/haskell-chess-0.1.0.0/x/bench-core/build/bench-core/bench-core +RTS -p -RTS
