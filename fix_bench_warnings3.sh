#!/bin/bash
sed -i 's/let Just board = parseFen/let Just _board = parseFen/g' scripts/BenchTactics.hs
sed -i 's/let Just b = parseFen/let Just _b = parseFen/g' scripts/BenchTactics.hs
sed -i 's/let Just (b, gs) = parseFen/let Just (_b, _gs) = parseFen/g' app/TinyMain2.hs
sed -i 's/let Just (b2, gs2) = parseFen/let Just (_b2, _gs2) = parseFen/g' app/TinyMain2.hs
sed -i 's/let Just board = parseFen/let Just _board = parseFen/g' bench/MicroBench.hs
sed -i 's/let Just b = parseFen/let Just _b = parseFen/g' bench/MicroBench.hs
