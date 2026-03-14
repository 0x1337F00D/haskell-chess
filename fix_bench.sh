#!/bin/bash
sed -i 's/let Just _b = parseFen/let Just b = parseFen/g' scripts/BenchTactics.hs
sed -i 's/let Just _b = parseFen/let Just b = parseFen/g' scripts/BenchSearch.hs
sed -i 's/let Just _b = parseFen/let Just b = parseFen/g' bench/MicroBench.hs
