#!/bin/bash
sed -i 's/putStrLn $ "NPS: " ++ show nps/putStrLn $ "NPS: " ++ show (nps :: Double)/g' scripts/BenchCore.hs
sed -i 's/let Just board = parseFen/let Just _board = parseFen/g' scripts/BenchCore.hs
sed -i 's/let Just board = parseFen/let Just _board = parseFen/g' scripts/BenchEvasions.hs
sed -i '/import System.Environment/d' bench/BenchMagic.hs
sed -i '/import Control.Monad (replicateM_)/d' bench/BenchMagic.hs

sed -i 's/let Just b = parseFen/let Just _b = parseFen/g' scripts/BenchCore.hs
sed -i 's/let Just b = parseFen/let Just _b = parseFen/g' scripts/BenchEvasions.hs
