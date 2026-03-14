#!/bin/bash
sed -i '/import System.Environment (getArgs)/d' scripts/BenchCore.hs
sed -i '/import System.Environment (getArgs)/d' scripts/BenchEvasions.hs
