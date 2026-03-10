import System.Process
main = callCommand "source ~/.ghcup/env && cabal run bench-search"
