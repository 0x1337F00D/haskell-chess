source ~/.ghcup/env
cabal clean
cabal test haskell-chess-test --ghc-options="-O0 +RTS -M4G -RTS"
