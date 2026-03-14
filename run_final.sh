source ~/.ghcup/env
cabal clean
cabal build all --ghc-options="-Wall -Werror +RTS -M6G -RTS"
