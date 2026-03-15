module Main where

import Chess.Board.Base
import Chess.NNUE.Flat
import Chess.NNUE.Types
import Chess.NNUE.Feature
import Chess.NNUE.Accumulator
import Chess.NNUE.Eval
import Data.Int
import Data.Primitive.ByteArray
import Chess.Board.Fen

main :: IO ()
main = do
  nn <- loadNnueFlat "tiny.hsnn"
  let (_b, _gs) = case parseFen "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1" of
                     Just x -> x
                     Nothing -> error "Invalid FEN"
      afs = collectFeaturesHalfKP _b
  acc <- refreshAcc nn afs
  print (evalAcc nn acc _gs)

  let (_b2, _gs2) = case parseFen "rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq e3 0 1" of
                       Just x -> x
                       Nothing -> error "Invalid FEN"
      afs2 = collectFeaturesHalfKP _b2
  acc2 <- refreshAcc nn afs2
  print (evalAcc nn acc2 _gs2)
