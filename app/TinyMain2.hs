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
  let Just (b, _) = parseFen "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
      afs = collectFeaturesHalfKP b
  acc <- refreshAcc nn afs
  print (evalAcc nn acc)

  let Just (b2, _) = parseFen "rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq e3 0 1"
      afs2 = collectFeaturesHalfKP b2
  acc2 <- refreshAcc nn afs2
  print (evalAcc nn acc2)
