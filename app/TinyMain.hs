module Main where

import Chess.NNUE.Flat
import Chess.NNUE.Types
import Chess.NNUE.Eval
import Data.Int
import Data.Primitive.ByteArray

main :: IO ()
main = do
  nn <- loadNnueFlat "tiny.hsnn"
  mba <- newByteArray (accSize nn * 4)
  let go i
        | i == accSize nn = pure ()
        | otherwise = writeByteArray mba i (64 :: Int32) >> go (i + 1)
  go 0
  ba <- unsafeFreezeByteArray mba
  print (evalAcc nn (Acc ba))
