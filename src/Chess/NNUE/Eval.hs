{-# LANGUAGE BangPatterns #-}
module Chess.NNUE.Eval
  ( evalAcc
  , clippedRelu16
  ) where

import Chess.NNUE.Types
import Data.Int
import Data.Primitive.ByteArray

{-# INLINE clippedRelu16 #-}
clippedRelu16 :: Int32 -> Int16
clippedRelu16 !x
  | x <= 0    = 0
  | x >= 127  = 127
  | otherwise = fromIntegral x

evalAcc :: Nnue -> Acc -> Int
evalAcc !nnue (Acc !accBA) = fromIntegral (out `quot` scale nnue)
  where
    !hidN = hiddenSize nnue
    !accN = accSize nnue

    !out = goHidden 0 (outBias nnue)

    goHidden :: Int -> Int32 -> Int32
    goHidden !i !accum
      | i == hidN  = accum
      | otherwise  =
          let !s0 = indexByteArray (h1Bias nnue) i :: Int32
              !s1 = dotAccRow accBA (h1Weights nnue) accN i s0
              !a  = fromIntegral (clippedRelu16 s1) :: Int32
              !w  = fromIntegral (indexByteArray (outWeights nnue) i :: Int16) :: Int32
          in goHidden (i + 1) (accum + a * w)

{-# INLINE dotAccRow #-}
dotAccRow :: ByteArray -> ByteArray -> Int -> Int -> Int32 -> Int32
dotAccRow !accBA !wBA !width !row !z0 = go 0 z0
  where
    !base = row * width
    go !j !z
      | j == width = z
      | otherwise  =
          let !a = fromIntegral (clippedRelu16 (indexByteArray accBA j :: Int32)) :: Int32
              !w = fromIntegral (indexByteArray wBA (base + j) :: Int16) :: Int32
          in go (j + 1) (z + a * w)
