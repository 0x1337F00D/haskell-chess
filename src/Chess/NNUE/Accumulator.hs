{-# LANGUAGE BangPatterns #-}
module Chess.NNUE.Accumulator
  ( refreshAcc
  ) where

import Chess.NNUE.Types
import Chess.NNUE.Feature
import Data.Int
import Data.Primitive.ByteArray

refreshAcc :: Nnue -> ActiveFeatures -> IO Acc
refreshAcc !nnue !afs = do
  mba <- newByteArray (accSize nnue * 4)

  -- initialize with bias
  let copyBias !i
        | i == accSize nnue = pure ()
        | otherwise = do
            let !b = fromIntegral (indexByteArray (ftBias nnue) i :: Int16) :: Int32
            writeByteArray mba i b
            copyBias (i + 1)
  copyBias 0

  addMany mba (whiteFeatures afs)
  addMany mba (blackFeatures afs)

  Acc <$> unsafeFreezeByteArray mba
  where
    !rowWidth = accSize nnue

    addMany !mba = goFeatures
      where
        goFeatures [] = pure ()
        goFeatures (f:fs) = addRow mba f >> goFeatures fs

    addRow !mba !feat = loop 0
      where
        !base = feat * rowWidth
        loop !j
          | j == rowWidth = pure ()
          | otherwise = do
              let !w = fromIntegral (indexByteArray (ftWeights nnue) (base + j) :: Int16) :: Int32
              !old <- readByteArray mba j :: IO Int32
              writeByteArray mba j (old + w)
              loop (j + 1)
