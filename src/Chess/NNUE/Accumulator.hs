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
  -- The HalfKP accumulator contains 2 independent 256-wide arrays. We pack them sequentially into a size 512 array.
  let !halfSize = accSize nnue
  let !fullSize = halfSize * 2
  mba <- newByteArray (fullSize * 4)

  -- initialize with bias for both halves
  let copyBias !i
        | i == halfSize = pure ()
        | otherwise = do
            let !b = fromIntegral (indexByteArray (ftBias nnue) i :: Int16) :: Int32
            writeByteArray mba i b
            writeByteArray mba (halfSize + i) b
            copyBias (i + 1)
  copyBias 0

  addMany mba 0 (whiteFeatures afs)
  addMany mba halfSize (blackFeatures afs)

  Acc <$> unsafeFreezeByteArray mba
  where
    !rowWidth = accSize nnue

    addMany !mba !offset = goFeatures
      where
        goFeatures [] = pure ()
        goFeatures (f:fs) = addRow mba offset f >> goFeatures fs

    addRow !mba !offset !feat = loop 0
      where
        !base = feat * rowWidth
        loop !j
          | j == rowWidth = pure ()
          | otherwise = do
              let !w = fromIntegral (indexByteArray (ftWeights nnue) (base + j) :: Int16) :: Int32
              !old <- readByteArray mba (offset + j) :: IO Int32
              writeByteArray mba (offset + j) (old + w)
              loop (j + 1)
