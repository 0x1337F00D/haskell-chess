{-# LANGUAGE BangPatterns #-}
module Chess.NNUE.AccumulatorDelta
  ( thawAcc
  , freezeAcc
  , applyDelta
  ) where

import Chess.NNUE.Types
import Chess.NNUE.FeatureDelta
import Data.Int
import Data.Primitive.ByteArray
import Control.Monad.ST (RealWorld)

thawAcc :: Acc -> IO (MutableByteArray RealWorld)
thawAcc (Acc ba) = do
  mba <- newByteArray (sizeofByteArray ba)
  copyByteArray mba 0 ba 0 (sizeofByteArray ba)
  pure mba

freezeAcc :: MutableByteArray RealWorld -> IO Acc
freezeAcc mba = do
  ba <- unsafeFreezeByteArray mba
  pure (Acc ba)

applyDelta :: Nnue -> MutableByteArray RealWorld -> AccDelta -> IO ()
applyDelta !nnue !mba !d
  | fullRefresh d = error "use refreshAcc for full refresh"
  | otherwise = do
      let halfSize = accSize nnue
      mapM_ (subRow mba 0) (removedW d)
      mapM_ (addRow mba 0) (addedW d)
      mapM_ (subRow mba halfSize) (removedB d)
      mapM_ (addRow mba halfSize) (addedB d)
  where
    !rowWidth = accSize nnue

    addRow !m !offset !feat = loop 0
      where
        !base = feat * rowWidth
        loop !j
          | j == rowWidth = pure ()
          | otherwise = do
              let !w = fromIntegral (indexByteArray (ftWeights nnue) (base + j) :: Int16) :: Int32
              !old <- readByteArray m (offset + j) :: IO Int32
              writeByteArray m (offset + j) (old + w)
              loop (j + 1)

    subRow !m !offset !feat = loop 0
      where
        !base = feat * rowWidth
        loop !j
          | j == rowWidth = pure ()
          | otherwise = do
              let !w = fromIntegral (indexByteArray (ftWeights nnue) (base + j) :: Int16) :: Int32
              !old <- readByteArray m (offset + j) :: IO Int32
              writeByteArray m (offset + j) (old - w)
              loop (j + 1)
