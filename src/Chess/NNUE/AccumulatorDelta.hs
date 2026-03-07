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
      mapM_ (subRow mba) (removedW d)
      mapM_ (addRow mba) (addedW d)
      mapM_ (subRow mba) (removedB d)
      mapM_ (addRow mba) (addedB d)
  where
    !rowWidth = accSize nnue

    addRow !m !feat = loop 0
      where
        !base = feat * rowWidth
        loop !j
          | j == rowWidth = pure ()
          | otherwise = do
              let !w = fromIntegral (indexByteArray (ftWeights nnue) (base + j) :: Int16) :: Int32
              !old <- readByteArray m j :: IO Int32
              writeByteArray m j (old + w)
              loop (j + 1)

    subRow !m !feat = loop 0
      where
        !base = feat * rowWidth
        loop !j
          | j == rowWidth = pure ()
          | otherwise = do
              let !w = fromIntegral (indexByteArray (ftWeights nnue) (base + j) :: Int16) :: Int32
              !old <- readByteArray m j :: IO Int32
              writeByteArray m j (old - w)
              loop (j + 1)
