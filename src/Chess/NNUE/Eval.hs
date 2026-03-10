{-# LANGUAGE BangPatterns, MagicHash, UnliftedFFITypes, CPP #-}
module Chess.NNUE.Eval
  ( evalAcc
  , clippedRelu16
  ) where

import Chess.NNUE.Types
import Chess.Types (Color(..))
import Chess.Board.GameState (GameState, turn)
import Data.Int
import Data.Primitive.ByteArray
import System.IO.Unsafe (unsafePerformIO)

#ifdef USE_AVX512
import GHC.Exts (ByteArray#)

foreign import ccall unsafe "dotAccRowHalfKPC"
  c_dotAccRowHalfKPC :: ByteArray# -> ByteArray# -> Int -> Int -> Int32 -> Int -> Int -> Int32

foreign import ccall unsafe "dotH2RowC"
  c_dotH2RowC :: ByteArray# -> ByteArray# -> Int -> Int -> Int32 -> Int32
#endif

{-# INLINE clippedRelu16 #-}
clippedRelu16 :: Int32 -> Int16
clippedRelu16 !x
  | x <= 0    = 0
  | x >= 127  = 127
  | otherwise = fromIntegral x

-- Stockfish HalfKP feeds [White Accumulator, Black Accumulator] directly into the hidden layer.
-- We represent this in `evalAcc` by offsetting the weights read if the engine side-to-move is Black.
-- Wait, HalfKP feeds `[us, them]`!
evalAcc :: Nnue -> Acc -> GameState -> Int
evalAcc !nnue (Acc !accBA) !gs = fromIntegral (out `quot` scale nnue)
  where
    !hidN = hiddenSize nnue
    !accN = accSize nnue

    !isWhite = turn gs == White
    !wOffset = if isWhite then 0 else accN
    !bOffset = if isWhite then accN else 0

    -- First, calculate the H1 activations
    !h1Activations = calcH1 0

    calcH1 :: Int -> ByteArray
    calcH1 _ = unsafePerformIO $ do
      mba <- newByteArray (hidN * 4) -- store activations as Int32 internally or Int16? Wait, let's keep it simple. Actually, we need to pass these activations to the next layer.
      let go !i
            | i == hidN = pure ()
            | otherwise = do
                let !s0 = indexByteArray (h1Bias nnue) i :: Int32
                let !s1 = dotAccRowHalfKP accBA (h1Weights nnue) accN i s0 wOffset bOffset
                let !a = fromIntegral (clippedRelu16 s1) :: Int32
                writeByteArray mba i a
                go (i + 1)
      go 0
      unsafeFreezeByteArray mba

    !out = goOut 0 (outBias nnue)

    goOut :: Int -> Int32 -> Int32
    goOut !i !accum
      | i == hidN  = accum
      | otherwise  =
          let !s0 = indexByteArray (h2Bias nnue) i :: Int32
              !s1 = dotH2Row h1Activations (h2Weights nnue) hidN i s0
              !a  = fromIntegral (clippedRelu16 s1) :: Int32
              !w  = fromIntegral (indexByteArray (outWeights nnue) i :: Int16) :: Int32
          in goOut (i + 1) (accum + a * w)

{-# INLINE dotH2Row #-}
dotH2Row :: ByteArray -> ByteArray -> Int -> Int -> Int32 -> Int32
#ifdef USE_AVX512
dotH2Row (ByteArray !actBA) (ByteArray !wBA) !hidN !row !z0 =
    c_dotH2RowC actBA wBA hidN row z0
#else
dotH2Row !actBA !wBA !hidN !row !z0 = go 0 z0
  where
    !base = row * hidN
    go !j !z
      | j == hidN = z
      | otherwise =
          let !a = indexByteArray actBA j :: Int32
              !w = fromIntegral (indexByteArray wBA (base + j) :: Int16) :: Int32
          in go (j + 1) (z + a * w)
#endif

{-# INLINE dotAccRowHalfKP #-}
dotAccRowHalfKP :: ByteArray -> ByteArray -> Int -> Int -> Int32 -> Int -> Int -> Int32
#ifdef USE_AVX512
dotAccRowHalfKP (ByteArray !accBA) (ByteArray !wBA) !accN !row !z0 !usOffset !themOffset =
    c_dotAccRowHalfKPC accBA wBA accN row z0 usOffset themOffset
#else
dotAccRowHalfKP !accBA !wBA !accN !row !z0 !usOffset !themOffset = goThem 0 (goUs 0 z0)
  where
    -- Row width is accN * 2 in HalfKP!
    !baseUs = row * (accN * 2)
    !baseThem = baseUs + accN

    -- usOffset/themOffset is the starting offset inside the Acc array (0 for White, 256 for Black, assuming Acc is size 512).
    goUs !j !z
      | j == accN = z
      | otherwise =
          let !a = fromIntegral (clippedRelu16 (indexByteArray accBA (usOffset + j) :: Int32)) :: Int32
              !w = fromIntegral (indexByteArray wBA (baseUs + j) :: Int16) :: Int32
          in goUs (j + 1) (z + a * w)

    goThem !j !z
      | j == accN = z
      | otherwise =
          let !a = fromIntegral (clippedRelu16 (indexByteArray accBA (themOffset + j) :: Int32)) :: Int32
              !w = fromIntegral (indexByteArray wBA (baseThem + j) :: Int16) :: Int32
          in goThem (j + 1) (z + a * w)
#endif
