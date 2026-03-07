{-# LANGUAGE BangPatterns #-}
module Chess.NNUE.Types
  ( Nnue(..)
  , Acc(..)
  , accElems
  , hiddenElems
  ) where

import Data.Int
import Data.Primitive.ByteArray

data Nnue = Nnue
  { ftInputSize :: !Int          -- number of binary input features
  , accSize     :: !Int          -- transformer output width
  , hiddenSize  :: !Int          -- first hidden width
  , ftBias      :: !ByteArray    -- Int16[accSize]
  , ftWeights   :: !ByteArray    -- Int16[ftInputSize * accSize], row-major
  , h1Bias      :: !ByteArray    -- Int32[hiddenSize]
  , h1Weights   :: !ByteArray    -- Int16[hiddenSize * accSize], row-major
  , outBias     :: !Int32
  , outWeights  :: !ByteArray    -- Int16[hiddenSize]
  , scale       :: !Int32
  }

newtype Acc = Acc ByteArray

accElems :: Nnue -> Int
accElems = accSize

hiddenElems :: Nnue -> Int
hiddenElems = hiddenSize
