module Chess.NNUE.FeatureDelta
  ( AccDelta(..)
  ) where

data AccDelta = AccDelta
  { fullRefresh :: !Bool
  , removedW    :: ![Int]
  , addedW      :: ![Int]
  , removedB    :: ![Int]
  , addedB      :: ![Int]
  }
