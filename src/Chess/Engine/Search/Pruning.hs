{-# LANGUAGE BangPatterns #-}
module Chess.Engine.Search.Pruning where

import qualified Data.Vector.Unboxed as U

-- | LMR Table
-- indexed by (depth * 64 + moveIndex)
-- Formula: 0.5 + log(depth) * log(index) / 2.0
lmrTable :: U.Vector Int
lmrTable = U.generate (64 * 64) gen
  where
    gen i =
        let d = i `div` 64
            idx = i `mod` 64
        in if d < 3 || idx < 2
           then 0
           else floor $ (0.5 :: Double) + log (fromIntegral d) * log (fromIntegral idx) / 2.0
