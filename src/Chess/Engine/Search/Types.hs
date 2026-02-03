{-# LANGUAGE BangPatterns #-}
module Chess.Engine.Search.Types where

import qualified Data.Vector.Unboxed.Mutable as UM
import Chess.Types (Move)

-- | Search Constants
infinity :: Int
infinity = 30000

mateValue :: Int
mateValue = 20000

-- | Helper to step score (negate and adjust for mate distance)
stepScore :: Int -> Int
stepScore s
    | s > 15000 = -s + 1
    | s < -15000 = -s - 1
    | otherwise = -s

-- | Node Type for Search
data NodeKind = Root | PV | NonPV
    deriving (Show, Eq)

-- | Check Status for Search
data CheckState = InCheck | NotInCheck
    deriving (Show, Eq)

-- | Search Context
-- Contains thread-local mutable data for move ordering and heuristics.
data SearchContext = SearchContext
    { ctxKillers :: !(UM.IOVector Move) -- 2 killers per ply * maxDepth
    , ctxHistory :: !(UM.IOVector Int)  -- 64*64 = 4096
    , ctxCounterMove :: !(UM.IOVector Move) -- 64*64 = 4096
    , ctxMaxDepth :: !Int
    }
