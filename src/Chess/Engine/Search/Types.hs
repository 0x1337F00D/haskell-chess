{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
module Chess.Engine.Search.Types where

import qualified Data.Vector.Unboxed.Mutable as UM
import Chess.Types (Move, CheckStatus(..))
import Chess.Board.Phase (Phase)

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

-- | Search Phase
data SearchPhase = MainSearch | Quiescence
    deriving (Show, Eq)

-- | Null Move State
data NullMoveState = NullMoveAllowed | NullMoveSkipped
    deriving (Show, Eq)

-- | Search Limits
data SearchLimits = SearchLimits
    { limitTime     :: !(Maybe Int) -- ^ Time limit in milliseconds
    , limitNodes    :: !(Maybe Int) -- ^ Node limit
    , limitDepth    :: !(Maybe Int) -- ^ Depth limit
    , limitMate     :: !(Maybe Int) -- ^ Search for mate in N moves
    , limitInfinite :: !Bool        -- ^ Infinite search
    } deriving (Show, Eq)

defaultLimits :: SearchLimits
defaultLimits = SearchLimits Nothing Nothing Nothing Nothing False

-- | Search Resources
-- Contains thread-local mutable data for move ordering and heuristics.
data SearchResources = SearchResources
    { resKillers :: !(UM.IOVector Move) -- 2 killers per ply * maxDepth
    , resHistory :: !(UM.IOVector Int)  -- 64*64 = 4096
    , resCounterMove :: !(UM.IOVector Move) -- 64*64 = 4096
    , resMaxDepth :: !Int
    }

-- | Search Context
-- Immutable state passed down the search tree.
data SearchContext (p :: Phase) = SearchContext
    { scResources     :: !SearchResources
    , scNodeKind      :: !NodeKind
    , scCheckState    :: !CheckStatus
    , scPhase         :: !SearchPhase
    , scPly           :: !Int
    , scNullMoveState :: !NullMoveState
    , scAge           :: !Int
    }
