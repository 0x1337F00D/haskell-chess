{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Chess.Core.Rules.Class where

import Chess.Core.Game.Internal
import Chess.Core.Move.Internal
import Chess.Core.Board.Internal (Color(..), KnownColor(..), sColor, SColor(..))
import Control.Parallel.Strategies (parMap, rseq)

-- Type-level Opposite Color
type family Opposite (c :: Color) :: Color where
  Opposite 'White = 'Black
  Opposite 'Black = 'White

-- | Class for Chess Variants
class ChessVariant (v :: Variant) where
  generateMoves :: KnownColor c => ActiveGame v c s -> [Move c]
  applyMove :: (KnownColor c, KnownColor (Opposite c)) => Move c -> ActiveGame v c s -> GameTransition v (Opposite c)
  executeMove :: (KnownColor c, KnownColor (Opposite c)) => Move c -> ActiveGame v c s -> MoveResult v (Opposite c)

  -- | Like executeMove, but optimized for perft tree traversal.
  -- Defaults to executeMove, but can be overridden to bypass redundant
  -- standard checkmate detection since perft naturally handles 0 legal moves.
  -- Must return Checkmate/Stalemate for variant-specific early terminations.
  perftExecuteMove :: (KnownColor c, KnownColor (Opposite c)) => Move c -> ActiveGame v c s -> MoveResult v (Opposite c)
  perftExecuteMove = executeMove

  -- | Perft (Performance Test) for this variant.
  -- Returns the number of leaf nodes at the given depth.
  -- Has a default implementation using perftExecuteMove, but can be optimized.
  perftVariant :: (KnownColor c, KnownColor (Opposite c)) => Int -> ActiveGame v c s -> Int
  default perftVariant :: forall c s. (KnownColor c, KnownColor (Opposite c)) => Int -> ActiveGame v c s -> Int
  perftVariant depth game = case sColor @c of
    SWhite -> perftWhite depth game
    SBlack -> perftBlack depth game

perftWhite :: ChessVariant v => Int -> ActiveGame v 'White s -> Int
perftWhite depth game
  | depth == 0 = 1
  | depth == 1 = length (generateMoves game)
  | depth >= 3 = sum $ parMap rseq go (generateMoves game)
  | otherwise = sum $ map go (generateMoves game)
  where
    go m = case perftExecuteMove m game of
             Continue nextGame -> perftBlack (depth - 1) nextGame
             _ -> 0

perftBlack :: ChessVariant v => Int -> ActiveGame v 'Black s -> Int
perftBlack depth game
  | depth == 0 = 1
  | depth == 1 = length (generateMoves game)
  | depth >= 3 = sum $ parMap rseq go (generateMoves game)
  | otherwise = sum $ map go (generateMoves game)
  where
    go m = case perftExecuteMove m game of
             Continue nextGame -> perftWhite (depth - 1) nextGame
             _ -> 0
