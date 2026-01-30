{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DefaultSignatures #-}

module Chess.Core.Rules.Class where

import Chess.Core.Game.Internal
import Chess.Core.Move.Internal
import Chess.Core.Board.Internal (Color(..), KnownColor(..))

-- Type-level Opposite Color
type family Opposite (c :: Color) :: Color where
  Opposite 'White = 'Black
  Opposite 'Black = 'White

-- | Class for Chess Variants
class ChessVariant (v :: Variant) where
  generateMoves :: KnownColor c => ActiveGame v c s -> [Move c]
  applyMove :: (KnownColor c, KnownColor (Opposite c)) => Move c -> ActiveGame v c s -> GameTransition v (Opposite c)
  executeMove :: (KnownColor c, KnownColor (Opposite c)) => Move c -> ActiveGame v c s -> MoveResult v (Opposite c)

  -- | Perft (Performance Test) for this variant.
  -- Returns the number of leaf nodes at the given depth.
  -- Has a default implementation using executeMove, but can be optimized.
  perftVariant :: KnownColor c => Int -> ActiveGame v c s -> Int
  default perftVariant :: (KnownColor c, KnownColor (Opposite c)) => Int -> ActiveGame v c s -> Int
  perftVariant depth game
    | depth == 0 = 1
    | otherwise = sum $ map go (generateMoves game)
    where
      go m = case executeMove m game of
               Continue nextGame -> perftVariant (depth - 1) nextGame
               _ -> if depth == 1 then 1 else 0
