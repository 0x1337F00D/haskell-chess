{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Chess.Core.Rules.Class where

import Chess.Core.Game.Internal
import Chess.Core.Move.Internal
import Chess.Core.Board.Internal (Color(..), KnownColor(..))
import Chess.Types (Depth)

-- Type-level Opposite Color
type family Opposite (c :: Color) :: Color where
  Opposite 'White = 'Black
  Opposite 'Black = 'White

-- | Class for Chess Variants
class ChessVariant (v :: Variant) where
  generateMoves :: KnownColor c => ActiveGame v c s -> [Move c]
  applyMove :: (KnownColor c, KnownColor (Opposite c)) => Move c -> ActiveGame v c s -> GameTransition v (Opposite c)
  executeMove :: (KnownColor c, KnownColor (Opposite c)) => Move c -> ActiveGame v c s -> MoveResult v (Opposite c)

  -- | Perft function that counts leaf nodes at a given depth.
  -- Variants can override this for performance (e.g., bypassing ActiveGame).
  perftVariant :: (KnownColor c, KnownColor (Opposite c)) => Depth -> ActiveGame v c s -> Int
  perftVariant 0 _ = 1
  perftVariant 1 ag = length (generateMoves ag)
  perftVariant depth ag = sum $ map countMove (generateMoves ag)
    where
      countMove :: Move c -> Int
      countMove m =
        case applyMove m ag of
          Transition (nextAg :: ActiveGame v (Opposite c) nextStatus) ->
              perftVariant (depth - 1) nextAg
