{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

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
