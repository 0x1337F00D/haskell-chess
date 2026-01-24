{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Chess.Core.Rules.Class where

import Chess.Core.Board.Internal (KnownColor, Color(..))
import Chess.Core.Game.Internal
import Chess.Core.Move.Internal

-- | Type-level Opposite Color
type family Opposite (c :: Color) :: Color where
  Opposite 'White = 'Black
  Opposite 'Black = 'White

-- | Class for Chess Variants
class ChessVariant (v :: Variant) where
  generateMoves :: KnownColor c => ActiveGame v c s -> [Move c]
  executeMove :: (KnownColor c, KnownColor (Opposite c)) => Move c -> ActiveGame v c s -> MoveResult v (Opposite c)

-- Generate Legal Moves
generateLegalMoves :: forall v c s. (KnownColor c, ChessVariant v) => ActiveGame v c s -> [Move c]
generateLegalMoves = generateMoves

-- Apply Move
applyMove :: forall v c s. (KnownColor c, KnownColor (Opposite c), ChessVariant v) => Move c -> ActiveGame v c s -> MoveResult v (Opposite c)
applyMove = executeMove
