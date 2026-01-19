{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Chess.Core.Rules where

import Chess.Core.Board
import Chess.Core.Game
import Chess.Core.Move

-- Type-level Opposite Color
type family Opposite (c :: Color) :: Color where
  Opposite 'White = 'Black
  Opposite 'Black = 'White

-- Generate Legal Moves
-- The compiler ensures that we can only generate moves for the side whose turn it is.
generateLegalMoves :: ActiveGame c s -> [Move c]
generateLegalMoves _ = [] -- Stub

-- Apply Move
-- Applies a move to the current game state, producing a result for the *next* player.
applyMove :: Move c -> ActiveGame c s -> MoveResult (Opposite c)
applyMove _ _ = Stalemate -- Stub
