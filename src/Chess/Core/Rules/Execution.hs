{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}

module Chess.Core.Rules.Execution where

import Chess.Core.Board.Internal (KnownColor)
import Chess.Core.Game.Internal (ActiveGame, Variant)
import Chess.Core.Move.Internal (Move, MoveResult)
import Chess.Core.Rules.Application (VariantMoveApply)
import Chess.Core.Rules.Color (Opposite)

-- | Rule-result capability for a chess variant.
class VariantMoveApply v => VariantMoveExecute (v :: Variant) where
  executeMove :: (KnownColor c, KnownColor (Opposite c)) => Move c -> ActiveGame v c s -> MoveResult v (Opposite c)

  -- | Like executeMove, but optimized for perft tree traversal.
  -- Defaults to executeMove, but can be overridden to bypass redundant
  -- standard checkmate detection since perft naturally handles 0 legal moves.
  -- Must return Checkmate/Stalemate for variant-specific early terminations.
  perftExecuteMove :: (KnownColor c, KnownColor (Opposite c)) => Move c -> ActiveGame v c s -> MoveResult v (Opposite c)
  perftExecuteMove = executeMove
