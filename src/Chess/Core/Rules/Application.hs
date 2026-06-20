{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}

module Chess.Core.Rules.Application where

import Chess.Core.Board.Internal (KnownColor)
import Chess.Core.Game.Internal (ActiveGame, Variant)
import Chess.Core.Move.Internal (GameTransition, Move)
import Chess.Core.Rules.Color (Opposite)

-- | Board and game-state transition capability for a chess variant.
class VariantMoveApply (v :: Variant) where
  applyMove :: (KnownColor c, KnownColor (Opposite c)) => Move c -> ActiveGame v c s -> GameTransition v (Opposite c)
