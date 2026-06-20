{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE KindSignatures #-}

module Chess.Core.Rules.Generation where

import Chess.Core.Board.Internal (KnownColor)
import Chess.Core.Game.Internal (ActiveGame, Variant)
import Chess.Core.Move.Internal (Move)

-- | Move-generation capability for a chess variant.
class VariantMoveGen (v :: Variant) where
  generateMoves :: KnownColor c => ActiveGame v c s -> [Move c]

  -- | Optimized move counting. Defaults to length of generateMoves,
  -- but can be overridden by variants for O(1) or allocation-free counting.
  countMoves :: KnownColor c => ActiveGame v c s -> Int
  countMoves = length . generateMoves
