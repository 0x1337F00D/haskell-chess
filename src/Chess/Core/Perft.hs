{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Chess.Core.Perft where

import Chess.Core.Board.Internal (KnownColor(..), sColor, SColor(..))
import Chess.Core.Game.Internal
import Chess.Core.Rules
import Chess.Core.Move
import Chess.Core.Move.Internal (GameTransition(..))
import Chess.Types (Depth, isZeroDepth, depthOne, decDepth)

-- | Perft function using Type-Safe architecture.
-- Counts leaf nodes at given depth.
perft :: forall v c s. (ChessVariant v, KnownColor c, KnownColor (Opposite c))
      => Depth -> ActiveGame v c s -> Int
perft d _ | isZeroDepth d = 1
perft d ag | d == depthOne = length (generateMoves ag)
perft d ag = sum $ map countMove (generateMoves ag)
  where
    countMove :: Move c -> Int
    countMove m =
      case applyMove m ag of
        Transition (nextAg :: ActiveGame v (Opposite c) nextStatus) ->
            perftRecursive (decDepth d) nextAg

perftRecursive :: forall v c s. (ChessVariant v, KnownColor c) => Depth -> ActiveGame v c s -> Int
perftRecursive d ag =
    case sColor @c of
      SWhite -> perft d ag
      SBlack -> perft d ag
