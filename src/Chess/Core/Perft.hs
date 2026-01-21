{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Chess.Core.Perft where

import Chess.Core.Board.Internal (KnownColor(..), opposite, Color(..), sColor, SColor(..))
import Chess.Core.Game.Internal
import Chess.Core.Rules
import Chess.Core.Move
import Data.Proxy (Proxy(..))

-- | Perft function using Type-Safe architecture.
-- Counts leaf nodes at given depth.
perft :: forall v c s. (ChessVariant v, KnownColor c, KnownColor (Opposite c))
      => Int -> ActiveGame v c s -> Int
perft 0 _ = 1
perft depth ag = sum $ map countMove (generateMoves ag)
  where
    countMove :: Move c -> Int
    countMove m =
      case executeMove m ag of
        Checkmate _ -> if depth == 1 then 1 else 0
        Stalemate -> if depth == 1 then 1 else 0
        Continue (nextAg :: ActiveGame v (Opposite c) nextStatus) ->
            perftRecursive (depth - 1) nextAg

perftRecursive :: forall v c s. (ChessVariant v, KnownColor c) => Int -> ActiveGame v c s -> Int
perftRecursive d ag =
    case sColor @c of
      SWhite -> perft d ag
      SBlack -> perft d ag
