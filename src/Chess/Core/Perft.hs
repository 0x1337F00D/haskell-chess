{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Chess.Core.Perft where

import Chess.Core.Board.Internal (KnownColor(..))
import Chess.Core.Game.Internal
import Chess.Core.Rules
import Chess.Types (Depth, unDepth)

-- | Perft function using Type-Safe architecture.
-- Counts leaf nodes at given depth.
-- Delegates to the variant-specific optimized implementation.
perft :: forall v c s. (ChessVariant v, KnownColor c, KnownColor (Opposite c))
      => Depth -> ActiveGame v c s -> Int
perft d ag = perftVariant (unDepth d) ag
