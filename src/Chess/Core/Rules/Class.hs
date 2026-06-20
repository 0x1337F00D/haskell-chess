{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

module Chess.Core.Rules.Class
  ( module Chess.Core.Rules.Application
  , module Chess.Core.Rules.Color
  , module Chess.Core.Rules.Execution
  , module Chess.Core.Rules.Generation
  , module Chess.Core.Rules.Perft
  , ChessVariant
  ) where

import Chess.Core.Game.Internal (Variant)
import Chess.Core.Rules.Application
import Chess.Core.Rules.Color
import Chess.Core.Rules.Execution
import Chess.Core.Rules.Generation
import Chess.Core.Rules.Perft

-- | Compatibility umbrella for complete chess variants.
class VariantPerft v => ChessVariant (v :: Variant)
