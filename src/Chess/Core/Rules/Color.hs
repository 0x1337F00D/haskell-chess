{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Chess.Core.Rules.Color where

import Chess.Core.Board.Internal (Color(..))

-- | Type-level opposite color.
type family Opposite (c :: Color) :: Color where
  Opposite 'White = 'Black
  Opposite 'Black = 'White
