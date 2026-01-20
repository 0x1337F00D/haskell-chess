{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Chess.Core.Move
  ( Move -- Opaque
  , moveFrom
  , moveTo
  , MoveResult(..) -- We can export MoveResult constructors as they are consumed by user
  ) where

import Chess.Core.Board
import Chess.Core.Move.Internal

moveFrom :: Move c -> Square
moveFrom (StandardMove f _) = f
moveFrom (CastlingMove f _) = f
moveFrom (EnPassantMove f _) = f
moveFrom (PromotionMove f _ _) = f

moveTo :: Move c -> Square
moveTo (StandardMove _ t) = t
moveTo (CastlingMove _ t) = t
moveTo (EnPassantMove _ t) = t
moveTo (PromotionMove _ t _) = t
