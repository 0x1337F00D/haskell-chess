{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Chess.Core.Move.Internal where

import Chess.Core.Board.Internal
import Chess.Core.Game.Internal

-- 5. Moves as Constructed Proofs

-- The Move type is exported abstractly (users can't construct it manually).
-- Internally, it utilizes a GADT.

data Move (c :: Color) where
  -- Standard Move: Normal move or capture
  StandardMove ::
    { smFrom :: Square
    , smTo   :: Square
    } -> Move c

  -- Castling Move: Kingside or Queenside
  -- We use the target file of the King (FileG or FileC) or the Rook's file?
  -- Typically Castling is encoded by King's movement.
  CastlingMove ::
    { cmFrom :: Square
    , cmTo   :: Square
    } -> Move c

  -- En Passant Move
  EnPassantMove ::
    { epFrom :: Square
    , epTo   :: Square
    } -> Move c

  -- Promotion Move
  PromotionMove ::
    { pmFrom :: Square
    , pmTo   :: Square
    , pmPromotion :: PieceType -- Promotion choice (Queen, Rook, Bishop, Knight)
    } -> Move c

  -- Drop Move
  DropMove ::
    { dmPiece :: PieceType
    , dmTo    :: Square
    } -> Move c

  -- Castling 960 Move
  Castling960Move ::
    { cm960From :: Square
    , cm960To   :: Square      -- Rook source (UCI target)
    , cm960KingDest :: Square  -- Actual King destination
    , cm960RookDest :: Square  -- Actual Rook destination
    } -> Move c

deriving instance Show (Move c)
deriving instance Eq (Move c)

-- 6. Check, Mate, and the Existential Step

-- The Next State result wrapper
-- Indexed by the color of the *next* turn (the side that just received the move).
data MoveResult (v :: Variant) (c :: Color) where
  -- Checkmate: The side 'c' is in checkmate. The game is won by 'Opposite c'.
  -- "Checkmate :: Winner c -> MoveResult c" in text might mean "Winner is defined relative to c"?
  -- Or strictly, if it's checkmate, the game ends.
  Checkmate :: Outcome -> MoveResult v c

  -- Stalemate: The side 'c' has no legal moves but is not in check. Draw.
  Stalemate :: MoveResult v c

  -- Continue: The game continues. The side 'c' is to move.
  -- Checks status is captured in the ActiveGame type.
  Continue  :: ActiveGame v c status -> MoveResult v c

deriving instance Show (VariantState v) => Show (MoveResult v c)
