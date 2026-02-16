{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}

module Chess.Core.Move.Internal where

import Chess.Core.Board.Internal
import Chess.Core.Game.Internal
import qualified Chess.Board.MoveGen as MG
import qualified Chess.Types as T
import Foreign.Storable (Storable)

-- 5. Moves as Constructed Proofs

-- The Move type is exported abstractly (users can't construct it manually).
-- Internally, it is now a newtype over GenMove (Word64) for zero-allocation efficiency.
-- We expose the GADT-like interface via Pattern Synonyms.

newtype Move (c :: Color) = Move MG.GenMove
  deriving newtype (Eq, Ord, Storable)

-- Pattern Synonyms to replicate the GADT interface

pattern QuietMove :: Square -> Square -> PieceType -> Move c
pattern QuietMove f t p <- (viewQuiet -> Just (f, t, p))
  where QuietMove f t p = Move (MG.GenQuiet (toTSquare f) (toTSquare t) (toTPieceType p))

pattern CaptureMove :: Square -> Square -> PieceType -> PieceType -> Move c
pattern CaptureMove f t p c <- (viewCapture -> Just (f, t, p, c))
  where CaptureMove f t p c = Move (MG.GenCapture (toTSquare f) (toTSquare t) (toTPieceType p) (toTPieceType c))

pattern CastlingMove :: Square -> Square -> Move c
pattern CastlingMove f t <- (viewCastling -> Just (f, t))
  where CastlingMove f t = Move (MG.GenCastling (toTSquare f) (toTSquare t))

pattern EnPassantMove :: Square -> Square -> Move c
pattern EnPassantMove f t <- (viewEnPassant -> Just (f, t))
  where EnPassantMove f t = Move (MG.GenEnPassant (toTSquare f) (toTSquare t))

pattern PromotionMove :: Square -> Square -> PieceType -> Move c
pattern PromotionMove f t p <- (viewPromotion -> Just (f, t, p))
  where PromotionMove f t p = Move (MG.GenPromotion (toTSquare f) (toTSquare t) (toTPieceType p))

pattern PromotionCaptureMove :: Square -> Square -> PieceType -> PieceType -> Move c
pattern PromotionCaptureMove f t p c <- (viewPromotionCapture -> Just (f, t, p, c))
  where PromotionCaptureMove f t p c = Move (MG.GenPromotionCapture (toTSquare f) (toTSquare t) (toTPieceType p) (toTPieceType c))

pattern DropMove :: PieceType -> Square -> Move c
pattern DropMove p t <- (viewDrop -> Just (p, t))
  where DropMove p t = Move (MG.GenDrop (toTPieceType p) (toTSquare t))

pattern Castling960Move :: Square -> Square -> Move c
pattern Castling960Move f r <- (viewCastling960 -> Just (f, r))
  where Castling960Move f r = Move (MG.GenCastling960 (toTSquare f) (toTSquare r))

{-# COMPLETE QuietMove, CaptureMove, CastlingMove, EnPassantMove, PromotionMove, PromotionCaptureMove, DropMove, Castling960Move #-}

-- View Helpers

viewQuiet :: Move c -> Maybe (Square, Square, PieceType)
viewQuiet (Move (MG.GenQuiet f t p)) = Just (fromTSquare f, fromTSquare t, fromTPieceType p)
viewQuiet _ = Nothing

viewCapture :: Move c -> Maybe (Square, Square, PieceType, PieceType)
viewCapture (Move (MG.GenCapture f t p c)) = Just (fromTSquare f, fromTSquare t, fromTPieceType p, fromTPieceType c)
viewCapture _ = Nothing

viewCastling :: Move c -> Maybe (Square, Square)
viewCastling (Move (MG.GenCastling f t)) = Just (fromTSquare f, fromTSquare t)
viewCastling _ = Nothing

viewEnPassant :: Move c -> Maybe (Square, Square)
viewEnPassant (Move (MG.GenEnPassant f t)) = Just (fromTSquare f, fromTSquare t)
viewEnPassant _ = Nothing

viewPromotion :: Move c -> Maybe (Square, Square, PieceType)
viewPromotion (Move (MG.GenPromotion f t p)) = Just (fromTSquare f, fromTSquare t, fromTPieceType p)
viewPromotion _ = Nothing

viewPromotionCapture :: Move c -> Maybe (Square, Square, PieceType, PieceType)
viewPromotionCapture (Move (MG.GenPromotionCapture f t p c)) = Just (fromTSquare f, fromTSquare t, fromTPieceType p, fromTPieceType c)
viewPromotionCapture _ = Nothing

viewDrop :: Move c -> Maybe (PieceType, Square)
viewDrop (Move (MG.GenDrop p t)) = Just (fromTPieceType p, fromTSquare t)
viewDrop _ = Nothing

viewCastling960 :: Move c -> Maybe (Square, Square)
viewCastling960 (Move (MG.GenCastling960 f r)) = Just (fromTSquare f, fromTSquare r)
viewCastling960 _ = Nothing

-- Converters

toTSquare :: Square -> T.Square
toTSquare (Square f r) = T.Square (fromEnum r * 8 + fromEnum f)

fromTSquare :: T.Square -> Square
fromTSquare (T.Square i) = Square (toEnum (i `mod` 8)) (toEnum (i `div` 8))

toTPieceType :: PieceType -> T.PieceType
toTPieceType King   = T.King
toTPieceType Queen  = T.Queen
toTPieceType Rook   = T.Rook
toTPieceType Bishop = T.Bishop
toTPieceType Knight = T.Knight
toTPieceType Pawn   = T.Pawn

fromTPieceType :: T.PieceType -> PieceType
fromTPieceType T.King   = King
fromTPieceType T.Queen  = Queen
fromTPieceType T.Rook   = Rook
fromTPieceType T.Bishop = Bishop
fromTPieceType T.Knight = Knight
fromTPieceType T.Pawn   = Pawn

-- Record Accessors (Backward Compatibility)
-- Note: These are now regular functions, not field accessors.

qmFrom :: Move c -> Square
qmFrom (QuietMove f _ _) = f
qmFrom _ = error "qmFrom: Not a QuietMove"

qmTo :: Move c -> Square
qmTo (QuietMove _ t _) = t
qmTo _ = error "qmTo: Not a QuietMove"

qmMoving :: Move c -> PieceType
qmMoving (QuietMove _ _ p) = p
qmMoving _ = error "qmMoving: Not a QuietMove"

cmFrom :: Move c -> Square
cmFrom (CaptureMove f _ _ _) = f
cmFrom _ = error "cmFrom: Not a CaptureMove"

cmTo :: Move c -> Square
cmTo (CaptureMove _ t _ _) = t
cmTo _ = error "cmTo: Not a CaptureMove"

cmMoving :: Move c -> PieceType
cmMoving (CaptureMove _ _ p _) = p
cmMoving _ = error "cmMoving: Not a CaptureMove"

cmCaptured :: Move c -> PieceType
cmCaptured (CaptureMove _ _ _ c) = c
cmCaptured _ = error "cmCaptured: Not a CaptureMove"

castlingFrom :: Move c -> Square
castlingFrom (CastlingMove f _) = f
castlingFrom _ = error "castlingFrom: Not a CastlingMove"

castlingTo :: Move c -> Square
castlingTo (CastlingMove _ t) = t
castlingTo _ = error "castlingTo: Not a CastlingMove"

epFrom :: Move c -> Square
epFrom (EnPassantMove f _) = f
epFrom _ = error "epFrom: Not a EnPassantMove"

epTo :: Move c -> Square
epTo (EnPassantMove _ t) = t
epTo _ = error "epTo: Not a EnPassantMove"

pmFrom :: Move c -> Square
pmFrom (PromotionMove f _ _) = f
pmFrom _ = error "pmFrom: Not a PromotionMove"

pmTo :: Move c -> Square
pmTo (PromotionMove _ t _) = t
pmTo _ = error "pmTo: Not a PromotionMove"

pmPromotion :: Move c -> PieceType
pmPromotion (PromotionMove _ _ p) = p
pmPromotion _ = error "pmPromotion: Not a PromotionMove"

pcmFrom :: Move c -> Square
pcmFrom (PromotionCaptureMove f _ _ _) = f
pcmFrom _ = error "pcmFrom: Not a PromotionCaptureMove"

pcmTo :: Move c -> Square
pcmTo (PromotionCaptureMove _ t _ _) = t
pcmTo _ = error "pcmTo: Not a PromotionCaptureMove"

pcmPromotion :: Move c -> PieceType
pcmPromotion (PromotionCaptureMove _ _ p _) = p
pcmPromotion _ = error "pcmPromotion: Not a PromotionCaptureMove"

pcmCaptured :: Move c -> PieceType
pcmCaptured (PromotionCaptureMove _ _ _ c) = c
pcmCaptured _ = error "pcmCaptured: Not a PromotionCaptureMove"

dmPiece :: Move c -> PieceType
dmPiece (DropMove p _) = p
dmPiece _ = error "dmPiece: Not a DropMove"

dmTo :: Move c -> Square
dmTo (DropMove _ t) = t
dmTo _ = error "dmTo: Not a DropMove"

cm960From :: Move c -> Square
cm960From (Castling960Move f _) = f
cm960From _ = error "cm960From: Not a Castling960Move"

cm960RookFrom :: Move c -> Square
cm960RookFrom (Castling960Move _ r) = r
cm960RookFrom _ = error "cm960RookFrom: Not a Castling960Move"

-- Show Instance to replicate derived Show
instance Show (Move c) where
  show (QuietMove f t p) = "QuietMove {qmFrom = " ++ show f ++ ", qmTo = " ++ show t ++ ", qmMoving = " ++ show p ++ "}"
  show (CaptureMove f t p c) = "CaptureMove {cmFrom = " ++ show f ++ ", cmTo = " ++ show t ++ ", cmMoving = " ++ show p ++ ", cmCaptured = " ++ show c ++ "}"
  show (CastlingMove f t) = "CastlingMove {castlingFrom = " ++ show f ++ ", castlingTo = " ++ show t ++ "}"
  show (EnPassantMove f t) = "EnPassantMove {epFrom = " ++ show f ++ ", epTo = " ++ show t ++ "}"
  show (PromotionMove f t p) = "PromotionMove {pmFrom = " ++ show f ++ ", pmTo = " ++ show t ++ ", pmPromotion = " ++ show p ++ "}"
  show (PromotionCaptureMove f t p c) = "PromotionCaptureMove {pcmFrom = " ++ show f ++ ", pcmTo = " ++ show t ++ ", pcmPromotion = " ++ show p ++ ", pcmCaptured = " ++ show c ++ "}"
  show (DropMove p t) = "DropMove {dmPiece = " ++ show p ++ ", dmTo = " ++ show t ++ "}"
  show (Castling960Move f r) = "Castling960Move {cm960From = " ++ show f ++ ", cm960RookFrom = " ++ show r ++ "}"

-- 6. Check, Mate, and the Existential Step

-- Transition (Active Game wrapper)
data GameTransition (v :: Variant) (c :: Color) where
  Transition :: ActiveGame v c s -> GameTransition v c

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
