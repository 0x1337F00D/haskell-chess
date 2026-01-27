{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Chess.Core.Move
  ( Move -- Opaque
  , moveFrom
  , moveTo
  , toUCI
  , MoveResult(..) -- We can export MoveResult constructors as they are consumed by user
  ) where

import Chess.Core.Board
import Chess.Core.Board.Internal (squareToString)
import Chess.Core.Move.Internal

moveFrom :: Move c -> Square
moveFrom (QuietMove f _ _) = f
moveFrom (CaptureMove f _ _ _) = f
moveFrom (CastlingMove f _) = f
moveFrom (EnPassantMove f _) = f
moveFrom (PromotionMove f _ _) = f
moveFrom (PromotionCaptureMove f _ _ _) = f
moveFrom (DropMove _ _) = error "moveFrom: DropMove has no origin"
moveFrom (Castling960Move f _) = f

moveTo :: Move c -> Square
moveTo (QuietMove _ t _) = t
moveTo (CaptureMove _ t _ _) = t
moveTo (CastlingMove _ t) = t
moveTo (EnPassantMove _ t) = t
moveTo (PromotionMove _ t _) = t
moveTo (PromotionCaptureMove _ t _ _) = t
moveTo (DropMove _ t) = t
moveTo (Castling960Move _ r) = r -- Return Rook source as 'to' square for UCI convention in 960

toUCI :: Move c -> String
toUCI (QuietMove f t _) = squareToString f ++ squareToString t
toUCI (CaptureMove f t _ _) = squareToString f ++ squareToString t
toUCI (CastlingMove f t) = squareToString f ++ squareToString t -- e1g1
toUCI (EnPassantMove f t) = squareToString f ++ squareToString t
toUCI (PromotionMove f t p) = squareToString f ++ squareToString t ++ pieceTypeChar p
toUCI (PromotionCaptureMove f t p _) = squareToString f ++ squareToString t ++ pieceTypeChar p
toUCI (DropMove p t) = pieceTypeSymbol p ++ "@" ++ squareToString t
toUCI (Castling960Move f r) = squareToString f ++ squareToString r

pieceTypeSymbol :: PieceType -> String
pieceTypeSymbol Pawn   = "P"
pieceTypeSymbol Knight = "N"
pieceTypeSymbol Bishop = "B"
pieceTypeSymbol Rook   = "R"
pieceTypeSymbol Queen  = "Q"
pieceTypeSymbol King   = "K"

pieceTypeChar :: PieceType -> String
pieceTypeChar Queen  = "q"
pieceTypeChar Rook   = "r"
pieceTypeChar Bishop = "b"
pieceTypeChar Knight = "n"
pieceTypeChar _      = "" -- Should not happen for promotion
