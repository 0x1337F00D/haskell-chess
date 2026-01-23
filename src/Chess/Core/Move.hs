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
moveFrom (StandardMove f _) = f
moveFrom (CastlingMove f _) = f
moveFrom (Castling960Move f _) = f
moveFrom (EnPassantMove f _) = f
moveFrom (PromotionMove f _ _) = f
moveFrom (DropMove _ _) = error "moveFrom: DropMove has no origin"

moveTo :: Move c -> Square
moveTo (StandardMove _ t) = t
moveTo (CastlingMove _ t) = t
moveTo (Castling960Move _ t) = t
moveTo (EnPassantMove _ t) = t
moveTo (PromotionMove _ t _) = t
moveTo (DropMove _ t) = t

toUCI :: Move c -> String
toUCI (StandardMove f t) = squareToString f ++ squareToString t
toUCI (CastlingMove f t) = squareToString f ++ squareToString t -- e1g1
toUCI (Castling960Move f t) = squareToString f ++ squareToString t
toUCI (EnPassantMove f t) = squareToString f ++ squareToString t
toUCI (PromotionMove f t p) = squareToString f ++ squareToString t ++ pieceTypeChar p
toUCI (DropMove p t) = pieceTypeSymbol p ++ "@" ++ squareToString t

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
