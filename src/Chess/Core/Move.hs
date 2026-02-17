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

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Builder as B

import Chess.Core.Board
import Chess.Core.Board.Internal (squareToBuilder)
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

toUCI :: Move c -> BS.ByteString
toUCI m = BL.toStrict $ B.toLazyByteString $ case m of
  QuietMove f t _ -> squareToBuilder f <> squareToBuilder t
  CaptureMove f t _ _ -> squareToBuilder f <> squareToBuilder t
  CastlingMove f t -> squareToBuilder f <> squareToBuilder t -- e1g1
  EnPassantMove f t -> squareToBuilder f <> squareToBuilder t
  PromotionMove f t p -> squareToBuilder f <> squareToBuilder t <> pieceTypeCharBuilder p
  PromotionCaptureMove f t p _ -> squareToBuilder f <> squareToBuilder t <> pieceTypeCharBuilder p
  DropMove p t -> pieceTypeSymbolBuilder p <> B.char7 '@' <> squareToBuilder t
  Castling960Move f r -> squareToBuilder f <> squareToBuilder r

pieceTypeSymbolBuilder :: PieceType -> B.Builder
pieceTypeSymbolBuilder Pawn   = B.char7 'P'
pieceTypeSymbolBuilder Knight = B.char7 'N'
pieceTypeSymbolBuilder Bishop = B.char7 'B'
pieceTypeSymbolBuilder Rook   = B.char7 'R'
pieceTypeSymbolBuilder Queen  = B.char7 'Q'
pieceTypeSymbolBuilder King   = B.char7 'K'

pieceTypeCharBuilder :: PieceType -> B.Builder
pieceTypeCharBuilder Queen  = B.char7 'q'
pieceTypeCharBuilder Rook   = B.char7 'r'
pieceTypeCharBuilder Bishop = B.char7 'b'
pieceTypeCharBuilder Knight = B.char7 'n'
pieceTypeCharBuilder _      = mempty
