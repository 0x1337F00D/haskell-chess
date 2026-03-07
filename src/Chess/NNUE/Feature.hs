{-# LANGUAGE BangPatterns #-}
module Chess.NNUE.Feature
  ( Perspective(..)
  , ActiveFeatures(..)
  , collectFeaturesHalfKP
  ) where

import Chess.Types
import Chess.Board.Base
import Chess.Bitboard (mapBitboard)
import Data.Bits (countTrailingZeros)

data Perspective = WhiteP | BlackP
  deriving (Eq, Show)

data ActiveFeatures = ActiveFeatures
  { whiteFeatures :: ![Int]
  , blackFeatures :: ![Int]
  }

boardPieces :: Board -> [(Color, PieceType, Square)]
boardPieces b =
  mapBitboard (\sq -> (White, Pawn, sq)) (whitePawns b) ++
  mapBitboard (\sq -> (White, Knight, sq)) (whiteKnights b) ++
  mapBitboard (\sq -> (White, Bishop, sq)) (whiteBishops b) ++
  mapBitboard (\sq -> (White, Rook, sq)) (whiteRooks b) ++
  mapBitboard (\sq -> (White, Queen, sq)) (whiteQueens b) ++
  mapBitboard (\sq -> (White, King, sq)) (whiteKings b) ++
  mapBitboard (\sq -> (Black, Pawn, sq)) (blackPawns b) ++
  mapBitboard (\sq -> (Black, Knight, sq)) (blackKnights b) ++
  mapBitboard (\sq -> (Black, Bishop, sq)) (blackBishops b) ++
  mapBitboard (\sq -> (Black, Rook, sq)) (blackRooks b) ++
  mapBitboard (\sq -> (Black, Queen, sq)) (blackQueens b) ++
  mapBitboard (\sq -> (Black, King, sq)) (blackKings b)

collectFeaturesHalfKP :: Board -> ActiveFeatures
collectFeaturesHalfKP b =
  ActiveFeatures
    { whiteFeatures = go WhiteP (kingSq White)
    , blackFeatures = go BlackP (kingSq Black)
    }
  where
    go persp !ksq =
      [ featureIndex persp ksq c pt sq
      | (c, pt, sq) <- boardPieces b
      ]

    kingSq !c = Square (countTrailingZeros (if c == White then whiteKings b else blackKings b))

featureIndex :: Perspective -> Square -> Color -> PieceType -> Square -> Int
featureIndex persp (Square ksq) c pt (Square sq) =
  perspBase + ksq * (12 * 64) + pieceCode c pt * 64 + sq
  where
    perspBase = case persp of
      WhiteP -> 0
      BlackP -> 64 * 12 * 64

pieceCode :: Color -> PieceType -> Int
pieceCode c pt = colorBase + ptCode pt
  where
    colorBase = case c of
      White -> 0
      Black -> 6
    ptCode Pawn   = 0
    ptCode Knight = 1
    ptCode Bishop = 2
    ptCode Rook   = 3
    ptCode Queen  = 4
    ptCode King   = 5
