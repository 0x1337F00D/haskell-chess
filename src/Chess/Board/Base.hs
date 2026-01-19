{-# LANGUAGE PatternSynonyms #-}
module Chess.Board.Base where

import Data.Bits ((.|.), (.&.), testBit, setBit, clearBit)

import Chess.Types
import Chess.Bitboard

-- | A board representation with bitboards for each piece type and color.
data Board = Board
  { whitePawns   :: !Bitboard
  , whiteKnights :: !Bitboard
  , whiteBishops :: !Bitboard
  , whiteRooks   :: !Bitboard
  , whiteQueens  :: !Bitboard
  , whiteKings   :: !Bitboard
  , blackPawns   :: !Bitboard
  , blackKnights :: !Bitboard
  , blackBishops :: !Bitboard
  , blackRooks   :: !Bitboard
  , blackQueens  :: !Bitboard
  , blackKings   :: !Bitboard
  , occupiedWhite :: !Bitboard
  , occupiedBlack :: !Bitboard
  , occupiedTotal :: !Bitboard
  } deriving (Eq, Show)

-- | An empty board.
empty :: Board
empty = Board 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0

-- | Get the bitboard for a specific piece type and color.
pieceBitboard :: Board -> Color -> PieceType -> Bitboard
pieceBitboard b White Pawn   = whitePawns b
pieceBitboard b White Knight = whiteKnights b
pieceBitboard b White Bishop = whiteBishops b
pieceBitboard b White Rook   = whiteRooks b
pieceBitboard b White Queen  = whiteQueens b
pieceBitboard b White King   = whiteKings b
pieceBitboard b Black Pawn   = blackPawns b
pieceBitboard b Black Knight = blackKnights b
pieceBitboard b Black Bishop = blackBishops b
pieceBitboard b Black Rook   = blackRooks b
pieceBitboard b Black Queen  = blackQueens b
pieceBitboard b Black King   = blackKings b

-- | Set the bitboard for a specific piece type and color.
setPieceBitboard :: Board -> Color -> PieceType -> Bitboard -> Board
setPieceBitboard b c pt bb = updateOccupancy $ case (c, pt) of
  (White, Pawn)   -> b { whitePawns   = bb }
  (White, Knight) -> b { whiteKnights = bb }
  (White, Bishop) -> b { whiteBishops = bb }
  (White, Rook)   -> b { whiteRooks   = bb }
  (White, Queen)  -> b { whiteQueens  = bb }
  (White, King)   -> b { whiteKings   = bb }
  (Black, Pawn)   -> b { blackPawns   = bb }
  (Black, Knight) -> b { blackKnights = bb }
  (Black, Bishop) -> b { blackBishops = bb }
  (Black, Rook)   -> b { blackRooks   = bb }
  (Black, Queen)  -> b { blackQueens  = bb }
  (Black, King)   -> b { blackKings   = bb }

-- | Update cached occupancy bitboards.
updateOccupancy :: Board -> Board
updateOccupancy b =
  let white = whitePawns b .|. whiteKnights b .|. whiteBishops b .|. whiteRooks b .|. whiteQueens b .|. whiteKings b
      black = blackPawns b .|. blackKnights b .|. blackBishops b .|. blackRooks b .|. blackQueens b .|. blackKings b
  in b { occupiedWhite = white, occupiedBlack = black, occupiedTotal = white .|. black }

-- | Get the piece at a square, if any.
pieceAt :: Board -> Square -> Maybe Piece
pieceAt b sq
  | not (testBit (occupiedTotal b) i) = Nothing
  | testBit (occupiedWhite b) i =
      if testBit (whitePawns b) i then Just (Piece White Pawn)
      else if testBit (whiteKnights b) i then Just (Piece White Knight)
      else if testBit (whiteBishops b) i then Just (Piece White Bishop)
      else if testBit (whiteRooks b) i then Just (Piece White Rook)
      else if testBit (whiteQueens b) i then Just (Piece White Queen)
      else Just (Piece White King)
  | otherwise =
      if testBit (blackPawns b) i then Just (Piece Black Pawn)
      else if testBit (blackKnights b) i then Just (Piece Black Knight)
      else if testBit (blackBishops b) i then Just (Piece Black Bishop)
      else if testBit (blackRooks b) i then Just (Piece Black Rook)
      else if testBit (blackQueens b) i then Just (Piece Black Queen)
      else Just (Piece Black King)
  where i = unSquare sq

-- | Get the color of the piece at a square, if any.
colorAt :: Board -> Square -> Maybe Color
colorAt b sq = fmap pieceColor (pieceAt b sq)

-- | Place a piece on the board. Overwrites any existing piece at that square.
putPiece :: Board -> Square -> Piece -> Board
putPiece b sq piece =
  let b' = removePieceAt b sq
      bb = pieceBitboard b' (pieceColor piece) (pieceType piece)
      bb' = setBit bb (unSquare sq)
  in setPieceBitboard b' (pieceColor piece) (pieceType piece) bb'

-- | Remove a piece from the board.
removePieceAt :: Board -> Square -> Board
removePieceAt b sq = b
  { whitePawns   = clearBit (whitePawns b) i
  , whiteKnights = clearBit (whiteKnights b) i
  , whiteBishops = clearBit (whiteBishops b) i
  , whiteRooks   = clearBit (whiteRooks b) i
  , whiteQueens  = clearBit (whiteQueens b) i
  , whiteKings   = clearBit (whiteKings b) i
  , blackPawns   = clearBit (blackPawns b) i
  , blackKnights = clearBit (blackKnights b) i
  , blackBishops = clearBit (blackBishops b) i
  , blackRooks   = clearBit (blackRooks b) i
  , blackQueens  = clearBit (blackQueens b) i
  , blackKings   = clearBit (blackKings b) i
  , occupiedWhite = clearBit (occupiedWhite b) i
  , occupiedBlack = clearBit (occupiedBlack b) i
  , occupiedTotal = clearBit (occupiedTotal b) i
  }
  where i = unSquare sq

-- | Bitboard of all pieces.
occupied :: Board -> Bitboard
occupied = occupiedTotal

-- | Bitboard of pieces by color.
occupiedBy :: Board -> Color -> Bitboard
occupiedBy b White = occupiedWhite b
occupiedBy b Black = occupiedBlack b

-- Attacks --------------------------------------------------------------------

-- | Attacks generated by the piece at the given square.
-- For sliding pieces, this accounts for blocking by other pieces on the board.
attacks :: Board -> Square -> Bitboard
attacks b sq = case pieceAt b sq of
  Nothing -> 0
  Just (Piece c pt) -> case pt of
    Pawn -> pawnAttacks c sq
    Knight -> knightAttacks sq
    King -> kingAttacks sq
    Bishop -> bishopAttacks sq (occupied b)
    Rook -> rookAttacks sq (occupied b)
    Queen -> bishopAttacks sq (occupied b) .|. rookAttacks sq (occupied b)

-- | Check if a square is attacked by any piece of the given color.
isAttackedBy :: Board -> Color -> Square -> Bool
isAttackedBy b color sq =
  (pawnAttacks (oppositeColor color) sq .&. pieceBitboard b color Pawn /= 0) ||
  (knightAttacks sq .&. pieceBitboard b color Knight /= 0) ||
  (kingAttacks sq .&. pieceBitboard b color King /= 0) ||
  (bishopAttacks sq (occupied b) .&. (pieceBitboard b color Bishop .|. pieceBitboard b color Queen) /= 0) ||
  (rookAttacks sq (occupied b) .&. (pieceBitboard b color Rook .|. pieceBitboard b color Queen) /= 0)

oppositeColor :: Color -> Color
oppositeColor White = Black
oppositeColor Black = White
