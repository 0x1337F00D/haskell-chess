{-# LANGUAGE PatternSynonyms #-}
module Chess.Board.Base where

import Data.Bits ((.|.), (.&.), testBit, setBit, clearBit, xor, shiftL)

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
-- Optimized to check for occupancy first.
putPiece :: Board -> Square -> Piece -> Board
putPiece b sq piece =
  let i = unSquare sq
      -- Check if occupied (fast check)
      captured = if testBit (occupiedTotal b) i then pieceAt b sq else Nothing

      -- Remove captured piece if any
      b1 = case captured of
             Nothing -> b
             Just (Piece capC capPt) -> unsafeRemovePiece b sq capC capPt

  in unsafePutPiece b1 sq piece

-- | Place a piece on the board assuming the square is empty.
unsafePutPiece :: Board -> Square -> Piece -> Board
unsafePutPiece b sq (Piece c pt) =
    let i = unSquare sq
        -- Update specific bitboard
        b' = case (c, pt) of
               (White, Pawn)   -> b { whitePawns   = setBit (whitePawns b) i }
               (White, Knight) -> b { whiteKnights = setBit (whiteKnights b) i }
               (White, Bishop) -> b { whiteBishops = setBit (whiteBishops b) i }
               (White, Rook)   -> b { whiteRooks   = setBit (whiteRooks b) i }
               (White, Queen)  -> b { whiteQueens  = setBit (whiteQueens b) i }
               (White, King)   -> b { whiteKings   = setBit (whiteKings b) i }
               (Black, Pawn)   -> b { blackPawns   = setBit (blackPawns b) i }
               (Black, Knight) -> b { blackKnights = setBit (blackKnights b) i }
               (Black, Bishop) -> b { blackBishops = setBit (blackBishops b) i }
               (Black, Rook)   -> b { blackRooks   = setBit (blackRooks b) i }
               (Black, Queen)  -> b { blackQueens  = setBit (blackQueens b) i }
               (Black, King)   -> b { blackKings   = setBit (blackKings b) i }

        -- Update occupancy
        white = if c == White then setBit (occupiedWhite b) i else occupiedWhite b
        black = if c == Black then setBit (occupiedBlack b) i else occupiedBlack b
        total = setBit (occupiedTotal b) i
    in b' { occupiedWhite = white, occupiedBlack = black, occupiedTotal = total }

-- | Remove a piece from the board knowing its color and type.
-- Does not update other bitboards, so it is faster than removePieceAt.
unsafeRemovePiece :: Board -> Square -> Color -> PieceType -> Board
unsafeRemovePiece b sq c pt =
    let i = unSquare sq
        -- Update the specific bitboard
        b' = case (c, pt) of
               (White, Pawn)   -> b { whitePawns   = clearBit (whitePawns b) i }
               (White, Knight) -> b { whiteKnights = clearBit (whiteKnights b) i }
               (White, Bishop) -> b { whiteBishops = clearBit (whiteBishops b) i }
               (White, Rook)   -> b { whiteRooks   = clearBit (whiteRooks b) i }
               (White, Queen)  -> b { whiteQueens  = clearBit (whiteQueens b) i }
               (White, King)   -> b { whiteKings   = clearBit (whiteKings b) i }
               (Black, Pawn)   -> b { blackPawns   = clearBit (blackPawns b) i }
               (Black, Knight) -> b { blackKnights = clearBit (blackKnights b) i }
               (Black, Bishop) -> b { blackBishops = clearBit (blackBishops b) i }
               (Black, Rook)   -> b { blackRooks   = clearBit (blackRooks b) i }
               (Black, Queen)  -> b { blackQueens  = clearBit (blackQueens b) i }
               (Black, King)   -> b { blackKings   = clearBit (blackKings b) i }

        -- Update occupancy
        white = if c == White then clearBit (occupiedWhite b) i else occupiedWhite b
        black = if c == Black then clearBit (occupiedBlack b) i else occupiedBlack b
        total = clearBit (occupiedTotal b) i
    in b' { occupiedWhite = white, occupiedBlack = black, occupiedTotal = total }

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

-- | Move a piece from one square to another.
-- Handles capturing if the target square is occupied.
movePiece :: Board -> Square -> Square -> Color -> PieceType -> Board
movePiece b from to c pt =
    let fromI = unSquare from
        toI   = unSquare to

        -- Check capture
        captured = if testBit (occupiedTotal b) toI
                   then pieceAt b to
                   else Nothing

        -- Remove captured piece (if any)
        b1 = case captured of
               Nothing -> b
               Just (Piece capC capPt) -> unsafeRemovePiece b to capC capPt

        -- Move the piece using XOR swap
        -- We want to flip bits at 'from' and 'to'.
        -- In b1, 'from' is 1, 'to' is 0.
        mask = (1 `shiftL` fromI) `xor` (1 `shiftL` toI)

        b2 = case (c, pt) of
               (White, Pawn)   -> b1 { whitePawns   = whitePawns b1 `xor` mask }
               (White, Knight) -> b1 { whiteKnights = whiteKnights b1 `xor` mask }
               (White, Bishop) -> b1 { whiteBishops = whiteBishops b1 `xor` mask }
               (White, Rook)   -> b1 { whiteRooks   = whiteRooks b1 `xor` mask }
               (White, Queen)  -> b1 { whiteQueens  = whiteQueens b1 `xor` mask }
               (White, King)   -> b1 { whiteKings   = whiteKings b1 `xor` mask }
               (Black, Pawn)   -> b1 { blackPawns   = blackPawns b1 `xor` mask }
               (Black, Knight) -> b1 { blackKnights = blackKnights b1 `xor` mask }
               (Black, Bishop) -> b1 { blackBishops = blackBishops b1 `xor` mask }
               (Black, Rook)   -> b1 { blackRooks   = blackRooks b1 `xor` mask }
               (Black, Queen)  -> b1 { blackQueens  = blackQueens b1 `xor` mask }
               (Black, King)   -> b1 { blackKings   = blackKings b1 `xor` mask }

        -- Update occupancy
        -- For the moving piece color, we flip both bits.
        whiteOcc = if c == White then occupiedWhite b1 `xor` mask else occupiedWhite b1
        blackOcc = if c == Black then occupiedBlack b1 `xor` mask else occupiedBlack b1
        totalOcc = occupiedTotal b1 `xor` mask

    in b2 { occupiedWhite = whiteOcc, occupiedBlack = blackOcc, occupiedTotal = totalOcc }

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

-- | Find the piece type at a square, assuming it is occupied by a piece of the given color.
{-# INLINE findPieceType #-}
findPieceType :: Board -> Color -> Square -> PieceType
findPieceType b c sq =
  let i = unSquare sq
  in case c of
    White ->
      if testBit (whitePawns b) i then Pawn
      else if testBit (whiteKnights b) i then Knight
      else if testBit (whiteBishops b) i then Bishop
      else if testBit (whiteRooks b) i then Rook
      else if testBit (whiteQueens b) i then Queen
      else King
    Black ->
      if testBit (blackPawns b) i then Pawn
      else if testBit (blackKnights b) i then Knight
      else if testBit (blackBishops b) i then Bishop
      else if testBit (blackRooks b) i then Rook
      else if testBit (blackQueens b) i then Queen
      else King
