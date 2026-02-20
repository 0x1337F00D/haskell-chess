{-# LANGUAGE PatternSynonyms #-}
module Chess.Board.Base where

import Data.Bits ((.|.), (.&.), testBit, setBit, clearBit, xor, shiftL, countTrailingZeros, countLeadingZeros, popCount)

import qualified Data.Vector.Unboxed as U
import Chess.Types
import Chess.Bitboard
import Chess.Data.Evaluation

-- | A board representation with bitboards for each piece type and color.
data Board = Board
  { whitePawns   :: {-# UNPACK #-} !Bitboard
  , whiteKnights :: {-# UNPACK #-} !Bitboard
  , whiteBishops :: {-# UNPACK #-} !Bitboard
  , whiteRooks   :: {-# UNPACK #-} !Bitboard
  , whiteQueens  :: {-# UNPACK #-} !Bitboard
  , whiteKings   :: {-# UNPACK #-} !Bitboard
  , blackPawns   :: {-# UNPACK #-} !Bitboard
  , blackKnights :: {-# UNPACK #-} !Bitboard
  , blackBishops :: {-# UNPACK #-} !Bitboard
  , blackRooks   :: {-# UNPACK #-} !Bitboard
  , blackQueens  :: {-# UNPACK #-} !Bitboard
  , blackKings   :: {-# UNPACK #-} !Bitboard
  , occupiedWhite :: {-# UNPACK #-} !Bitboard
  , occupiedBlack :: {-# UNPACK #-} !Bitboard
  , occupiedTotal :: {-# UNPACK #-} !Bitboard
  -- Aggregated Bitboards (Segmented)
  , whiteDiagonal   :: {-# UNPACK #-} !Bitboard -- Bishops | Queens
  , whiteOrthogonal :: {-# UNPACK #-} !Bitboard -- Rooks | Queens
  , blackDiagonal   :: {-# UNPACK #-} !Bitboard -- Bishops | Queens
  , blackOrthogonal :: {-# UNPACK #-} !Bitboard -- Rooks | Queens
  -- Cached Evaluation Scores
  , scoreWhite      :: {-# UNPACK #-} !PackedScore
  , scoreBlack      :: {-# UNPACK #-} !PackedScore
  , gamePhase       :: {-# UNPACK #-} !Int
  } deriving (Eq, Show)

-- | An empty board.
empty :: Board
empty = Board 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0

-- | Compute scores from scratch.
computeScores :: Board -> Board
computeScores b =
    let
        -- Helper
        eval :: Bitboard -> PackedScore -> U.Vector PackedScore -> PackedScore
        eval bb mat table = (popCount bb * mat) + evalPacked bb table

        -- White Scores
        sw = eval (whitePawns b)   (packedMaterialValue Pawn)   packedPawnTable
           + eval (whiteKnights b) (packedMaterialValue Knight) packedKnightTable
           + eval (whiteBishops b) (packedMaterialValue Bishop) packedBishopTable
           + eval (whiteRooks b)   (packedMaterialValue Rook)   packedRookTable
           + eval (whiteQueens b)  (packedMaterialValue Queen)  packedQueenTable
           + eval (whiteKings b)   (packedMaterialValue King)   packedKingTable

        -- Black Scores
        sb = eval (blackPawns b)   (packedMaterialValue Pawn)   packedPawnTableFlip
           + eval (blackKnights b) (packedMaterialValue Knight) packedKnightTableFlip
           + eval (blackBishops b) (packedMaterialValue Bishop) packedBishopTableFlip
           + eval (blackRooks b)   (packedMaterialValue Rook)   packedRookTableFlip
           + eval (blackQueens b)  (packedMaterialValue Queen)  packedQueenTableFlip
           + eval (blackKings b)   (packedMaterialValue King)   packedKingTableFlip

        -- Game Phase
        phase = (popCount (whiteKnights b) + popCount (blackKnights b)) * phaseValue Knight
              + (popCount (whiteBishops b) + popCount (blackBishops b)) * phaseValue Bishop
              + (popCount (whiteRooks b)   + popCount (blackRooks b))   * phaseValue Rook
              + (popCount (whiteQueens b)  + popCount (blackQueens b))  * phaseValue Queen

    in b { scoreWhite = sw, scoreBlack = sb, gamePhase = phase }

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

-- | Update cached occupancy and aggregated bitboards.
updateOccupancy :: Board -> Board
updateOccupancy b =
  let wB = whiteBishops b
      wR = whiteRooks b
      wQ = whiteQueens b
      bB = blackBishops b
      bR = blackRooks b
      bQ = blackQueens b

      wDiag = wB .|. wQ
      wOrth = wR .|. wQ
      bDiag = bB .|. bQ
      bOrth = bR .|. bQ

      white = whitePawns b .|. whiteKnights b .|. wB .|. wR .|. wQ .|. whiteKings b
      black = blackPawns b .|. blackKnights b .|. bB .|. bR .|. bQ .|. blackKings b
  in b { occupiedWhite = white
       , occupiedBlack = black
       , occupiedTotal = white .|. black
       , whiteDiagonal = wDiag
       , whiteOrthogonal = wOrth
       , blackDiagonal = bDiag
       , blackOrthogonal = bOrth
       }

-- | Get the piece at a square, if any.
{-# INLINE pieceAt #-}
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
{-# INLINE colorAt #-}
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
        -- Update specific bitboard and aggregates
        b' = case (c, pt) of
               (White, Pawn)   -> b { whitePawns   = setBit (whitePawns b) i }
               (White, Knight) -> b { whiteKnights = setBit (whiteKnights b) i }
               (White, Bishop) -> b { whiteBishops = setBit (whiteBishops b) i
                                    , whiteDiagonal = setBit (whiteDiagonal b) i }
               (White, Rook)   -> b { whiteRooks   = setBit (whiteRooks b) i
                                    , whiteOrthogonal = setBit (whiteOrthogonal b) i }
               (White, Queen)  -> b { whiteQueens  = setBit (whiteQueens b) i
                                    , whiteDiagonal = setBit (whiteDiagonal b) i
                                    , whiteOrthogonal = setBit (whiteOrthogonal b) i }
               (White, King)   -> b { whiteKings   = setBit (whiteKings b) i }

               (Black, Pawn)   -> b { blackPawns   = setBit (blackPawns b) i }
               (Black, Knight) -> b { blackKnights = setBit (blackKnights b) i }
               (Black, Bishop) -> b { blackBishops = setBit (blackBishops b) i
                                    , blackDiagonal = setBit (blackDiagonal b) i }
               (Black, Rook)   -> b { blackRooks   = setBit (blackRooks b) i
                                    , blackOrthogonal = setBit (blackOrthogonal b) i }
               (Black, Queen)  -> b { blackQueens  = setBit (blackQueens b) i
                                    , blackDiagonal = setBit (blackDiagonal b) i
                                    , blackOrthogonal = setBit (blackOrthogonal b) i }
               (Black, King)   -> b { blackKings   = setBit (blackKings b) i }

        -- Update occupancy
        white = if c == White then setBit (occupiedWhite b) i else occupiedWhite b
        black = if c == Black then setBit (occupiedBlack b) i else occupiedBlack b
        total = setBit (occupiedTotal b) i

        -- Incremental Scores
        val = pstValue c pt sq
        mat = packedMaterialValue pt
        ph  = phaseValue pt

        newScoreWhite = if c == White then scoreWhite b + val + mat else scoreWhite b
        newScoreBlack = if c == Black then scoreBlack b + val + mat else scoreBlack b
        newPhase = gamePhase b + ph

    in b' { occupiedWhite = white, occupiedBlack = black, occupiedTotal = total
          , scoreWhite = newScoreWhite, scoreBlack = newScoreBlack, gamePhase = newPhase }

-- | Remove a piece from the board knowing its color and type.
-- Does not update other bitboards, so it is faster than removePieceAt.
unsafeRemovePiece :: Board -> Square -> Color -> PieceType -> Board
unsafeRemovePiece b sq c pt =
    let i = unSquare sq
        -- Update the specific bitboard and aggregates
        b' = case (c, pt) of
               (White, Pawn)   -> b { whitePawns   = clearBit (whitePawns b) i }
               (White, Knight) -> b { whiteKnights = clearBit (whiteKnights b) i }
               (White, Bishop) -> b { whiteBishops = clearBit (whiteBishops b) i
                                    , whiteDiagonal = clearBit (whiteDiagonal b) i }
               (White, Rook)   -> b { whiteRooks   = clearBit (whiteRooks b) i
                                    , whiteOrthogonal = clearBit (whiteOrthogonal b) i }
               (White, Queen)  -> b { whiteQueens  = clearBit (whiteQueens b) i
                                    , whiteDiagonal = clearBit (whiteDiagonal b) i
                                    , whiteOrthogonal = clearBit (whiteOrthogonal b) i }
               (White, King)   -> b { whiteKings   = clearBit (whiteKings b) i }

               (Black, Pawn)   -> b { blackPawns   = clearBit (blackPawns b) i }
               (Black, Knight) -> b { blackKnights = clearBit (blackKnights b) i }
               (Black, Bishop) -> b { blackBishops = clearBit (blackBishops b) i
                                    , blackDiagonal = clearBit (blackDiagonal b) i }
               (Black, Rook)   -> b { blackRooks   = clearBit (blackRooks b) i
                                    , blackOrthogonal = clearBit (blackOrthogonal b) i }
               (Black, Queen)  -> b { blackQueens  = clearBit (blackQueens b) i
                                    , blackDiagonal = clearBit (blackDiagonal b) i
                                    , blackOrthogonal = clearBit (blackOrthogonal b) i }
               (Black, King)   -> b { blackKings   = clearBit (blackKings b) i }

        -- Update occupancy
        white = if c == White then clearBit (occupiedWhite b) i else occupiedWhite b
        black = if c == Black then clearBit (occupiedBlack b) i else occupiedBlack b
        total = clearBit (occupiedTotal b) i

        -- Incremental Scores
        val = pstValue c pt sq
        mat = packedMaterialValue pt
        ph  = phaseValue pt

        newScoreWhite = if c == White then scoreWhite b - (val + mat) else scoreWhite b
        newScoreBlack = if c == Black then scoreBlack b - (val + mat) else scoreBlack b
        newPhase = gamePhase b - ph

    in b' { occupiedWhite = white, occupiedBlack = black, occupiedTotal = total
          , scoreWhite = newScoreWhite, scoreBlack = newScoreBlack, gamePhase = newPhase }

-- | Remove a piece from the board.
removePieceAt :: Board -> Square -> Board
removePieceAt b sq = updateOccupancy $ b
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
               (White, Bishop) -> b1 { whiteBishops = whiteBishops b1 `xor` mask
                                     , whiteDiagonal = whiteDiagonal b1 `xor` mask }
               (White, Rook)   -> b1 { whiteRooks   = whiteRooks b1 `xor` mask
                                     , whiteOrthogonal = whiteOrthogonal b1 `xor` mask }
               (White, Queen)  -> b1 { whiteQueens  = whiteQueens b1 `xor` mask
                                     , whiteDiagonal = whiteDiagonal b1 `xor` mask
                                     , whiteOrthogonal = whiteOrthogonal b1 `xor` mask }
               (White, King)   -> b1 { whiteKings   = whiteKings b1 `xor` mask }

               (Black, Pawn)   -> b1 { blackPawns   = blackPawns b1 `xor` mask }
               (Black, Knight) -> b1 { blackKnights = blackKnights b1 `xor` mask }
               (Black, Bishop) -> b1 { blackBishops = blackBishops b1 `xor` mask
                                     , blackDiagonal = blackDiagonal b1 `xor` mask }
               (Black, Rook)   -> b1 { blackRooks   = blackRooks b1 `xor` mask
                                     , blackOrthogonal = blackOrthogonal b1 `xor` mask }
               (Black, Queen)  -> b1 { blackQueens  = blackQueens b1 `xor` mask
                                     , blackDiagonal = blackDiagonal b1 `xor` mask
                                     , blackOrthogonal = blackOrthogonal b1 `xor` mask }
               (Black, King)   -> b1 { blackKings   = blackKings b1 `xor` mask }

        -- Update occupancy
        -- For the moving piece color, we flip both bits.
        whiteOcc = if c == White then occupiedWhite b1 `xor` mask else occupiedWhite b1
        blackOcc = if c == Black then occupiedBlack b1 `xor` mask else occupiedBlack b1
        totalOcc = occupiedTotal b1 `xor` mask

        -- Incremental Scores (Move only, capture handled in b1)
        pstFrom = pstValue c pt from
        pstTo   = pstValue c pt to

        newScoreWhite = if c == White then scoreWhite b1 - pstFrom + pstTo else scoreWhite b1
        newScoreBlack = if c == Black then scoreBlack b1 - pstFrom + pstTo else scoreBlack b1
        newPhase = gamePhase b1

    in b2 { occupiedWhite = whiteOcc, occupiedBlack = blackOcc, occupiedTotal = totalOcc
          , scoreWhite = newScoreWhite, scoreBlack = newScoreBlack, gamePhase = newPhase }

-- | Bitboard of all pieces.
{-# INLINE occupied #-}
occupied :: Board -> Bitboard
occupied = occupiedTotal

-- | Bitboard of pieces by color.
{-# INLINE occupiedBy #-}
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
-- Optimized using aggregated bitboards and direct field access.
{-# INLINE isAttackedBy #-}
isAttackedBy :: Board -> Color -> Square -> Bool
isAttackedBy b White sq =
  (pawnAttacks Black sq .&. whitePawns b /= 0) ||
  (knightAttacks sq .&. whiteKnights b /= 0) ||
  (kingAttacks sq .&. whiteKings b /= 0) ||
  (bishopAttacks sq (occupied b) .&. whiteDiagonal b /= 0) ||
  (rookAttacks sq (occupied b) .&. whiteOrthogonal b /= 0)
isAttackedBy b Black sq =
  (pawnAttacks White sq .&. blackPawns b /= 0) ||
  (knightAttacks sq .&. blackKnights b /= 0) ||
  (kingAttacks sq .&. blackKings b /= 0) ||
  (bishopAttacks sq (occupied b) .&. blackDiagonal b /= 0) ||
  (rookAttacks sq (occupied b) .&. blackOrthogonal b /= 0)

{-# INLINE oppositeColor #-}
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

-- | Move a piece assuming the target square is empty.
-- It updates the specific piece bitboard and the occupancy bitboards.
{-# INLINE unsafeMovePiece #-}
unsafeMovePiece :: Board -> Square -> Square -> Color -> PieceType -> Board
unsafeMovePiece b from to c pt =
    let fromI = unSquare from
        toI   = unSquare to
        mask = (1 `shiftL` fromI) `xor` (1 `shiftL` toI)

        b' = case (c, pt) of
               (White, Pawn)   -> b { whitePawns   = whitePawns b `xor` mask }
               (White, Knight) -> b { whiteKnights = whiteKnights b `xor` mask }
               (White, Bishop) -> b { whiteBishops = whiteBishops b `xor` mask
                                    , whiteDiagonal = whiteDiagonal b `xor` mask }
               (White, Rook)   -> b { whiteRooks   = whiteRooks b `xor` mask
                                    , whiteOrthogonal = whiteOrthogonal b `xor` mask }
               (White, Queen)  -> b { whiteQueens  = whiteQueens b `xor` mask
                                    , whiteDiagonal = whiteDiagonal b `xor` mask
                                    , whiteOrthogonal = whiteOrthogonal b `xor` mask }
               (White, King)   -> b { whiteKings   = whiteKings b `xor` mask }

               (Black, Pawn)   -> b { blackPawns   = blackPawns b `xor` mask }
               (Black, Knight) -> b { blackKnights = blackKnights b `xor` mask }
               (Black, Bishop) -> b { blackBishops = blackBishops b `xor` mask
                                    , blackDiagonal = blackDiagonal b `xor` mask }
               (Black, Rook)   -> b { blackRooks   = blackRooks b `xor` mask
                                    , blackOrthogonal = blackOrthogonal b `xor` mask }
               (Black, Queen)  -> b { blackQueens  = blackQueens b `xor` mask
                                    , blackDiagonal = blackDiagonal b `xor` mask
                                    , blackOrthogonal = blackOrthogonal b `xor` mask }
               (Black, King)   -> b { blackKings   = blackKings b `xor` mask }

        white = if c == White then occupiedWhite b `xor` mask else occupiedWhite b
        black = if c == Black then occupiedBlack b `xor` mask else occupiedBlack b
        total = occupiedTotal b `xor` mask

        -- Incremental Scores
        pstFrom = pstValue c pt from
        pstTo   = pstValue c pt to

        newScoreWhite = if c == White then scoreWhite b - pstFrom + pstTo else scoreWhite b
        newScoreBlack = if c == Black then scoreBlack b - pstFrom + pstTo else scoreBlack b
        newPhase = gamePhase b

    in b' { occupiedWhite = white, occupiedBlack = black, occupiedTotal = total
          , scoreWhite = newScoreWhite, scoreBlack = newScoreBlack, gamePhase = newPhase }

-- Attackers ------------------------------------------------------------------

-- | Returns a bitboard of all pieces attacking a square.
-- Uses the provided occupancy bitboard for sliding attacks.
{-# INLINE attackersTo #-}
attackersTo :: Board -> Square -> Bitboard -> Bitboard
attackersTo b sq occ =
    (pawnAttacks Black sq .&. whitePawns b) .|.
    (pawnAttacks White sq .&. blackPawns b) .|.
    (knightAttacks sq .&. (whiteKnights b .|. blackKnights b)) .|.
    (kingAttacks sq .&. (whiteKings b .|. blackKings b)) .|.
    (bishopAttacks sq occ .&. (whiteDiagonal b .|. blackDiagonal b)) .|.
    (rookAttacks sq occ .&. (whiteOrthogonal b .|. blackOrthogonal b))

-- | Get X-Ray attacker behind a piece.
{-# INLINE getXRayAttacker #-}
getXRayAttacker :: Board -> Square -> Square -> Bitboard -> Bitboard
getXRayAttacker b sq from occ =
    let r = ray sq from
        blockers = r .&. occ
    in if blockers == 0 then 0
       else
         let fromI = unSquare from
             sqI = unSquare sq
             diff = fromI - sqI
             attackerSq = if diff > 0
                          then Square (countTrailingZeros blockers)
                          else Square (63 - countLeadingZeros blockers)
         in case pieceAt b attackerSq of
              Nothing -> 0
              Just (Piece _ pt) ->
                  if isSlider pt && compatible pt sq from
                  then bbFromSquare attackerSq
                  else 0

-- | Check if a piece type is a slider.
{-# INLINE isSlider #-}
isSlider :: PieceType -> Bool
isSlider Bishop = True
isSlider Rook = True
isSlider Queen = True
isSlider _ = False

-- | Check if a piece type can attack along the ray between two squares.
{-# INLINE compatible #-}
compatible :: PieceType -> Square -> Square -> Bool
compatible pt sq from =
    let sameRank = squareRank sq == squareRank from
        sameFile = squareFile sq == squareFile from
        sameDiag = abs (squareFile sq - squareFile from) == abs (squareRank sq - squareRank from)
    in case pt of
        Rook -> sameRank || sameFile
        Bishop -> sameDiag
        Queen -> sameRank || sameFile || sameDiag
        _ -> False
