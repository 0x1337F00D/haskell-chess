module Chess.Engine.SEE (see, seeGen, attackersTo, pieceValue) where

import Data.Bits
import Chess.Types
import Chess.Bitboard
import Chess.Board.Base
import qualified Chess.Board.MoveGen as MoveGen

-- | Piece values for SEE.
pieceValue :: PieceType -> Int
pieceValue Pawn   = 100
pieceValue Knight = 320
pieceValue Bishop = 330
pieceValue Rook   = 500
pieceValue Queen  = 900
pieceValue King   = 20000

-- | SEE Implementation using GenMove.
-- Avoids Move allocation and piece lookups.
{-# INLINE seeGen #-}
seeGen :: Board -> Color -> MoveGen.GenMove -> Int
seeGen b c gm = case gm of
    MoveGen.GenCapture from to pt capPt ->
        runSeeStart b from to c (pieceValue pt) (pieceValue capPt)
    MoveGen.GenEnPassant from to ->
        runSeeStart b from to c 100 100
    _ -> 0

-- | SEE Implementation.
see :: Board -> Move -> Int
see _ NullMove = 0
see _ (DropMove {}) = 0
see board (Move from to _) =
    let
        -- Target value
        target = case pieceAt board to of
            Just (Piece _ pt) -> pieceValue pt
            Nothing -> 0

        -- En Passant check
        isEP = target == 0 && (pieceType (unsafePieceAt board from) == Pawn) && (squareFile from /= squareFile to)
        valTarget = if isEP then 100 else target

        -- Attacker value
        valAttacker = pieceValue (pieceType (unsafePieceAt board from))

        -- Side to move (attacker)
        c = pieceColor (unsafePieceAt board from)

    in runSeeStart board from to c valAttacker valTarget

{-# INLINE runSeeStart #-}
runSeeStart :: Board -> Square -> Square -> Color -> Int -> Int -> Int
runSeeStart b from to c valAttacker valTarget =
    let
        -- Remove moving piece from occupancy
        occ = occupied b `clearBit` (unSquare from)

        -- Compute attackers to 'to' square.
        -- We must remove 'from' from attackers because the piece at 'from' is the one moving to 'to'.
        -- (It would be included in attackersTo because the board bitboards still have it at 'from').
        atts = attackersTo b to occ `clearBit` (unSquare from)

        side = oppositeColor c

        -- Run swap. Pass valAttacker as the next victim.
        score = runSEERec b to side occ atts valAttacker

    in valTarget - score

runSEERec :: Board -> Square -> Color -> Bitboard -> Bitboard -> Int -> Int
runSEERec b sq side occ atts valVictim =
    let lva = getLeastValuableAttackerPacked b side atts
    in if lva == noAttacker
       then 0
       else
            let valAttacker = lva `shiftR` 6
                fromI = lva .&. 0x3F
                from = Square fromI

                -- Remove piece from occ
                newOcc = occ `clearBit` fromI

                -- Update attackers (X-ray)
                newAtts = (atts `clearBit` fromI) .|. getXRayAttacker b sq from newOcc

                nextSide = oppositeColor side

                -- Recurse
                scoreNext = runSEERec b sq nextSide newOcc newAtts valAttacker

            in max 0 (valVictim - scoreNext)

noAttacker :: Int
noAttacker = -1

packAttacker :: Int -> Bitboard -> Int
packAttacker value bb = (value `shiftL` 6) .|. countTrailingZeros bb

-- | Pack the least valuable attacker as (piece value << 6) | square.
-- Returns noAttacker when there is no attacker for the side.
{-# INLINE getLeastValuableAttackerPacked #-}
getLeastValuableAttackerPacked :: Board -> Color -> Bitboard -> Int
getLeastValuableAttackerPacked b side atts =
    let myAtts = atts .&. occupiedBy b side
    in if myAtts == 0 then noAttacker
       else
         let pawns = myAtts .&. pieceBitboard b side Pawn
         in if pawns /= 0 then packAttacker 100 pawns
         else
           let knights = myAtts .&. pieceBitboard b side Knight
           in if knights /= 0 then packAttacker 320 knights
           else
             let bishops = myAtts .&. pieceBitboard b side Bishop
             in if bishops /= 0 then packAttacker 330 bishops
             else
               let rooks = myAtts .&. pieceBitboard b side Rook
               in if rooks /= 0 then packAttacker 500 rooks
               else
                 let queens = myAtts .&. pieceBitboard b side Queen
                 in if queens /= 0 then packAttacker 900 queens
                 else
                   let kings = myAtts .&. pieceBitboard b side King
                   in if kings /= 0 then packAttacker 20000 kings
                   else noAttacker

unsafePieceAt :: Board -> Square -> Piece
unsafePieceAt b sq = case pieceAt b sq of
    Just p -> p
    Nothing -> Piece White Pawn -- Should not happen in valid SEE calls
