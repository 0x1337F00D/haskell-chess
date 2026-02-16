module Chess.Engine.SEE (see, seeGen, attackersTo, pieceValue) where

import Data.Bits
import Data.List (foldl')
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
        -- We don't have 'Color' passed to 'see', so we infer it from the moving piece.
        -- unsafePieceAt uses pieceAt which checks occupancy.
        -- But since we are moving from 'from', it must be occupied.
        c = pieceColor (unsafePieceAt board from)

    in runSeeStart board from to c valAttacker valTarget

{-# INLINE runSeeStart #-}
runSeeStart :: Board -> Square -> Square -> Color -> Int -> Int -> Int
runSeeStart b from to c valAttacker valTarget =
    let
        -- Initial gains list: [Target]
        initialGain = [valTarget]

        -- Remove moving piece from occupancy
        occ = occupied b `clearBit` (unSquare from)

        -- Compute attackers to 'to' square
        atts = attackersTo b to occ

        side = oppositeColor c

        -- Run swap
        scores = runSEE b to side occ atts (valAttacker : initialGain)

    in negamax scores

runSEE :: Board -> Square -> Color -> Bitboard -> Bitboard -> [Int] -> [Int]
runSEE b sq side occ atts gains =
    case getLeastValuableAttacker b side atts of
        Nothing -> gains
        Just (from, pt) ->
            let val = pieceValue pt
                newGains = val : gains

                -- Remove piece from occ
                newOcc = occ `clearBit` (unSquare from)

                -- Update attackers (X-ray)
                newAtts = (atts `clearBit` (unSquare from)) .|. getXRayAttacker b sq from newOcc

                nextSide = oppositeColor side
            in runSEE b sq nextSide newOcc newAtts newGains

negamax :: [Int] -> Int
negamax [] = 0
negamax [_] = 0
negamax (_:rest) =
    case reverse rest of
        [] -> 0
        (vTarget:victims) -> vTarget - foldl' (\s v -> max 0 (v - s)) 0 victims

getLeastValuableAttacker :: Board -> Color -> Bitboard -> Maybe (Square, PieceType)
getLeastValuableAttacker b side atts =
    let myAtts = atts .&. occupiedBy b side
    in if myAtts == 0 then Nothing
       else
         let pawns = myAtts .&. pieceBitboard b side Pawn
         in if pawns /= 0 then Just (Square (countTrailingZeros pawns), Pawn)
         else
           let knights = myAtts .&. pieceBitboard b side Knight
           in if knights /= 0 then Just (Square (countTrailingZeros knights), Knight)
           else
             let bishops = myAtts .&. pieceBitboard b side Bishop
             in if bishops /= 0 then Just (Square (countTrailingZeros bishops), Bishop)
             else
               let rooks = myAtts .&. pieceBitboard b side Rook
               in if rooks /= 0 then Just (Square (countTrailingZeros rooks), Rook)
               else
                 let queens = myAtts .&. pieceBitboard b side Queen
                 in if queens /= 0 then Just (Square (countTrailingZeros queens), Queen)
                 else
                   let kings = myAtts .&. pieceBitboard b side King
                   in if kings /= 0 then Just (Square (countTrailingZeros kings), King)
                   else Nothing

unsafePieceAt :: Board -> Square -> Piece
unsafePieceAt b sq = case pieceAt b sq of
    Just p -> p
    Nothing -> Piece White Pawn -- Should not happen in valid SEE calls
