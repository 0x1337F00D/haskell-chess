module Chess.Engine.SEE (see, attackersTo) where

import Data.Bits
import Data.List (foldl')
import Chess.Types
import Chess.Bitboard
import Chess.Board.Base

-- | Piece values for SEE.
pieceValue :: PieceType -> Int
pieceValue Pawn   = 100
pieceValue Knight = 320
pieceValue Bishop = 330
pieceValue Rook   = 500
pieceValue Queen  = 900
pieceValue King   = 20000

-- | Returns a bitboard of all pieces attacking a square.
-- Uses the provided occupancy bitboard for sliding attacks.
attackersTo :: Board -> Square -> Bitboard -> Bitboard
attackersTo b sq occ =
    (pawnAttacks Black sq .&. whitePawns b) .|.
    (pawnAttacks White sq .&. blackPawns b) .|.
    (knightAttacks sq .&. (whiteKnights b .|. blackKnights b)) .|.
    (kingAttacks sq .&. (whiteKings b .|. blackKings b)) .|.
    (bishopAttacks sq occ .&. (whiteDiagonal b .|. blackDiagonal b)) .|.
    (rookAttacks sq occ .&. (whiteOrthogonal b .|. blackOrthogonal b))

-- | SEE Implementation.
see :: Board -> Move -> Int
see _ NullMove = 0
see _ (DropMove {}) = 0
see board move =
    let
        from = mFrom move
        to = mTo move

        -- Target value
        target = case pieceAt board to of
            Just (Piece _ pt) -> pieceValue pt
            Nothing -> 0

        -- En Passant check
        isEP = target == 0 && (pieceType (unsafePieceAt board from) == Pawn) && (squareFile from /= squareFile to)
        valTarget = if isEP then 100 else target

        -- Attacker value
        valAttacker = pieceValue (pieceType (unsafePieceAt board from))

        -- Initial gains list: [Target]
        initialGain = [valTarget]

        -- Remove moving piece from occupancy
        occ = occupied board `clearBit` (unSquare from)

        -- Compute attackers to 'to' square
        atts = attackersTo board to occ

        side = oppositeColor (pieceColor (unsafePieceAt board from))

        -- Run swap
        scores = runSEE board to side occ atts (valAttacker : initialGain)

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

isSlider :: PieceType -> Bool
isSlider Bishop = True
isSlider Rook = True
isSlider Queen = True
isSlider _ = False

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

unsafePieceAt :: Board -> Square -> Piece
unsafePieceAt b sq = case pieceAt b sq of
    Just p -> p
    Nothing -> Piece White Pawn -- Should not happen in valid SEE calls
