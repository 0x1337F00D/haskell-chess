{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Chess.Engine.Evaluation (
    evaluate,
    evaluatePos,
    Evaluate(..),
    evalMaterial,
    evalPST,
    evalKingSafety,
    evalMopUp,
    evalPhase,
    Score,
    totalPhase
) where

import qualified Data.Vector.Unboxed as U
import Data.Bits (countTrailingZeros, clearBit, popCount, (.&.), (.|.), shiftL, shiftR)

import Chess.Types
import Chess.Bitboard
import qualified Chess.Board.Base as Base
import Chess.Board.GameState (GameState(..))
import Chess.Board (Board(..), ValidatedBoard, getBoard)
import Chess.Board.Phase (Phase(..), Position(..))
import qualified Chess.Data.Evaluation as E

-- | Evaluation Typeclass
class Evaluate (p :: Phase) where
    evaluatePos :: Position p -> Score

instance Evaluate 'Opening where
    {-# INLINE evaluatePos #-}
    evaluatePos (Position vBoard) = evaluateScore vBoard False

instance Evaluate 'Middlegame where
    {-# INLINE evaluatePos #-}
    evaluatePos (Position vBoard) = evaluateScore vBoard False

instance Evaluate 'Endgame where
    {-# INLINE evaluatePos #-}
    evaluatePos (Position vBoard) = evaluateScore vBoard True

-- | Common evaluation function using incremental updates.
{-# INLINE evaluateScore #-}
evaluateScore :: ValidatedBoard -> Bool -> Score
evaluateScore vBoard isEndgame =
    let (Board b gs _) = getBoard vBoard

        -- Retrieve incremental scores
        wPacked = Base.scoreWhite b
        bPacked = Base.scoreBlack b
        phase   = Base.gamePhase b
        clampedPhase = min E.totalPhase (max 0 phase)

        -- Calculate counts for bias correction
        wCount = popCount (Base.occupiedWhite b)
        bCount = popCount (Base.occupiedBlack b)

        bias = 65536

        -- Unpack scores
        -- MG is in upper 32 bits
        mgScore = (wPacked `shiftR` 32) - (bPacked `shiftR` 32)

        -- EG is in lower 32 bits, biased
        egScore = ((wPacked .&. 0xFFFFFFFF) - wCount * bias) -
                  ((bPacked .&. 0xFFFFFFFF) - bCount * bias)

        -- Dynamic Terms (not incremental yet)
        (wSafety, bSafety) = evalKingSafety b
        safetyAdj = bSafety - wSafety

        mopUpAdj = if isEndgame || clampedPhase < 10 then evalMopUp b else 0

        egScoreTotal = egScore + safetyAdj + mopUpAdj

        finalScore = (((mgScore + safetyAdj) * clampedPhase) + (egScoreTotal * (E.totalPhase - clampedPhase))) `div` E.totalPhase
    in if turn gs == White then finalScore else -finalScore

-- | Tapered Evaluation Parameters
totalPhase :: Int
totalPhase = E.totalPhase

-- | Calculate the phase of the game (0 = Endgame, 24 = Start).
evalPhase :: Base.Board -> Int
evalPhase b =
    let wn = popCount (Base.whiteKnights b)
        wb = popCount (Base.whiteBishops b)
        wr = popCount (Base.whiteRooks b)
        wq = popCount (Base.whiteQueens b)
        bn = popCount (Base.blackKnights b)
        bb = popCount (Base.blackBishops b)
        br = popCount (Base.blackRooks b)
        bq = popCount (Base.blackQueens b)
        phase = wn * E.phaseKnight + wb * E.phaseBishop + wr * E.phaseRook + wq * E.phaseQueen +
                bn * E.phaseKnight + bb * E.phaseBishop + br * E.phaseRook + bq * E.phaseQueen
    in min totalPhase (max 0 phase)

-- | Calculate Material Scores (MG, EG).
evalMaterial :: Base.Board -> (Score, Score)
evalMaterial b =
    let !wp = popCount (Base.whitePawns b)
        !wn = popCount (Base.whiteKnights b)
        !wb = popCount (Base.whiteBishops b)
        !wr = popCount (Base.whiteRooks b)
        !wq = popCount (Base.whiteQueens b)
        !bp = popCount (Base.blackPawns b)
        !bn = popCount (Base.blackKnights b)
        !bb = popCount (Base.blackBishops b)
        !br = popCount (Base.blackRooks b)
        !bq = popCount (Base.blackQueens b)

        !mgMat = (wp * E.mgValuePawn + wn * E.mgValueKnight + wb * E.mgValueBishop + wr * E.mgValueRook + wq * E.mgValueQueen) -
                 (bp * E.mgValuePawn + bn * E.mgValueKnight + bb * E.mgValueBishop + br * E.mgValueRook + bq * E.mgValueQueen)
        !egMat = (wp * E.egValuePawn + wn * E.egValueKnight + wb * E.egValueBishop + wr * E.egValueRook + wq * E.egValueQueen) -
                 (bp * E.egValuePawn + bn * E.egValueKnight + bb * E.egValueBishop + br * E.egValueRook + bq * E.egValueQueen)
    in (mgMat, egMat)

-- | Calculate PST Scores (MG, EG).
evalPST :: Base.Board -> (Score, Score)
evalPST b =
    let !wPacked = evalPacked (Base.whitePawns b)   E.packedPawnTable   +
                   evalPacked (Base.whiteKnights b) E.packedKnightTable +
                   evalPacked (Base.whiteBishops b) E.packedBishopTable +
                   evalPacked (Base.whiteRooks b)   E.packedRookTable   +
                   evalPacked (Base.whiteQueens b)  E.packedQueenTable  +
                   evalPacked (Base.whiteKings b)   E.packedKingTable

        !bPacked = evalPacked (Base.blackPawns b)   E.packedPawnTableFlip   +
                   evalPacked (Base.blackKnights b) E.packedKnightTableFlip +
                   evalPacked (Base.blackBishops b) E.packedBishopTableFlip +
                   evalPacked (Base.blackRooks b)   E.packedRookTableFlip   +
                   evalPacked (Base.blackQueens b)  E.packedQueenTableFlip  +
                   evalPacked (Base.blackKings b)   E.packedKingTableFlip

        !wp = popCount (Base.whitePawns b)
        !wn = popCount (Base.whiteKnights b)
        !wb = popCount (Base.whiteBishops b)
        !wr = popCount (Base.whiteRooks b)
        !wq = popCount (Base.whiteQueens b)
        !wk = popCount (Base.whiteKings b)
        !bp = popCount (Base.blackPawns b)
        !bn = popCount (Base.blackKnights b)
        !bb = popCount (Base.blackBishops b)
        !br = popCount (Base.blackRooks b)
        !bq = popCount (Base.blackQueens b)
        !bk = popCount (Base.blackKings b)

        !wCount = wp + wn + wb + wr + wq + wk
        !bCount = bp + bn + bb + br + bq + bk

        !mgPST = (wPacked `shiftR` 32) - (bPacked `shiftR` 32)
        !egPST = ((wPacked .&. 0xFFFFFFFF) - wCount * 65536) -
                 ((bPacked .&. 0xFFFFFFFF) - bCount * 65536)
    in (mgPST, egPST)

-- | Calculate King Safety Score (MG bias usually).
-- Returns (White Safety Penalty, Black Safety Penalty). Positive means penalty (bad for that side).
-- We return (WSafety, BSafety).
evalKingSafety :: Base.Board -> (Score, Score)
evalKingSafety b =
    let wKingSq = countTrailingZeros (Base.whiteKings b)
        bKingSq = countTrailingZeros (Base.blackKings b)
        wSafety = kingSafety b White (Square wKingSq)
        bSafety = kingSafety b Black (Square bKingSq)
    in (wSafety, bSafety)

-- | Calculate MopUp Score (EG bias).
-- Returns score from White's perspective.
evalMopUp :: Base.Board -> Score
evalMopUp b =
    let wKingSq = countTrailingZeros (Base.whiteKings b)
        bKingSq = countTrailingZeros (Base.blackKings b)

        !wKRank = wKingSq `div` 8
        !wKFile = wKingSq `mod` 8
        !bKRank = bKingSq `div` 8
        !bKFile = bKingSq `mod` 8

        !wDistCenter = abs (wKRank * 2 - 7) + abs (wKFile * 2 - 7)
        !bDistCenter = abs (bKRank * 2 - 7) + abs (bKFile * 2 - 7)

        !distKings = abs (wKRank - bKRank) + abs (wKFile - bKFile)

        !wMopUp = 5 * bDistCenter - 2 * distKings
        !bMopUp = 5 * wDistCenter - 2 * distKings
    in wMopUp - bMopUp

-- | Evaluate the board position from the perspective of the side to move.
-- Now composed of helper functions.
evaluate :: ValidatedBoard -> Score
evaluate vBoard = evaluateScore vBoard True

evalPacked :: Bitboard -> U.Vector PackedScore -> PackedScore
{-# INLINE evalPacked #-}
evalPacked bb table = go bb 0
  where
    go :: Bitboard -> PackedScore -> PackedScore
    go 0 !acc = acc
    go b !acc =
        let i = countTrailingZeros b
            !packed = table `U.unsafeIndex` i
        in go (clearBit b i) (acc + packed)

-- | Calculate King Safety Penalty
kingSafety :: Base.Board -> Color -> Square -> Score
kingSafety b us kSq =
    let them = if us == White then Black else White
        zone = kingAttacks kSq
        occ = Base.occupied b

        -- Enemy pieces
        enemyKnights = Base.pieceBitboard b them Knight
        enemyBishops = Base.pieceBitboard b them Bishop
        enemyRooks   = Base.pieceBitboard b them Rook
        enemyQueens  = Base.pieceBitboard b them Queen

        countAttacks :: Bitboard -> (Square -> Bitboard) -> Int -> Int
        countAttacks 0 _ _ = 0
        countAttacks bb attacksFn units = go bb 0
          where
            go 0 !acc = acc
            go pieces !acc =
                let i = countTrailingZeros pieces
                    sq = Square i
                    atts = attacksFn sq
                    -- Count how many squares in the king zone are attacked
                    hits = popCount (atts .&. zone)
                in go (clearBit pieces i) (acc + hits * units)

        -- Accumulate attack units
        !vN = countAttacks enemyKnights knightAttacks 2
        !vB = countAttacks enemyBishops (\s -> bishopAttacks s occ) 2
        !vR = countAttacks enemyRooks   (\s -> rookAttacks s occ) 3
        !vQ = countAttacks enemyQueens  (\s -> bishopAttacks s occ .|. rookAttacks s occ) 5

        !totalUnits = vN + vB + vR + vQ

    in if totalUnits == 0 then 0 else E.safetyTable U.! (min 99 totalUnits)
