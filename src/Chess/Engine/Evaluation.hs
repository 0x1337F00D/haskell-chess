{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Chess.Engine.Evaluation (
    evaluate,
    evaluatePos,
    Evaluate(..),
    evalKingSafety,
    evalMopUp,
    Score,
    totalPhase
) where

import qualified Data.Vector.Unboxed as U
import Data.Bits (countTrailingZeros, clearBit, popCount, (.&.), (.|.))

import Chess.Types
import Chess.Bitboard
import qualified Chess.Board.Base as Base
import Chess.Board.GameState (GameState(..))
import Chess.Board (Board(..), ValidatedBoard, getBoard)
import Chess.Board.Phase (Phase(..), Position(..))
import Chess.Data.Evaluation

-- | Evaluation Typeclass
class Evaluate (p :: Phase) where
    evaluatePos :: Position p -> Score

instance Evaluate 'Opening where
    {-# INLINE evaluatePos #-}
    evaluatePos (Position vBoard) =
        let (Board b gs _) = getBoard vBoard
            clampedPhase = min totalPhase (max 0 (Base.gamePhase b))

            (mgW, egW) = unpackScore (Base.scoreWhite b)
            (mgB, egB) = unpackScore (Base.scoreBlack b)

            mgScore = mgW - mgB
            egScore = egW - egB

            (wSafety, bSafety) = evalKingSafety b
            safetyAdj = bSafety - wSafety
            -- No MopUp in Opening
            egScoreTotal = egScore + safetyAdj
            finalScore = (((mgScore + safetyAdj) * clampedPhase) + (egScoreTotal * (totalPhase - clampedPhase))) `div` totalPhase
        in if turn gs == White then finalScore else -finalScore

instance Evaluate 'Middlegame where
    {-# INLINE evaluatePos #-}
    evaluatePos (Position vBoard) =
        let (Board b gs _) = getBoard vBoard
            clampedPhase = min totalPhase (max 0 (Base.gamePhase b))

            (mgW, egW) = unpackScore (Base.scoreWhite b)
            (mgB, egB) = unpackScore (Base.scoreBlack b)

            mgScore = mgW - mgB
            egScore = egW - egB

            (wSafety, bSafety) = evalKingSafety b
            safetyAdj = bSafety - wSafety

            -- Conditional MopUp: If phase drops to Endgame levels (< 10), enable MopUp.
            mopUpAdj = if clampedPhase < 10 then evalMopUp b else 0

            egScoreTotal = egScore + safetyAdj + mopUpAdj
            finalScore = (((mgScore + safetyAdj) * clampedPhase) + (egScoreTotal * (totalPhase - clampedPhase))) `div` totalPhase
        in if turn gs == White then finalScore else -finalScore

instance Evaluate 'Endgame where
    {-# INLINE evaluatePos #-}
    evaluatePos (Position vBoard) =
        let (Board b gs _) = getBoard vBoard
            clampedPhase = min totalPhase (max 0 (Base.gamePhase b))

            (mgW, egW) = unpackScore (Base.scoreWhite b)
            (mgB, egB) = unpackScore (Base.scoreBlack b)

            mgScore = mgW - mgB
            egScore = egW - egB

            (wSafety, bSafety) = evalKingSafety b
            safetyAdj = bSafety - wSafety
            -- MopUp in Endgame
            mopUpAdj = evalMopUp b
            egScoreTotal = egScore + safetyAdj + mopUpAdj
            finalScore = (((mgScore + safetyAdj) * clampedPhase) + (egScoreTotal * (totalPhase - clampedPhase))) `div` totalPhase
        in if turn gs == White then finalScore else -finalScore

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
evaluate vBoard =
    let (Board b gs _) = getBoard vBoard

        clampedPhase = min totalPhase (max 0 (Base.gamePhase b))

        (mgW, egW) = unpackScore (Base.scoreWhite b)
        (mgB, egB) = unpackScore (Base.scoreBlack b)

        mgScore = mgW - mgB
        egScore = egW - egB

        (wSafety, bSafety) = evalKingSafety b
        safetyAdj = bSafety - wSafety

        mopUpAdj = evalMopUp b

        egScoreTotal = egScore + safetyAdj + mopUpAdj

        finalScore = (((mgScore + safetyAdj) * clampedPhase) + (egScoreTotal * (totalPhase - clampedPhase))) `div` totalPhase
    in if turn gs == White then finalScore else -finalScore

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

        loopKnights :: Bitboard -> Int -> Int
        loopKnights 0 !acc = acc
        loopKnights bb !acc =
            let i = countTrailingZeros bb
                s = Square i
                !atts = knightAttacks s
                !hits = popCount (atts .&. zone)
            in loopKnights (clearBit bb i) (acc + hits * 2)

        loopBishops :: Bitboard -> Int -> Int
        loopBishops 0 !acc = acc
        loopBishops bb !acc =
            let i = countTrailingZeros bb
                s = Square i
                !atts = bishopAttacks s occ
                !hits = popCount (atts .&. zone)
            in loopBishops (clearBit bb i) (acc + hits * 2)

        loopRooks :: Bitboard -> Int -> Int
        loopRooks 0 !acc = acc
        loopRooks bb !acc =
            let i = countTrailingZeros bb
                s = Square i
                !atts = rookAttacks s occ
                !hits = popCount (atts .&. zone)
            in loopRooks (clearBit bb i) (acc + hits * 3)

        loopQueens :: Bitboard -> Int -> Int
        loopQueens 0 !acc = acc
        loopQueens bb !acc =
            let i = countTrailingZeros bb
                s = Square i
                !attB = bishopAttacks s occ
                !attR = rookAttacks s occ
                !hits = popCount ((attB .|. attR) .&. zone)
            in loopQueens (clearBit bb i) (acc + hits * 5)

        !vN = loopKnights enemyKnights 0
        !vB = loopBishops enemyBishops 0
        !vR = loopRooks enemyRooks 0
        !vQ = loopQueens enemyQueens 0

        !totalUnits = vN + vB + vR + vQ

    in if totalUnits == 0 then 0 else safetyTable U.! (min 99 totalUnits)
