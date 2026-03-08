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
import Chess.Board.GameState
import Chess.Board (Board(..), ValidatedBoard, getBoard)
import Chess.Board.Phase (Phase(..), Position(..))
import Chess.Data.Evaluation

import Chess.NNUE.Types (Nnue)
import Chess.NNUE.Flat
import Chess.NNUE.Feature
import Chess.NNUE.Accumulator
import Chess.NNUE.Eval
import System.IO.Unsafe (unsafePerformIO)
import Control.Exception (try, SomeException)

{-# NOINLINE globalNnue #-}
globalNnue :: Maybe Nnue
globalNnue = unsafePerformIO $ do
  res <- try (loadNnueFlat "tiny.hsnn") :: IO (Either SomeException Nnue)
  case res of
    Left _ -> pure Nothing
    Right n -> pure (Just n)

evaluateNNUE :: Base.Board -> GameState -> Score
evaluateNNUE !b !gs = case globalNnue of
  Nothing -> 0
  Just nnue -> unsafePerformIO $ do
    let !afs = collectFeaturesHalfKP b
    !acc <- refreshAcc nnue afs
    let !score = evalAcc nnue acc gs
    pure $ if turn gs == White then score else -score

-- | Evaluation Typeclass
class Evaluate (p :: Phase) where
    evaluatePos :: Position p s -> Score

instance Evaluate 'Opening where
    {-# INLINE evaluatePos #-}
    evaluatePos (Position vBoard) = evaluate vBoard

instance Evaluate 'Middlegame where
    {-# INLINE evaluatePos #-}
    evaluatePos (Position vBoard) = evaluate vBoard

instance Evaluate 'Endgame where
    {-# INLINE evaluatePos #-}
    evaluatePos (Position vBoard) = evaluate vBoard

-- | Calculate King Safety Score (MG bias usually).
-- Returns (White Safety Penalty, Black Safety Penalty). Positive means penalty (bad for that side).
-- We return (WSafety, BSafety).
evalKingSafety :: Base.Board -> (Score, Score)
{-# INLINE evalKingSafety #-}
evalKingSafety b =
    let wKingSq = countTrailingZeros (Base.whiteKings b)
        bKingSq = countTrailingZeros (Base.blackKings b)
        wSafety = kingSafety b White (Square wKingSq)
        bSafety = kingSafety b Black (Square bKingSq)
    in (wSafety, bSafety)

-- | Calculate MopUp Score (EG bias).
-- Returns score from White's perspective.
evalMopUp :: Base.Board -> Score
{-# INLINE evalMopUp #-}
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
evaluate :: ValidatedBoard s -> Score
evaluate vBoard =
    let (Board b gs _) = getBoard vBoard
        clampedPhase = min totalPhase (max 0 (Base.gamePhase b))

        (mgW, egW) = unpackScore (Base.scoreWhite b)
        (mgB, egB) = unpackScore (Base.scoreBlack b)

        mgScore = mgW - mgB
        egScore = egW - egB

        (wSafety, bSafety) = evalKingSafety b
        safetyAdj = bSafety - wSafety

        mopUpAdj = if clampedPhase < 10 then evalMopUp b else 0

        egScoreTotal = egScore + safetyAdj + mopUpAdj

        finalScore = (((mgScore + safetyAdj) * clampedPhase) + (egScoreTotal * (totalPhase - clampedPhase))) `div` totalPhase
        classicalScore = if turn gs == White then finalScore else -finalScore

        nnueScore = evaluateNNUE b gs
    in classicalScore + nnueScore

-- | Calculate King Safety Penalty
kingSafety :: Base.Board -> Color -> Square -> Score
kingSafety b us kSq =
    let zone = kingAttacks kSq
        occ = Base.occupied b

        -- Enemy pieces (unpacked directly)
        (enemyKnights, enemyBishops, enemyRooks, enemyQueens) = case us of
            Black -> (Base.whiteKnights b, Base.whiteBishops b, Base.whiteRooks b, Base.whiteQueens b)
            White -> (Base.blackKnights b, Base.blackBishops b, Base.blackRooks b, Base.blackQueens b)

        !vN = foldBitboard (\acc s -> acc + popCount (knightAttacks s .&. zone) * 2) 0 enemyKnights
        !vB = foldBitboard (\acc s -> acc + popCount (bishopAttacks s occ .&. zone) * 2) 0 enemyBishops
        !vR = foldBitboard (\acc s -> acc + popCount (rookAttacks s occ .&. zone) * 3) 0 enemyRooks
        !vQ = foldBitboard (\acc s -> acc + popCount ((bishopAttacks s occ .|. rookAttacks s occ) .&. zone) * 5) 0 enemyQueens

        !totalUnits = vN + vB + vR + vQ

    in if totalUnits == 0 then 0 else safetyTable `U.unsafeIndex` (min 99 totalUnits)
