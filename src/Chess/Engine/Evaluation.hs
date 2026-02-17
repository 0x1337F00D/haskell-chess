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

-- | Evaluation score in centipawns.
type Score = Int

-- | Packed Score (MG in upper 32 bits, EG in lower 32 bits).
type PackedScore = Int

-- | Evaluation Typeclass
class Evaluate (p :: Phase) where
    evaluatePos :: Position p -> Score

instance Evaluate 'Opening where
    {-# INLINE evaluatePos #-}
    evaluatePos (Position vBoard) =
        let (Board b gs _) = getBoard vBoard
            clampedPhase = evalPhase b
            (mgMat, egMat) = evalMaterial b
            (mgPST, egPST) = evalPST b
            mgScore = mgMat + mgPST
            egScore = egMat + egPST
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
            clampedPhase = evalPhase b
            (mgMat, egMat) = evalMaterial b
            (mgPST, egPST) = evalPST b
            mgScore = mgMat + mgPST
            egScore = egMat + egPST
            (wSafety, bSafety) = evalKingSafety b
            safetyAdj = bSafety - wSafety

            -- Conditional MopUp: If phase drops to Endgame levels (< 10), enable MopUp.
            -- This handles transition from Middlegame to Endgame within the search tree.
            mopUpAdj = if clampedPhase < 10 then evalMopUp b else 0

            egScoreTotal = egScore + safetyAdj + mopUpAdj
            finalScore = (((mgScore + safetyAdj) * clampedPhase) + (egScoreTotal * (totalPhase - clampedPhase))) `div` totalPhase
        in if turn gs == White then finalScore else -finalScore

instance Evaluate 'Endgame where
    {-# INLINE evaluatePos #-}
    evaluatePos (Position vBoard) =
        let (Board b gs _) = getBoard vBoard
            clampedPhase = evalPhase b
            (mgMat, egMat) = evalMaterial b
            (mgPST, egPST) = evalPST b
            mgScore = mgMat + mgPST
            egScore = egMat + egPST
            (wSafety, bSafety) = evalKingSafety b
            safetyAdj = bSafety - wSafety
            -- MopUp in Endgame
            mopUpAdj = evalMopUp b
            egScoreTotal = egScore + safetyAdj + mopUpAdj
            finalScore = (((mgScore + safetyAdj) * clampedPhase) + (egScoreTotal * (totalPhase - clampedPhase))) `div` totalPhase
        in if turn gs == White then finalScore else -finalScore

-- | Pack two scores into one 64-bit integer (assuming 64-bit Int).
-- We bias EG by +65536 to ensure it is positive and doesn't carry into MG.
packScore :: Score -> Score -> PackedScore
packScore mg eg = (mg `shiftL` 32) .|. ((eg + 65536) .&. 0xFFFFFFFF)

-- | Tapered Evaluation Parameters
totalPhase :: Int
totalPhase = 24

phasePawn, phaseKnight, phaseBishop, phaseRook, phaseQueen :: Int
phasePawn = 0
phaseKnight = 1
phaseBishop = 1
phaseRook = 2
phaseQueen = 4

-- | MG and EG values for material
mgValuePawn, mgValueKnight, mgValueBishop, mgValueRook, mgValueQueen :: Score
mgValuePawn   = 82
mgValueKnight = 337
mgValueBishop = 365
mgValueRook   = 477
mgValueQueen  = 1025

egValuePawn, egValueKnight, egValueBishop, egValueRook, egValueQueen :: Score
egValuePawn   = 94
egValueKnight = 281
egValueBishop = 297
egValueRook   = 512
egValueQueen  = 936

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
        phase = wn * phaseKnight + wb * phaseBishop + wr * phaseRook + wq * phaseQueen +
                bn * phaseKnight + bb * phaseBishop + br * phaseRook + bq * phaseQueen
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

        !mgMat = (wp * mgValuePawn + wn * mgValueKnight + wb * mgValueBishop + wr * mgValueRook + wq * mgValueQueen) -
                 (bp * mgValuePawn + bn * mgValueKnight + bb * mgValueBishop + br * mgValueRook + bq * mgValueQueen)
        !egMat = (wp * egValuePawn + wn * egValueKnight + wb * egValueBishop + wr * egValueRook + wq * egValueQueen) -
                 (bp * egValuePawn + bn * egValueKnight + bb * egValueBishop + br * egValueRook + bq * egValueQueen)
    in (mgMat, egMat)

-- | Calculate PST Scores (MG, EG).
evalPST :: Base.Board -> (Score, Score)
evalPST b =
    let !wPacked = evalPacked (Base.whitePawns b)   packedPawnTable   +
                   evalPacked (Base.whiteKnights b) packedKnightTable +
                   evalPacked (Base.whiteBishops b) packedBishopTable +
                   evalPacked (Base.whiteRooks b)   packedRookTable   +
                   evalPacked (Base.whiteQueens b)  packedQueenTable  +
                   evalPacked (Base.whiteKings b)   packedKingTable

        !bPacked = evalPacked (Base.blackPawns b)   packedPawnTableFlip   +
                   evalPacked (Base.blackKnights b) packedKnightTableFlip +
                   evalPacked (Base.blackBishops b) packedBishopTableFlip +
                   evalPacked (Base.blackRooks b)   packedRookTableFlip   +
                   evalPacked (Base.blackQueens b)  packedQueenTableFlip  +
                   evalPacked (Base.blackKings b)   packedKingTableFlip

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
evaluate vBoard =
    let (Board b gs _) = getBoard vBoard

        clampedPhase = evalPhase b

        (mgMat, egMat) = evalMaterial b
        (mgPST, egPST) = evalPST b

        mgScore = mgMat + mgPST
        egScore = egMat + egPST

        (wSafety, bSafety) = evalKingSafety b
        safetyAdj = bSafety - wSafety

        mopUpAdj = evalMopUp b

        egScoreTotal = egScore + safetyAdj + mopUpAdj

        finalScore = (((mgScore + safetyAdj) * clampedPhase) + (egScoreTotal * (totalPhase - clampedPhase))) `div` totalPhase
    in if turn gs == White then finalScore else -finalScore

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

-- | Flip a PSTO table for Black (mirror ranks).
-- Transforms a White table to Black table.
flipTable :: U.Vector Score -> U.Vector Score
flipTable v = U.generate 64 $ \i ->
    let r = i `div` 8
        f = i `mod` 8
    in v U.! ((7 - r) * 8 + f)

-- Raw Tables (Rank 8 to Rank 1)
-- Because we use fromList, index 0 corresponds to A1 if we listed Rank 1 first.
-- But the data is Rank 8 first. So index 0 gets Rank 8 data.
-- This effectively creates a vertically flipped board (Black perspective).
-- So rawPawnTable IS the Black table (mgPawnTableFlip).
-- And we need to flip it to get White table.

rawMgPawnTable, rawEgPawnTable :: U.Vector Score
rawMgPawnTable = U.fromList
    [ 0,   0,   0,   0,   0,   0,  0,   0
    , 98, 134,  61,  95,  68, 126, 34, -11
    , -6,   7,  26,  31,  65,  56, 25, -20
    ,-14,  13,   6,  21,  23,  12, 17, -23
    ,-27,  -2,  -5,  12,  17,   6, 10, -25
    ,-26,  -4,  -4, -10,   3,   3, 33, -12
    ,-35,  -1, -20, -23, -15,  24, 38, -22
    ,  0,   0,   0,   0,   0,   0,  0,   0
    ]
rawEgPawnTable = U.fromList
    [  0,   0,   0,   0,   0,   0,   0,   0
    ,178, 173, 158, 134, 147, 132, 165, 187
    , 94, 100,  85,  67,  56,  53,  82,  83
    , 32,  24,  13,   5,  -2,   4,  17,  17
    , 13,   9,  -3,  -7,  -7,  -8,   3,  -1
    ,  4,   7,  -6,   1,   0,  -5,  -1,  -8
    , 13,   8,   8,  10,  13,   0,   2,  -7
    ,  0,   0,   0,   0,   0,   0,   0,   0
    ]

rawMgKnightTable, rawEgKnightTable :: U.Vector Score
rawMgKnightTable = U.fromList
    [-167, -89, -34, -49,  61, -97, -15, -107
    , -73, -41,  72,  36,  23,  62,   7,  -17
    , -47,  60,  37,  65,  84, 129,  73,   44
    ,  -9,  17,  19,  53,  37,  69,  18,   22
    , -13,   4,  16,  13,  28,  19,  21,   -8
    , -23,  -9,  12,  10,  19,  17,  25,  -16
    , -29, -53, -12,  -3,  -1,  18, -14,  -19
    ,-105, -21, -58, -33, -17, -28, -19,  -23
    ]
rawEgKnightTable = U.fromList
    [ -58, -38, -13, -28, -31, -27, -63, -99
    , -25,  -8, -25,  -2,  -9, -25, -24, -52
    , -24, -20,  10,   9,  -1,  -9, -19, -41
    , -17,   3,  22,  22,  22,  11,   8, -18
    , -18,  -6,  16,  25,  16,  17,   4, -18
    , -23,  -3,  -1,  15,  10,  -3, -18, -22
    , -42, -20, -10,  -5,  -2, -20, -23, -44
    , -29, -51, -23, -15, -22, -18, -50, -64
    ]

rawMgBishopTable, rawEgBishopTable :: U.Vector Score
rawMgBishopTable = U.fromList
    [ -29,   4, -82, -37, -25, -42,   7,  -8
    , -26,  16, -18, -13,  30,  59,  18, -47
    , -16,  37,  43,  40,  35,  50,  37,  -2
    ,  -4,   5,  19,  50,  37,  37,   7,  -2
    ,  -6,  13,  13,  26,  34,  12,  10,   4
    ,   0,  15,  15,  15,  14,  27,  18,  10
    ,   4,  15,  16,   0,   7,  21,  33,   1
    , -33,  -3, -14, -21, -13, -12, -39, -21
    ]
rawEgBishopTable = U.fromList
    [ -14, -21, -11,  -8,  -7,  -9, -17, -24
    ,  -8,  -4,   7, -12, -3, -13,  -4, -14
    ,   4,   1,  16,   3,   7,  16,   4, -11
    ,  17,   9,  12,  14,  11,  10,  12,   4
    ,   3,   9,  12,   9,  14,  10,   3,   2
    ,  -4,   0,   6,  12,  13,   6,   3,  -1
    , -19,  -3,  -6,  -4,  -2, -13,  -8, -18
    , -23,  -9, -23,  -5,  -9, -16,  -5, -17
    ]

rawMgRookTable, rawEgRookTable :: U.Vector Score
rawMgRookTable = U.fromList
    [  32,  42,  32,  51,  63,   9,  31,  43
    ,  27,  32,  58,  62,  80,  67,  26,  44
    ,  -5,  19,  26,  36,  17,  45,  61,  16
    , -24, -11,   7,  26,  24,  35,  -8, -20
    , -36, -26, -12,  -1,   9,  -7,   6, -23
    , -45, -25, -16, -17,   3,   0,  -5, -33
    , -44, -16, -20,  -9,  -1,  11,  -6, -71
    , -19, -13,   1,  17,  16,   7, -37, -26
    ]
rawEgRookTable = U.fromList
    [  13,  10,  18,  15,  12,  12,   8,   5
    ,  11,  13,  13,  11,  -3,   3,  19,   7
    ,  -7,  -2,   4,   1,   0,   2,  -5, -15
    , -10, -10,   0,   3,   5,   5,  -7, -12
    , -12, -10,   5,   4,   6,   4,  -4, -22
    ,  -9,   3,   8,   6,  13,   1,  -6, -14
    , -22,  -4,   1,   6,  12,  10,   9, -14
    ,  -9,   0,   7,   2,   2,  -5, -15,  -7
    ]

rawMgQueenTable, rawEgQueenTable :: U.Vector Score
rawMgQueenTable = U.fromList
    [ -28,   0,  29,  12,  59,  44,  43,  45
    , -24, -39,  -5,   1, -16,  57,  28,  54
    , -13, -17,   7,   8,  29,  56,  47,  57
    , -27, -27, -16, -16,  -1,  17,  -2,   1
    ,  -9, -26, -19, -10,  -2,  -4,   3,  -3
    , -14,   2, -11,  -2,  -5,   2,  14,   5
    , -35,  -8,  11,   2,   8,  15,  -3,   1
    ,  -1, -18,  -9,  10, -15, -25, -31, -50
    ]
rawEgQueenTable = U.fromList
    [  -9,  22,  22,  27,  27,  19,  10,  20
    , -17,  20,  32,  41,  58,  25,  30,   0
    , -20,   6,   9,  49,  47,  35,  19,   9
    ,   3,  22,  24,  45,  57,  40,  57,  36
    , -18,  28,  19,  47,  31,  34,  39,  23
    , -16, -27,  15,   6,   9,  17,  10,   5
    , -22, -23, -30, -16, -16, -23, -36, -32
    , -33, -28, -22, -43,  -5, -32, -20, -41
    ]

rawMgKingTable, rawEgKingTable :: U.Vector Score
rawMgKingTable = U.fromList
    [ -65,  23,  16, -15, -56, -34,   2,  13
    ,  29,  -1, -20,  -7,  -8,  -4, -38, -29
    ,  -9,  24,   2, -16, -20,   6,  22, -22
    , -17, -20, -12, -27, -30, -25, -14, -36
    , -49,  -1, -27, -39, -46, -44, -33, -51
    , -14, -14, -22, -46, -44, -30, -15, -27
    ,   1,   7,  -8, -64, -43, -16,   9,   8
    , -15,  36,  12, -54,   8, -28,  24,  14
    ]
rawEgKingTable = U.fromList
    [ -74, -35, -18, -18, -11,  15,   4, -17
    , -12,  17,  14,  17,  17,  38,  23,  11
    ,  10,  17,  23,  15,  20,  45,  44,  13
    ,  -8,  22,  24,  27,  26,  33,  26,   3
    , -18,  -4,  21,  24,  27,  23,  -11, -11
    , -19,  -3,  11,  21,  23,  16,   7,  -9
    , -27, -11,   4,  13,  14,   4,  -5, -17
    , -53, -34, -21, -11, -28, -14, -24, -43
    ]

-- Packed Tables (White = flip raw, Black = raw)
packedPawnTable, packedKnightTable, packedBishopTable, packedRookTable, packedQueenTable, packedKingTable :: U.Vector PackedScore
packedPawnTable   = U.zipWith packScore (flipTable rawMgPawnTable) (flipTable rawEgPawnTable)
packedKnightTable = U.zipWith packScore (flipTable rawMgKnightTable) (flipTable rawEgKnightTable)
packedBishopTable = U.zipWith packScore (flipTable rawMgBishopTable) (flipTable rawEgBishopTable)
packedRookTable   = U.zipWith packScore (flipTable rawMgRookTable) (flipTable rawEgRookTable)
packedQueenTable  = U.zipWith packScore (flipTable rawMgQueenTable) (flipTable rawEgQueenTable)
packedKingTable   = U.zipWith packScore (flipTable rawMgKingTable) (flipTable rawEgKingTable)

packedPawnTableFlip, packedKnightTableFlip, packedBishopTableFlip, packedRookTableFlip, packedQueenTableFlip, packedKingTableFlip :: U.Vector PackedScore
packedPawnTableFlip   = U.zipWith packScore rawMgPawnTable   rawEgPawnTable
packedKnightTableFlip = U.zipWith packScore rawMgKnightTable rawEgKnightTable
packedBishopTableFlip = U.zipWith packScore rawMgBishopTable rawEgBishopTable
packedRookTableFlip   = U.zipWith packScore rawMgRookTable   rawEgRookTable
packedQueenTableFlip  = U.zipWith packScore rawMgQueenTable  rawEgQueenTable
packedKingTableFlip   = U.zipWith packScore rawMgKingTable   rawEgKingTable

-- | King Safety Table (Quadratic)
-- Indexed by attack units.
safetyTable :: U.Vector Score
safetyTable = U.generate 100 $ \i ->
    if i == 0 then 0
    else (i * i * 3) `div` 4 + 5

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

<<<<<<< HEAD
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
=======
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
>>>>>>> origin/main

        !totalUnits = vN + vB + vR + vQ

    in if totalUnits == 0 then 0 else safetyTable U.! (min 99 totalUnits)
