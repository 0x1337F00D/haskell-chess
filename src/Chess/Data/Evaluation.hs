{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Chess.Data.Evaluation where

import qualified Data.Vector.Unboxed as U
import Data.Bits (countTrailingZeros, clearBit, shiftL, shiftR, (.&.), (.|.))

import Chess.Types
import Chess.Bitboard

-- | Evaluation score in centipawns.
type Score = Int

-- | Packed Score (MG in upper 32 bits, EG in lower 32 bits).
type PackedScore = Int

-- | Pack two scores into one 64-bit integer (assuming 64-bit Int).
-- We bias EG by +65536 to ensure it is positive and doesn't carry into MG.
packScore :: Score -> Score -> PackedScore
packScore mg eg = (mg `shiftL` 32) .|. ((eg + 65536) .&. 0xFFFFFFFF)

-- | Unpack a packed score into (MG, EG).
unpackScore :: PackedScore -> (Score, Score)
unpackScore ps =
    let mg = ps `shiftR` 32
        egRaw = ps .&. 0xFFFFFFFF
        eg = egRaw - 65536
    in (mg, eg)

-- | Tapered Evaluation Parameters
totalPhase :: Int
totalPhase = 24

phasePawn, phaseKnight, phaseBishop, phaseRook, phaseQueen :: Int
phasePawn = 0
phaseKnight = 1
phaseBishop = 1
phaseRook = 2
phaseQueen = 4

-- | Get phase value for a piece type.
phaseValue :: PieceType -> Int
phaseValue Pawn   = phasePawn
phaseValue Knight = phaseKnight
phaseValue Bishop = phaseBishop
phaseValue Rook   = phaseRook
phaseValue Queen  = phaseQueen
phaseValue King   = 0

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

-- | Get material value (MG, EG) for a piece type.
materialValue :: PieceType -> (Score, Score)
materialValue Pawn   = (mgValuePawn, egValuePawn)
materialValue Knight = (mgValueKnight, egValueKnight)
materialValue Bishop = (mgValueBishop, egValueBishop)
materialValue Rook   = (mgValueRook, egValueRook)
materialValue Queen  = (mgValueQueen, egValueQueen)
materialValue King   = (0, 0)

-- | Get packed material score for a piece type.
packedMaterialValue :: PieceType -> PackedScore
packedMaterialValue pt =
    let (mg, eg) = materialValue pt
    in packScore mg eg

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

-- | Flip a PSTO table for Black (mirror ranks).
-- Transforms a White table to Black table.
flipTable :: U.Vector Score -> U.Vector Score
flipTable v = U.generate 64 $ \i ->
    let r = i `div` 8
        f = i `mod` 8
    in v U.! ((7 - r) * 8 + f)

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

-- | Get Packed PST Score for a piece at a square.
-- Note: White tables are already flipped from raw (Black) tables.
-- Raw tables are from Black's perspective if mapped 0=A1..63=H8 (with Rank 8 first in data).
-- But wait, the raw tables data:
-- Rank 8, Rank 7... Rank 1.
-- Index 0 is A8 (if standard iteration).
-- But standard iteration [0..63] usually maps A1..H8.
-- If `U.fromList` takes a list where first element is index 0.
-- And index 0 is A1.
-- Then the list should be ordered Rank 1 to Rank 8.
-- But the list provided is likely printed visually (Rank 8 first).
-- Let's check `Evaluation.hs` comment:
-- "Because we use fromList, index 0 corresponds to A1 if we listed Rank 1 first.
-- But the data is Rank 8 first. So index 0 gets Rank 8 data.
-- This effectively creates a vertically flipped board (Black perspective)."
-- So `rawMgPawnTable` has index 0 (A1) = value from Rank 8 of data.
-- So `rawMgPawnTable` IS flipped (Black).
-- `flipTable` flips it back to White (Rank 1 at A1).
-- So `packedPawnTable` (White) uses `flipTable`.
-- `packedPawnTableFlip` (Black) uses raw.
-- This seems correct.

pstValue :: Color -> PieceType -> Square -> PackedScore
pstValue White pt (Square i) = case pt of
    Pawn   -> packedPawnTable   `U.unsafeIndex` i
    Knight -> packedKnightTable `U.unsafeIndex` i
    Bishop -> packedBishopTable `U.unsafeIndex` i
    Rook   -> packedRookTable   `U.unsafeIndex` i
    Queen  -> packedQueenTable  `U.unsafeIndex` i
    King   -> packedKingTable   `U.unsafeIndex` i
pstValue Black pt (Square i) = case pt of
    Pawn   -> packedPawnTableFlip   `U.unsafeIndex` i
    Knight -> packedKnightTableFlip `U.unsafeIndex` i
    Bishop -> packedBishopTableFlip `U.unsafeIndex` i
    Rook   -> packedRookTableFlip   `U.unsafeIndex` i
    Queen  -> packedQueenTableFlip  `U.unsafeIndex` i
    King   -> packedKingTableFlip   `U.unsafeIndex` i

-- | King Safety Table (Quadratic)
-- Indexed by attack units.
safetyTable :: U.Vector Score
safetyTable = U.generate 100 $ \i ->
    if i == 0 then 0
    else (i * i * 3) `div` 4 + 5

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
