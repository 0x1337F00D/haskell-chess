{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE BangPatterns #-}
module Chess.Bitboard where

import Data.Bits
import Data.Word (Word64)
import Data.List (foldl')
import qualified Data.Vector.Unboxed as U

import Chess.Types (Square(..), Color(..), squares)

-- | Bitboard is a 64-bit value representing a set of squares.
type Bitboard = Word64

pattern BB_EMPTY :: Bitboard
pattern BB_EMPTY = 0x0000000000000000

pattern BB_ALL :: Bitboard
pattern BB_ALL = 0xffffffffffffffff

-- Square bitboard constants --------------------------------------------------
-- The least significant bit is A1 and the most significant is H8.

pattern BB_A1, BB_B1, BB_C1, BB_D1, BB_E1, BB_F1, BB_G1, BB_H1 :: Bitboard
pattern BB_A1 = 0x0000000000000001
pattern BB_B1 = 0x0000000000000002
pattern BB_C1 = 0x0000000000000004
pattern BB_D1 = 0x0000000000000008
pattern BB_E1 = 0x0000000000000010
pattern BB_F1 = 0x0000000000000020
pattern BB_G1 = 0x0000000000000040
pattern BB_H1 = 0x0000000000000080

pattern BB_A2, BB_B2, BB_C2, BB_D2, BB_E2, BB_F2, BB_G2, BB_H2 :: Bitboard
pattern BB_A2 = 0x0000000000000100
pattern BB_B2 = 0x0000000000000200
pattern BB_C2 = 0x0000000000000400
pattern BB_D2 = 0x0000000000000800
pattern BB_E2 = 0x0000000000001000
pattern BB_F2 = 0x0000000000002000
pattern BB_G2 = 0x0000000000004000
pattern BB_H2 = 0x0000000000008000

pattern BB_A3, BB_B3, BB_C3, BB_D3, BB_E3, BB_F3, BB_G3, BB_H3 :: Bitboard
pattern BB_A3 = 0x0000000000010000
pattern BB_B3 = 0x0000000000020000
pattern BB_C3 = 0x0000000000040000
pattern BB_D3 = 0x0000000000080000
pattern BB_E3 = 0x0000000000100000
pattern BB_F3 = 0x0000000000200000
pattern BB_G3 = 0x0000000000400000
pattern BB_H3 = 0x0000000000800000

pattern BB_A4, BB_B4, BB_C4, BB_D4, BB_E4, BB_F4, BB_G4, BB_H4 :: Bitboard
pattern BB_A4 = 0x0000000001000000
pattern BB_B4 = 0x0000000002000000
pattern BB_C4 = 0x0000000004000000
pattern BB_D4 = 0x0000000008000000
pattern BB_E4 = 0x0000000010000000
pattern BB_F4 = 0x0000000020000000
pattern BB_G4 = 0x0000000040000000
pattern BB_H4 = 0x0000000080000000

pattern BB_A5, BB_B5, BB_C5, BB_D5, BB_E5, BB_F5, BB_G5, BB_H5 :: Bitboard
pattern BB_A5 = 0x0000000100000000
pattern BB_B5 = 0x0000000200000000
pattern BB_C5 = 0x0000000400000000
pattern BB_D5 = 0x0000000800000000
pattern BB_E5 = 0x0000001000000000
pattern BB_F5 = 0x0000002000000000
pattern BB_G5 = 0x0000004000000000
pattern BB_H5 = 0x0000008000000000

pattern BB_A6, BB_B6, BB_C6, BB_D6, BB_E6, BB_F6, BB_G6, BB_H6 :: Bitboard
pattern BB_A6 = 0x0000010000000000
pattern BB_B6 = 0x0000020000000000
pattern BB_C6 = 0x0000040000000000
pattern BB_D6 = 0x0000080000000000
pattern BB_E6 = 0x0000100000000000
pattern BB_F6 = 0x0000200000000000
pattern BB_G6 = 0x0000400000000000
pattern BB_H6 = 0x0000800000000000

pattern BB_A7, BB_B7, BB_C7, BB_D7, BB_E7, BB_F7, BB_G7, BB_H7 :: Bitboard
pattern BB_A7 = 0x0001000000000000
pattern BB_B7 = 0x0002000000000000
pattern BB_C7 = 0x0004000000000000
pattern BB_D7 = 0x0008000000000000
pattern BB_E7 = 0x0010000000000000
pattern BB_F7 = 0x0020000000000000
pattern BB_G7 = 0x0040000000000000
pattern BB_H7 = 0x0080000000000000

pattern BB_A8, BB_B8, BB_C8, BB_D8, BB_E8, BB_F8, BB_G8, BB_H8 :: Bitboard
pattern BB_A8 = 0x0100000000000000
pattern BB_B8 = 0x0200000000000000
pattern BB_C8 = 0x0400000000000000
pattern BB_D8 = 0x0800000000000000
pattern BB_E8 = 0x1000000000000000
pattern BB_F8 = 0x2000000000000000
pattern BB_G8 = 0x4000000000000000
pattern BB_H8 = 0x8000000000000000

bbSquares :: [Bitboard]
bbSquares =
  [ BB_A1, BB_B1, BB_C1, BB_D1, BB_E1, BB_F1, BB_G1, BB_H1
  , BB_A2, BB_B2, BB_C2, BB_D2, BB_E2, BB_F2, BB_G2, BB_H2
  , BB_A3, BB_B3, BB_C3, BB_D3, BB_E3, BB_F3, BB_G3, BB_H3
  , BB_A4, BB_B4, BB_C4, BB_D4, BB_E4, BB_F4, BB_G4, BB_H4
  , BB_A5, BB_B5, BB_C5, BB_D5, BB_E5, BB_F5, BB_G5, BB_H5
  , BB_A6, BB_B6, BB_C6, BB_D6, BB_E6, BB_F6, BB_G6, BB_H6
  , BB_A7, BB_B7, BB_C7, BB_D7, BB_E7, BB_F7, BB_G7, BB_H7
  , BB_A8, BB_B8, BB_C8, BB_D8, BB_E8, BB_F8, BB_G8, BB_H8
  ]

-- Predefined square masks ----------------------------------------------------

bbCorners :: Bitboard
bbCorners = BB_A1 .|. BB_H1 .|. BB_A8 .|. BB_H8

bbCenter :: Bitboard
bbCenter = BB_D4 .|. BB_E4 .|. BB_D5 .|. BB_E5

bbLightSquares :: Bitboard
bbLightSquares = foldl' (.|.) 0
  [bb | (i, bb) <- zip ([0..] :: [Int]) bbSquares, even ((i `div` 8) + (i `mod` 8))]

bbDarkSquares :: Bitboard
bbDarkSquares = foldl' (.|.) 0
  [bb | (i, bb) <- zip ([0..] :: [Int]) bbSquares, odd ((i `div` 8) + (i `mod` 8))]

-- File and rank masks --------------------------------------------------------

bbFileA, bbFileB, bbFileC, bbFileD,
  bbFileE, bbFileF, bbFileG, bbFileH :: Bitboard
bbFileA = foldl' (.|.) 0 [bbSquares !! (0 + 8*r) | r <- [0..7]]
bbFileB = foldl' (.|.) 0 [bbSquares !! (1 + 8*r) | r <- [0..7]]
bbFileC = foldl' (.|.) 0 [bbSquares !! (2 + 8*r) | r <- [0..7]]
bbFileD = foldl' (.|.) 0 [bbSquares !! (3 + 8*r) | r <- [0..7]]
bbFileE = foldl' (.|.) 0 [bbSquares !! (4 + 8*r) | r <- [0..7]]
bbFileF = foldl' (.|.) 0 [bbSquares !! (5 + 8*r) | r <- [0..7]]
bbFileG = foldl' (.|.) 0 [bbSquares !! (6 + 8*r) | r <- [0..7]]
bbFileH = foldl' (.|.) 0 [bbSquares !! (7 + 8*r) | r <- [0..7]]

bbFiles :: [Bitboard]
bbFiles =
  [ bbFileA, bbFileB, bbFileC, bbFileD
  , bbFileE, bbFileF, bbFileG, bbFileH
  ]

bbRank1, bbRank2, bbRank3, bbRank4,
  bbRank5, bbRank6, bbRank7, bbRank8 :: Bitboard
bbRank1 = foldl' (.|.) 0 [bbSquares !! (8*0 + f) | f <- [0..7]]
bbRank2 = foldl' (.|.) 0 [bbSquares !! (8*1 + f) | f <- [0..7]]
bbRank3 = foldl' (.|.) 0 [bbSquares !! (8*2 + f) | f <- [0..7]]
bbRank4 = foldl' (.|.) 0 [bbSquares !! (8*3 + f) | f <- [0..7]]
bbRank5 = foldl' (.|.) 0 [bbSquares !! (8*4 + f) | f <- [0..7]]
bbRank6 = foldl' (.|.) 0 [bbSquares !! (8*5 + f) | f <- [0..7]]
bbRank7 = foldl' (.|.) 0 [bbSquares !! (8*6 + f) | f <- [0..7]]
bbRank8 = foldl' (.|.) 0 [bbSquares !! (8*7 + f) | f <- [0..7]]

bbRanks :: [Bitboard]
bbRanks =
  [ bbRank1, bbRank2, bbRank3, bbRank4
  , bbRank5, bbRank6, bbRank7, bbRank8
  ]

bbBackranks :: Bitboard
bbBackranks = bbRank1 .|. bbRank8

-- Bitboard operations --------------------------------------------------------

-- | Index of least significant 1 bit, if any.
lsb :: Bitboard -> Maybe Int
lsb 0 = Nothing
lsb bb = Just (countTrailingZeros bb)

-- | Indices of bits in ascending order.
scanForward :: Bitboard -> [Int]
scanForward bb
  | bb == 0   = []
  | otherwise = let i = countTrailingZeros bb
                    bb' = clearBit bb i
                in i : scanForward bb'

-- | Map a function over the set bits of a bitboard.
-- Avoids intermediate list allocation of squares.
mapBitboard :: (Square -> a) -> Bitboard -> [a]
{-# INLINE mapBitboard #-}
mapBitboard f = go
  where
    go 0 = []
    go bb =
        let i = countTrailingZeros bb
            bb' = clearBit bb i
        in f (Square i) : go bb'

-- | Map a function returning a list over the set bits of a bitboard and concat.
-- Avoids intermediate list allocation of squares.
flatMapBitboard :: (Square -> [a]) -> Bitboard -> [a]
{-# INLINE flatMapBitboard #-}
flatMapBitboard f = go
  where
    go 0 = []
    go bb =
        let i = countTrailingZeros bb
            bb' = clearBit bb i
        in f (Square i) ++ go bb'

-- | Fold a function over the set bits of a bitboard.
foldBitboard :: (a -> Square -> a) -> a -> Bitboard -> a
{-# INLINE foldBitboard #-}
foldBitboard f z bb = go z bb
  where
    go !acc 0 = acc
    go !acc b =
        let i = countTrailingZeros b
            b' = clearBit b i
        in go (f acc (Square i)) b'

-- | Monadic fold over the set bits of a bitboard.
foldBitboardM :: Monad m => (a -> Square -> m a) -> a -> Bitboard -> m a
{-# INLINE foldBitboardM #-}
foldBitboardM f z bb = go z bb
  where
    go !acc 0 = return acc
    go !acc b = do
        let i = countTrailingZeros b
            b' = clearBit b i
        acc' <- f acc (Square i)
        go acc' b'

-- | Index of most significant 1 bit, if any.
msb :: Bitboard -> Maybe Int
msb 0 = Nothing
msb bb = Just (63 - countLeadingZeros bb)

-- | Indices of bits in descending order.
scanReversed :: Bitboard -> [Int]
scanReversed bb
  | bb == 0   = []
  | otherwise = let i = 63 - countLeadingZeros bb
                    bb' = clearBit bb i
                in i : scanReversed bb'

-- | Population count of set bits.
popcount :: Bitboard -> Int
popcount = popCount

-- Geometric transforms -------------------------------------------------------

-- | Flip the board vertically (swap ranks).
flipVertical :: Bitboard -> Bitboard
flipVertical bb = foldl' setBitIf 0 [0..63]
  where
    setBitIf acc i = if testBit bb i
                        then setBit acc ((7 - (i `div` 8)) * 8 + (i `mod` 8))
                        else acc

-- | Flip the board horizontally (swap files).
flipHorizontal :: Bitboard -> Bitboard
flipHorizontal bb = foldl' setBitIf 0 [0..63]
  where
    setBitIf acc i = if testBit bb i
                        then setBit acc ((i `div` 8) * 8 + (7 - (i `mod` 8)))
                        else acc

-- | Flip the board along the main diagonal.
flipDiagonal :: Bitboard -> Bitboard
flipDiagonal bb = foldl' setBitIf 0 [0..63]
  where
    setBitIf acc i = if testBit bb i
                        then let r = i `div` 8
                                 f = i `mod` 8
                             in setBit acc (f*8 + r)
                        else acc

-- | Flip the board along the anti-diagonal.
flipAntiDiagonal :: Bitboard -> Bitboard
flipAntiDiagonal bb = foldl' setBitIf 0 [0..63]
  where
    setBitIf acc i = if testBit bb i
                        then let r = i `div` 8
                                 f = i `mod` 8
                             in setBit acc ((7-f)*8 + (7-r))
                        else acc

-- Shift operations -----------------------------------------------------------

shiftDown, shift2Down, shiftUp, shift2Up :: Bitboard -> Bitboard
shiftLeft, shiftRight :: Bitboard -> Bitboard
shiftDownLeft, shiftDownRight, shiftUpLeft, shiftUpRight :: Bitboard -> Bitboard

shiftDown bb     = bb `shiftR` 8
shift2Down bb    = bb `shiftR` 16
shiftUp bb       = (bb `shiftL` 8) .&. BB_ALL
shift2Up bb      = (bb `shiftL` 16) .&. BB_ALL
shiftLeft bb     = (bb `shiftL` 1) .&. complement bbFileA
shiftRight bb    = (bb `shiftR` 1) .&. complement bbFileH
shiftDownLeft bb = shiftDown bb .&. complement bbFileH
shiftDownRight bb= shiftDown bb .&. complement bbFileA
shiftUpLeft bb   = shiftUp bb   .&. complement bbFileH
shiftUpRight bb  = shiftUp bb   .&. complement bbFileA

-- Helpers -------------------------------------------------------------------

bbFromSquare :: Square -> Bitboard
bbFromSquare (Square i) = 1 `shiftL` i

-- Attack tables --------------------------------------------------------------

-- | Generate knight attacks from a square using coordinate offsets.
knightAttacksFrom :: Square -> Bitboard
knightAttacksFrom (Square n) = foldl' (.|.) 0 [ bbFromSquare (Square idx)
                                              | (df,dr) <- deltas
                                              , let f = file + df
                                                    r = rank + dr
                                              , f >= 0, f < 8, r >= 0, r < 8
                                              , let idx = r*8 + f ]
  where
    file = n `mod` 8
    rank = n `div` 8
    deltas = [ (1,2), (2,1), (2,-1), (1,-2)
             , (-1,-2), (-2,-1), (-2,1), (-1,2) ]

-- | Generate king attacks from a square.
kingAttacksFrom :: Square -> Bitboard
kingAttacksFrom (Square n) = foldl' (.|.) 0 [ bbFromSquare (Square idx)
                                             | (df,dr) <- deltas
                                             , let f = file + df
                                                   r = rank + dr
                                             , f >= 0, f < 8, r >= 0, r < 8
                                             , let idx = r*8 + f ]
  where
    file = n `mod` 8
    rank = n `div` 8
    deltas = [ (1,1), (1,0), (1,-1)
             , (0,1),         (0,-1)
             , (-1,1), (-1,0), (-1,-1) ]

-- | Generate pawn attacks given a color from a square.
pawnAttacksFrom :: Color -> Square -> Bitboard
pawnAttacksFrom col (Square n) = foldl' (.|.) 0 [ bbFromSquare (Square idx)
                                                 | (df,dr) <- deltas
                                                 , let f = file + df
                                                       r = rank + dr
                                                 , f >= 0, f < 8, r >= 0, r < 8
                                                 , let idx = r*8 + f ]
  where
    file = n `mod` 8
    rank = n `div` 8
    deltas = case col of
               White -> [(-1,1),(1,1)]
               Black -> [(-1,-1),(1,-1)]

-- Precomputed attack arrays

bbKnightAttacks :: U.Vector Bitboard
bbKnightAttacks = U.fromList $ map knightAttacksFrom squares

bbKingAttacks :: U.Vector Bitboard
bbKingAttacks = U.fromList $ map kingAttacksFrom squares

bbWhitePawnAttacks :: U.Vector Bitboard
bbWhitePawnAttacks = U.fromList $ map (pawnAttacksFrom White) squares

bbBlackPawnAttacks :: U.Vector Bitboard
bbBlackPawnAttacks = U.fromList $ map (pawnAttacksFrom Black) squares

-- | Lookup knight attacks for a square.
knightAttacks :: Square -> Bitboard
knightAttacks (Square i) = bbKnightAttacks `U.unsafeIndex` i

-- | Lookup king attacks for a square.
kingAttacks :: Square -> Bitboard
kingAttacks (Square i) = bbKingAttacks `U.unsafeIndex` i

-- | Lookup pawn attacks for a color and square.
pawnAttacks :: Color -> Square -> Bitboard
pawnAttacks White (Square i) = bbWhitePawnAttacks `U.unsafeIndex` i
pawnAttacks Black (Square i) = bbBlackPawnAttacks `U.unsafeIndex` i

-- Sliding Attack Generators --------------------------------------------------

-- Directions indices
-- 0: N (+8)
-- 1: S (-8)
-- 2: E (+1)
-- 3: W (-1)
-- 4: NE (+9)
-- 5: NW (+7)
-- 6: SE (-7)
-- 7: SW (-9)

-- | Precomputed rays for every square and 8 directions.
-- Index = square * 8 + directionIndex
bbRays :: U.Vector Bitboard
bbRays = U.generate (64 * 8) $ \i ->
    let sq = Square (i `div` 8)
        dirIdx = i `mod` 8
        (df, dr) = case dirIdx of
            0 -> (0, 1)   -- N
            1 -> (0, -1)  -- S
            2 -> (1, 0)   -- E
            3 -> (-1, 0)  -- W
            4 -> (1, 1)   -- NE
            5 -> (-1, 1)  -- NW
            6 -> (1, -1)  -- SE
            7 -> (-1, -1) -- SW
            _ -> (0, 0)
    in generateRay sq df dr

generateRay :: Square -> Int -> Int -> Bitboard
generateRay (Square i) df dr = go (f+df) (r+dr) 0
  where
    f = i `mod` 8
    r = i `div` 8
    go cf cr acc
      | cf < 0 || cf > 7 || cr < 0 || cr > 7 = acc
      | otherwise = go (cf+df) (cr+dr) (setBit acc (cr*8 + cf))

-- | Get attacks for a sliding piece in a specific direction.
-- Marked INLINE to allow constant folding of 'dirIdx' when called from bishopAttacks/rookAttacks.
-- This removes runtime checks and allows fusion of bitwise operations.
getRayAttacks :: Square -> Int -> Bitboard -> Bitboard
{-# INLINE getRayAttacks #-}
getRayAttacks (Square sq) dirIdx occ =
    let mask = bbRays `U.unsafeIndex` (sq * 8 + dirIdx)
        blockers = mask .&. occ
    in if blockers == 0
       then mask
       else let b = if dirIdx `elem` [0, 2, 4, 5] -- Positive directions (N, E, NE, NW)
                    then countTrailingZeros blockers -- lsb
                    else 63 - countLeadingZeros blockers -- msb
                blockerMask = bbRays `U.unsafeIndex` (b * 8 + dirIdx)
            in mask `xor` blockerMask

-- | Generate bishop attacks (diagonal).
bishopAttacks :: Square -> Bitboard -> Bitboard
{-# INLINE bishopAttacks #-}
bishopAttacks sq occ =
    getRayAttacks sq 4 occ .|.
    getRayAttacks sq 5 occ .|.
    getRayAttacks sq 6 occ .|.
    getRayAttacks sq 7 occ

-- | Generate rook attacks (orthogonal).
rookAttacks :: Square -> Bitboard -> Bitboard
{-# INLINE rookAttacks #-}
rookAttacks sq occ =
    getRayAttacks sq 0 occ .|.
    getRayAttacks sq 1 occ .|.
    getRayAttacks sq 2 occ .|.
    getRayAttacks sq 3 occ

-- Rays ----------------------------------------------------------------------

-- | Precomputed rays between all pairs of squares.
-- Index = from * 64 + to
bbRaysBetween :: U.Vector Bitboard
bbRaysBetween = U.generate (64 * 64) $ \i ->
    let from = Square (i `div` 64)
        to   = Square (i `mod` 64)
    in rayInit from to

-- | Helper to generate ray (logic from original ray function)
rayInit :: Square -> Square -> Bitboard
rayInit a@(Square ai) b@(Square bi)
  | a == b = 0
  | abs df == abs dr || df == 0 || dr == 0 = go (fileA+dfSign) (rankA+drSign) 0
  | otherwise = 0
  where
    fileA = ai `mod` 8
    rankA = ai `div` 8
    fileB = bi `mod` 8
    rankB = bi `div` 8
    df = fileB - fileA
    dr = rankB - rankA
    dfSign = signum df
    drSign = signum dr

    go f r acc
      | f == fileB && r == rankB = acc .|. bbFromSquare b
      | f < 0 || f > 7 || r < 0 || r > 7 = 0
      | otherwise =
          let acc' = acc .|. bbFromSquare (Square (r*8 + f))
          in go (f+dfSign) (r+drSign) acc'

-- | Bitboard of squares in a ray from one square to another, including the
-- target but excluding the origin. Zero if not aligned.
ray :: Square -> Square -> Bitboard
{-# INLINE ray #-}
ray (Square from) (Square to) = bbRaysBetween `U.unsafeIndex` (from * 64 + to)

-- | Squares strictly between two aligned squares.
between :: Square -> Square -> Bitboard
between a b = case ray a b of
                0 -> 0
                bb -> bb `clearBit` (unSquare b)
