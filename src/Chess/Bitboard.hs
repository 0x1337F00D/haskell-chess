{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Chess.Bitboard where

import Data.Bits
import Data.Word (Word64)
import Data.List (foldl')
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as M
import System.IO.Unsafe (unsafePerformIO)
import Control.Monad (forM, when, liftM)

import Chess.Types (Square(..), Color(..), squares)

-- | Bitboard is a 64-bit value representing a set of squares.
type Bitboard = Word64

pattern BB_EMPTY :: Bitboard
pattern BB_EMPTY = 0x0000000000000000

-- | Bitwise AND NOT.
(.&~.) :: Bitboard -> Bitboard -> Bitboard
{-# INLINE (.&~.) #-}
x .&~. y = x .&. complement y
infixl 7 .&~.

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

-- | Index of least significant 1 bit. Total function, caller must ensure non-zero input.
lsbTotal :: Bitboard -> Int
{-# INLINE lsbTotal #-}
lsbTotal = countTrailingZeros

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

-- | Index of most significant 1 bit. Total function, caller must ensure non-zero input.
msbTotal :: Bitboard -> Int
{-# INLINE msbTotal #-}
msbTotal bb = 63 - countLeadingZeros bb

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

-- | Check if the bitboard has more than 5 bits set.
-- Faster than `popCount bb > 5` by unrolling Brian Kernighan's algorithm.
moreThan5 :: Bitboard -> Bool
{-# INLINE moreThan5 #-}
moreThan5 x =
    let x1 = x .&. (x - 1)
        x2 = x1 .&. (x1 - 1)
        x3 = x2 .&. (x2 - 1)
        x4 = x3 .&. (x3 - 1)
        x5 = x4 .&. (x4 - 1)
    in x5 /= 0

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
shiftLeft bb     = (bb `shiftL` 1) .&~. bbFileA
shiftRight bb    = (bb `shiftR` 1) .&~. bbFileH
shiftDownLeft bb = shiftDown bb .&~. bbFileH
shiftDownRight bb= shiftDown bb .&~. bbFileA
shiftUpLeft bb   = shiftUp bb   .&~. bbFileH
shiftUpRight bb  = shiftUp bb   .&~. bbFileA

-- Helpers -------------------------------------------------------------------

bbFromSquare :: Square -> Bitboard
bbFromSquare (Square i) = 1 `shiftL` i

-- Attack tables --------------------------------------------------------------

-- | Generate knight attacks from a square using coordinate offsets.
knightAttacksFrom :: Square -> Bitboard
knightAttacksFrom (Square n)
  | n >= 0 && n < 64 = knightAttacksArray `U.unsafeIndex` n
  | otherwise        = BB_EMPTY

{-# NOINLINE knightAttacksArray #-}
knightAttacksArray :: U.Vector Bitboard
knightAttacksArray = U.generate 64 gen
  where
    gen n = foldl' (.|.) 0 [ bbFromSquare (Square idx)
                           | (df,dr) <- deltas
                           , let f = (n `mod` 8) + df
                                 r = (n `div` 8) + dr
                           , f >= 0, f < 8, r >= 0, r < 8
                           , let idx = r*8 + f ]
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

-- | Xorshift64 RNG
xorShift64 :: Word64 -> Word64
xorShift64 x =
  let x1 = x `xor` (x `shiftR` 12)
      x2 = x1 `xor` (x1 `shiftL` 25)
      x3 = x2 `xor` (x2 `shiftR` 27)
  in x3 * 0x2545F4914F6CDD1D

-- | Get attacks for a sliding piece in a specific direction.
-- Slow version used for magic bitboard generation.
getRayAttacksSlow :: Square -> Int -> Bitboard -> Bitboard
getRayAttacksSlow (Square sq) dirIdx occ =
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
-- Magic Bitboards -----------------------------------------------------------

data Magic = Magic
    { mMask   :: !Bitboard
    , mMagic  :: !Word64
    , mShift  :: !Int
    , mOffset :: !Int     -- Offset into the global attack table
    } deriving (Show)

-- Unbox Instances for Magic
newtype instance U.MVector s Magic = MV_Magic (U.MVector s (Word64, Word64, Int, Int))
newtype instance U.Vector    Magic = V_Magic  (U.Vector    (Word64, Word64, Int, Int))

instance U.Unbox Magic

instance M.MVector U.MVector Magic where
    {-# INLINE basicLength #-}
    {-# INLINE basicUnsafeSlice #-}
    {-# INLINE basicOverlaps #-}
    {-# INLINE basicUnsafeNew #-}
    {-# INLINE basicInitialize #-}
    {-# INLINE basicUnsafeReplicate #-}
    {-# INLINE basicUnsafeRead #-}
    {-# INLINE basicUnsafeWrite #-}
    {-# INLINE basicClear #-}
    {-# INLINE basicSet #-}
    {-# INLINE basicUnsafeCopy #-}
    {-# INLINE basicUnsafeMove #-}
    {-# INLINE basicUnsafeGrow #-}
    basicLength (MV_Magic v) = M.basicLength v
    basicUnsafeSlice i n (MV_Magic v) = MV_Magic (M.basicUnsafeSlice i n v)
    basicOverlaps (MV_Magic v1) (MV_Magic v2) = M.basicOverlaps v1 v2
    basicUnsafeNew n = MV_Magic `liftM` M.basicUnsafeNew n
    basicInitialize (MV_Magic v) = M.basicInitialize v
    basicUnsafeReplicate n (Magic m g s o) = MV_Magic `liftM` M.basicUnsafeReplicate n (m, g, s, o)
    basicUnsafeRead (MV_Magic v) i = do
        (m, g, s, o) <- M.basicUnsafeRead v i
        return (Magic m g s o)
    basicUnsafeWrite (MV_Magic v) i (Magic m g s o) = M.basicUnsafeWrite v i (m, g, s, o)
    basicClear (MV_Magic v) = M.basicClear v
    basicSet (MV_Magic v) (Magic m g s o) = M.basicSet v (m, g, s, o)
    basicUnsafeCopy (MV_Magic v1) (MV_Magic v2) = M.basicUnsafeCopy v1 v2
    basicUnsafeMove (MV_Magic v1) (MV_Magic v2) = M.basicUnsafeMove v1 v2
    basicUnsafeGrow (MV_Magic v) n = MV_Magic `liftM` M.basicUnsafeGrow v n

instance G.Vector U.Vector Magic where
    {-# INLINE basicUnsafeFreeze #-}
    {-# INLINE basicUnsafeThaw #-}
    {-# INLINE basicLength #-}
    {-# INLINE basicUnsafeSlice #-}
    {-# INLINE basicUnsafeIndexM #-}
    {-# INLINE basicUnsafeCopy #-}
    {-# INLINE elemseq #-}
    basicUnsafeFreeze (MV_Magic v) = V_Magic `liftM` G.basicUnsafeFreeze v
    basicUnsafeThaw (V_Magic v) = MV_Magic `liftM` G.basicUnsafeThaw v
    basicLength (V_Magic v) = G.basicLength v
    basicUnsafeSlice i n (V_Magic v) = V_Magic (G.basicUnsafeSlice i n v)
    basicUnsafeIndexM (V_Magic v) i = do
        (m, g, s, o) <- G.basicUnsafeIndexM v i
        return (Magic m g s o)
    basicUnsafeCopy (MV_Magic mv) (V_Magic v) = G.basicUnsafeCopy mv v
    elemseq _ (Magic m g s o) z = G.elemseq (undefined :: U.Vector Word64) m
                               $ G.elemseq (undefined :: U.Vector Word64) g
                               $ G.elemseq (undefined :: U.Vector Int) s
                               $ G.elemseq (undefined :: U.Vector Int) o z

-- | Generate occupancy from index and mask.
-- Iterates over set bits in mask. If bit n of index is set, set the n-th set bit of mask.
getOccupancy :: Int -> Bitboard -> Bitboard
getOccupancy idx mask = go idx mask 0
  where
    go _ 0 acc = acc
    go i m acc =
        let b = countTrailingZeros m
            m' = clearBit m b
            acc' = if testBit i 0 then setBit acc b else acc
        in go (i `shiftR` 1) m' acc'

-- | Try to find a magic number for a square and mask.
findMagic :: Square -> Bitboard -> (Square -> Bitboard -> Bitboard) -> IO (Maybe (Magic, U.Vector Bitboard))
findMagic sq mask attackFn = do
    let bits = popcount mask
        size = 1 `shiftL` bits
        -- Use unboxed vectors for O(1) access during magic verification
        occs = U.generate size (`getOccupancy` mask)
        attacks = U.map (attackFn sq) occs

    table <- UM.replicate size 0

    -- Try random numbers
    let check :: Word64 -> Int -> Int -> Bool -> IO Bool
        check magic sh idx valid = do
           if idx >= size then return valid
           else do
               let occ = occs `U.unsafeIndex` idx
                   att = attacks `U.unsafeIndex` idx
                   magicIdx = (fromIntegral ((occ * magic) `unsafeShiftR` sh)) :: Int

               existing <- UM.read table magicIdx
               if existing == 0 then do
                   UM.write table magicIdx att
                   check magic sh (idx + 1) True
               else if existing /= att then
                   return False -- Collision with different attack set
               else
                   check magic sh (idx + 1) True -- Collision with same attack set (okay)

    let attempt :: Int -> Word64 -> IO (Maybe (Magic, U.Vector Bitboard))
        attempt 0 _ = return Nothing
        attempt n seed = do
            let s1 = xorShift64 seed
                s2 = xorShift64 s1
                s3 = xorShift64 s2
                magic = s1 .&. s2 .&. s3 -- Sparse magic
                sh = 64 - bits

            UM.set table 0
            valid <- check magic sh 0 True

            if valid then do
                frozen <- U.freeze table
                return $ Just (Magic mask magic sh 0, frozen)
            else
                attempt (n-1) s3

    attempt 10000000 (fromIntegral (unSquare sq) * 0x9e3779b97f4a7c15 + 0xDEADBEEF)

-- | Initialize magic tables.
initMagics :: Bool -> IO (U.Vector Magic, U.Vector Magic, U.Vector Bitboard)
initMagics verbose = do
    when verbose $ putStrLn "Initializing Magic Bitboards..."

    let getBishopMask sq =
          let att = getRayAttacksSlow sq 4 0 .|. getRayAttacksSlow sq 5 0 .|. getRayAttacksSlow sq 6 0 .|. getRayAttacksSlow sq 7 0
          in att .&~. (bbFileA .|. bbFileH .|. bbRank1 .|. bbRank8)

    let getRookMask sq =
            let r = unSquare sq `div` 8
                f = unSquare sq `mod` 8
                maskRank = (bbRank1 `shiftL` (r*8)) .&~. (bbFileA .|. bbFileH)
                maskFile = (bbFileA `shiftL` f) .&~. (bbRank1 .|. bbRank8)
            in maskRank .|. maskFile

    let solve isRook = do
            res <- forM squares $ \sq -> do
                let mask = if isRook then getRookMask sq else getBishopMask sq
                let attFn = if isRook
                            then (\s o -> getRayAttacksSlow s 0 o .|. getRayAttacksSlow s 1 o .|. getRayAttacksSlow s 2 o .|. getRayAttacksSlow s 3 o)
                            else (\s o -> getRayAttacksSlow s 4 o .|. getRayAttacksSlow s 5 o .|. getRayAttacksSlow s 6 o .|. getRayAttacksSlow s 7 o)

                mb <- findMagic sq mask attFn
                case mb of
                    Just (m, t) -> return (m, t)
                    Nothing -> error $ "Failed to find magic for " ++ show sq ++ " bits=" ++ show (popcount mask)
            return res

    bishopRes <- solve False
    rookRes <- solve True

    -- Flatten tables
    let (bishopMagicsList, bishopTables) = unzip bishopRes
        (rookMagicsList, rookTables) = unzip rookRes

    let startOffset = 0
    let (bishopMagics, nextOffset, allBishopTables) = foldl' (\(ms, off, tabs) (m, t) ->
            (ms ++ [m { mOffset = off }], off + U.length t, tabs ++ [t])) ([], startOffset, []) (zip bishopMagicsList bishopTables)

    let (rookMagics, _, allRookTables) = foldl' (\(ms, off, tabs) (m, t) ->
            (ms ++ [m { mOffset = off }], off + U.length t, tabs ++ [t])) ([], nextOffset, []) (zip rookMagicsList rookTables)

    let hugeTable = U.concat (allBishopTables ++ allRookTables)

    return (U.fromList bishopMagics, U.fromList rookMagics, hugeTable)

-- Global Magic Tables
{-# NOINLINE magicData #-}
magicData :: (U.Vector Magic, U.Vector Magic, U.Vector Bitboard)
magicData = unsafePerformIO (initMagics False)

{-# NOINLINE bbBishopMagics #-}
bbBishopMagics :: U.Vector Magic
bbBishopMagics = let (b, _, _) = magicData in b

{-# NOINLINE bbRookMagics #-}
bbRookMagics :: U.Vector Magic
bbRookMagics = let (_, r, _) = magicData in r

{-# NOINLINE bbMagicTable #-}
bbMagicTable :: U.Vector Bitboard
bbMagicTable = let (_, _, t) = magicData in t

-- | Magic attack lookup.
magicAttack :: Magic -> Bitboard -> Bitboard
{-# INLINE magicAttack #-}
magicAttack (Magic mask magic sh offset) occ =
    let idx = ((occ .&. mask) * magic) `unsafeShiftR` sh
    in bbMagicTable `U.unsafeIndex` (offset + fromIntegral idx)

-- | Generate bishop attacks (diagonal) using Magic Bitboards.
bishopAttacks :: Square -> Bitboard -> Bitboard
{-# INLINE bishopAttacks #-}
bishopAttacks (Square sq) occ =
    let m = bbBishopMagics `U.unsafeIndex` sq
    in magicAttack m occ

-- | Generate rook attacks (orthogonal) using Magic Bitboards.
rookAttacks :: Square -> Bitboard -> Bitboard
{-# INLINE rookAttacks #-}
rookAttacks (Square sq) occ =
    let m = bbRookMagics `U.unsafeIndex` sq
    in magicAttack m occ

-- | Get attacks for a sliding piece in a specific direction.
-- Uses Magic Bitboards by masking the full attack set.
getRayAttacks :: Square -> Int -> Bitboard -> Bitboard
{-# INLINE getRayAttacks #-}
getRayAttacks sq dirIdx occ =
    let attacks = case dirIdx of
            0 -> rookAttacks sq occ
            1 -> rookAttacks sq occ
            2 -> rookAttacks sq occ
            3 -> rookAttacks sq occ
            4 -> bishopAttacks sq occ
            5 -> bishopAttacks sq occ
            6 -> bishopAttacks sq occ
            7 -> bishopAttacks sq occ
            _ -> 0
        mask = bbRays `U.unsafeIndex` (unSquare sq * 8 + dirIdx)
    in attacks .&. mask

-- Rays ----------------------------------------------------------------------

-- | Precomputed alignment masks for orthogonal and diagonal rays.
bbOrthogonalMasks :: U.Vector Bitboard
bbOrthogonalMasks = U.generate 64 $ \i ->
    let f = i `mod` 8
        r = i `div` 8
    in ((bbRank1 `shiftL` (r*8)) .|. (bbFileA `shiftL` f)) `clearBit` i

bbDiagonalMasks :: U.Vector Bitboard
bbDiagonalMasks = U.generate 64 $ \i ->
    let sq = Square i
        att = getRayAttacksSlow sq 4 0 .|. getRayAttacksSlow sq 5 0 .|. getRayAttacksSlow sq 6 0 .|. getRayAttacksSlow sq 7 0
    in att `clearBit` i

-- | Returns true if the two squares are orthogonally aligned.
isOrthogonallyAligned :: Square -> Square -> Bool
{-# INLINE isOrthogonallyAligned #-}
isOrthogonallyAligned (Square s1) (Square s2) =
    testBit (bbOrthogonalMasks `U.unsafeIndex` s1) s2

-- | Returns true if the two squares are diagonally aligned.
isDiagonallyAligned :: Square -> Square -> Bool
{-# INLINE isDiagonallyAligned #-}
isDiagonallyAligned (Square s1) (Square s2) =
    testBit (bbDiagonalMasks `U.unsafeIndex` s1) s2

-- | Precomputed lines passing through two squares.
-- Index = s1 * 64 + s2
bbLines :: U.Vector Bitboard
bbLines = U.generate (64 * 64) $ \i ->
    let from = Square (i `shiftR` 6)
        to   = Square (i .&. 63)
    in lineInit from to

-- | Helper to generate line
lineInit :: Square -> Square -> Bitboard
lineInit a@(Square ai) b@(Square bi)
  | a == b = 0
  | abs df == abs dr || df == 0 || dr == 0 = go fileA rankA dfSign drSign 0 .|. go fileA rankA (-dfSign) (-drSign) 0
  | otherwise = 0
  where
    fileA = ai .&. 7
    rankA = ai `shiftR` 3
    fileB = bi .&. 7
    rankB = bi `shiftR` 3
    df = fileB - fileA
    dr = rankB - rankA
    dfSign = signum df
    drSign = signum dr

    go f r df' dr' acc
      | f < 0 || f > 7 || r < 0 || r > 7 = acc
      | otherwise = go (f+df') (r+dr') df' dr' (acc .|. bbFromSquare (Square (r*8 + f)))

-- | Check if three squares are collinear. Returns false if any two squares are equal.
isCollinear :: Square -> Square -> Square -> Bool
{-# INLINE isCollinear #-}
isCollinear (Square s1) (Square s2) (Square s3) =
    testBit (bbLines `U.unsafeIndex` (s1 * 64 + s2)) s3

-- | Precomputed rays between all pairs of squares.
-- Index = from * 64 + to
bbRaysBetween :: U.Vector Bitboard
bbRaysBetween = U.generate (64 * 64) $ \i ->
    let from = Square (i `shiftR` 6)
        to   = Square (i .&. 63)
    in rayInit from to

-- | Helper to generate ray (logic from original ray function)
rayInit :: Square -> Square -> Bitboard
rayInit a@(Square ai) b@(Square bi)
  | a == b = 0
  | abs df == abs dr || df == 0 || dr == 0 = go (fileA+dfSign) (rankA+drSign) 0
  | otherwise = 0
  where
    fileA = ai .&. 7
    rankA = ai `shiftR` 3
    fileB = bi .&. 7
    rankB = bi `shiftR` 3
    df = fileB - fileA
    dr = rankB - rankA
    dfSign = signum df
    drSign = signum dr

    go f r acc
      | f == fileB && r == rankB = acc
      | f < 0 || f > 7 || r < 0 || r > 7 = 0
      | otherwise =
          let acc' = acc .|. bbFromSquare (Square (r*8 + f))
          in go (f+dfSign) (r+drSign) acc'

-- | Bitboard of squares strictly between two aligned squares. Zero if not aligned.
ray :: Square -> Square -> Bitboard
{-# INLINE ray #-}
ray (Square from) (Square to) = bbRaysBetween `U.unsafeIndex` (from * 64 + to)

-- | Squares strictly between two aligned squares.
between :: Square -> Square -> Bitboard
{-# INLINE between #-}
between = ray
