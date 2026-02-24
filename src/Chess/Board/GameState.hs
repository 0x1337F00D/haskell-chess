{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

module Chess.Board.GameState where

import Data.Word (Word64)
import Data.Bits
import Chess.Types
import Chess.Bitboard

-- | Castling rights as a bitmask of the starting rook squares.
-- If a bit is set, castling using that rook is potentially allowed.
type CastlingRights = Bitboard

-- | No castling rights.
noCastling :: CastlingRights
noCastling = 0

-- | All castling rights (Standard Chess).
allCastling :: CastlingRights
allCastling = BB_A1 .|. BB_H1 .|. BB_A8 .|. BB_H8

-- | State needed to play a game, excluding piece placement.
-- Packed into a single Word64 to avoid heap allocation.
-- Layout:
--   Bit 0      (1): Turn (0=White, 1=Black)
--   Bits 1-4   (4): White Rook 1 (3 bits File, 1 bit Present)
--   Bits 5-8   (4): White Rook 2
--   Bits 9-12  (4): Black Rook 1
--   Bits 13-16 (4): Black Rook 2
--   Bits 17-23 (7): EP Square
--   Bits 24-33 (10): Halfmove Clock
--   Bits 34-49 (16): Fullmove Number
--   Bits 50-63 (14): Unused

{-# INLINE mkStatePacked #-}
mkStatePacked :: Color -> CastlingRights -> Square -> HalfmoveClock -> FullmoveNumber -> Word64
mkStatePacked t c e h f =
    (if t == Black then 1 else 0) .|.
    (packCastling c `shiftL` 1) .|.
    (fromIntegral (unSquare e) `shiftL` 17) .|.
    (fromIntegral (unHalfmoveClock h) `shiftL` 24) .|.
    (fromIntegral (unFullmoveNumber f) `shiftL` 34)

{-# INLINE unpackStatePacked #-}
unpackStatePacked :: Word64 -> (Color, CastlingRights, Square, HalfmoveClock, FullmoveNumber)
unpackStatePacked p =
    ( if testBit p 0 then Black else White
    , unpackCastling ((p `shiftR` 1) .&. 0xFFFF)
    , Square (fromIntegral ((p `shiftR` 17) .&. 0x7F))
    , HalfmoveClock (fromIntegral ((p `shiftR` 24) .&. 0x3FF))
    , FullmoveNumber (fromIntegral ((p `shiftR` 34) .&. 0xFFFF))
    )

{-# INLINE getTurn #-}
getTurn :: Word64 -> Color
getTurn p = if testBit p 0 then Black else White

{-# INLINE getCastlingRights #-}
getCastlingRights :: Word64 -> CastlingRights
getCastlingRights p = unpackCastling ((p `shiftR` 1) .&. 0xFFFF)

{-# INLINE getEpSquare #-}
getEpSquare :: Word64 -> Square
getEpSquare p = Square (fromIntegral ((p `shiftR` 17) .&. 0x7F))

{-# INLINE getHalfmoveClock #-}
getHalfmoveClock :: Word64 -> HalfmoveClock
getHalfmoveClock p = HalfmoveClock (fromIntegral ((p `shiftR` 24) .&. 0x3FF))

{-# INLINE getFullmoveNumber #-}
getFullmoveNumber :: Word64 -> FullmoveNumber
getFullmoveNumber p = FullmoveNumber (fromIntegral ((p `shiftR` 34) .&. 0xFFFF))

-- | Helper to pack castling rights into 16 bits.
-- 4 Slots (4 bits each: Present + File).
{-# INLINE packCastling #-}
packCastling :: CastlingRights -> Word64
packCastling cr =
  let w = cr .&. 0xFF -- Rank 1
      b = (cr `shiftR` 56) .&. 0xFF -- Rank 8 shifted to 0-7

      packRank r =
         let f1 = countTrailingZeros r
             r1 = clearBit r f1
             f2 = countTrailingZeros r1
         in if r == 0 then 0
            else if r1 == 0 then (1 `shiftL` 3) .|. (fromIntegral f1) -- Present + File
            else -- Two rooks (or more, we take first two)
                 let p1 = (1 `shiftL` 3) .|. (fromIntegral f1)
                     p2 = (1 `shiftL` 3) .|. (fromIntegral f2)
                 in p1 .|. (p2 `shiftL` 4)
  in packRank w .|. (packRank b `shiftL` 8)

-- | Helper to unpack castling rights from 16 bits.
{-# INLINE unpackCastling #-}
unpackCastling :: Word64 -> CastlingRights
unpackCastling w =
    let unpackSlot offset shiftVal =
            let s = (w `shiftR` offset) .&. 0xF
            in if testBit s 3 -- Present
               then bit (fromIntegral (s .&. 0x7) + shiftVal)
               else 0
    in unpackSlot 0 0 .|.     -- White Slot 1 (Rank 1, shift 0)
       unpackSlot 4 0 .|.     -- White Slot 2
       unpackSlot 8 56 .|.    -- Black Slot 1 (Rank 8, shift 56)
       unpackSlot 12 56       -- Black Slot 2

-- | Initial game state for standard chess (Packed).
initialStatePacked :: Word64
initialStatePacked = mkStatePacked White allCastling NoSquare 0 1

-- | Check if the given side has kingside castling rights.
-- For standard chess, checks H1/H8.
{-# INLINE canCastleStandardKingside #-}
canCastleStandardKingside :: Word64 -> Color -> Bool
canCastleStandardKingside p c =
    let shiftVal = if c == White then 1 else 9
        p' = p `shiftR` shiftVal
        s1 = p' .&. 0xF
        s2 = (p' `shiftR` 4) .&. 0xF
        target = 15 -- Present(8) | File 7(H)
    in s1 == target || s2 == target

-- | Check if the given side has queenside castling rights.
-- For standard chess, checks A1/A8.
{-# INLINE canCastleStandardQueenside #-}
canCastleStandardQueenside :: Word64 -> Color -> Bool
canCastleStandardQueenside p c =
    let shiftVal = if c == White then 1 else 9
        p' = p `shiftR` shiftVal
        s1 = p' .&. 0xF
        s2 = (p' `shiftR` 4) .&. 0xF
        target = 8 -- Present(8) | File 0(A)
    in s1 == target || s2 == target

-- | Remove castling rights for a color (e.g. king moved).
{-# INLINE removeColorCastlingRights #-}
removeColorCastlingRights :: Word64 -> Color -> Word64
removeColorCastlingRights p White = p .&. complement (0xFF `shiftL` 1) -- Clear White slots (8 bits at 1)
removeColorCastlingRights p Black = p .&. complement (0xFF `shiftL` 9) -- Clear Black slots (8 bits at 9)

-- | Remove castling rights for a specific rook square (e.g. rook moved or captured).
{-# INLINE removeCastlingRight #-}
removeCastlingRight :: Word64 -> Square -> Word64
removeCastlingRight p sq =
    let rank = squareRank sq
        file = squareFile sq
    in if rank == 0 -- White
       then
           let mask1 = (1 `shiftL` 3) .|. (fromIntegral file) -- Present + File
               s1 = (p `shiftR` 1) .&. 0xF
               s2 = (p `shiftR` 5) .&. 0xF

               -- If slot matches, clear Present bit (bit 3 of slot)
               -- We construct a mask to clear bit 3 if file matches
               p' = if s1 == mask1 then p `clearBit` (1 + 3) else p
               p'' = if s2 == mask1 then p' `clearBit` (5 + 3) else p'
           in p''
       else if rank == 7 -- Black
       then
           let mask1 = (1 `shiftL` 3) .|. (fromIntegral file)
               s1 = (p `shiftR` 9) .&. 0xF
               s2 = (p `shiftR` 13) .&. 0xF

               p' = if s1 == mask1 then p `clearBit` (9 + 3) else p
               p'' = if s2 == mask1 then p' `clearBit` (13 + 3) else p'
           in p''
       else p
