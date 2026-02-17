{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE BangPatterns #-}

module Chess.Board.GameState
  ( GameState(..)
  , CastlingRights
  , noCastling
  , allCastling
  , initialGameState
  , turn
  , castlingRights
  , epSquare
  , halfmoveClock
  , fullmoveNumber
  , zobristHash
  , setTurn
  , setCastlingRights
  , setEpSquare
  , setHalfmoveClock
  , setFullmoveNumber
  , setZobristHash
  , canCastleKingside
  , canCastleQueenside
  , removeColorCastlingRights
  , removeCastlingRight
  ) where

import Data.Word (Word64)
import Data.Bits
import Chess.Types
import Chess.Bitboard

-- | Castling rights as a bitmask of the starting rook squares (A1, H1, A8, H8).
-- If a bit is set, castling using that rook is potentially allowed.
type CastlingRights = Bitboard

-- | No castling rights.
noCastling :: CastlingRights
noCastling = 0

-- | All castling rights.
allCastling :: CastlingRights
allCastling = BB_A1 .|. BB_H1 .|. BB_A8 .|. BB_H8

-- | State needed to play a game, excluding piece placement.
-- Packed Layout:
-- Bits 0-0   (1) : Turn (0=White, 1=Black)
-- Bits 1-4   (4) : Castling Rights (A1, H1, A8, H8)
-- Bits 5-11  (7) : En Passant Square (0-63, 64=None)
-- Bits 12-21 (10): Halfmove Clock
-- Bits 22-37 (16): Fullmove Number
-- Bits 38-63 (26): Unused
data GameState = GameState
  { gsPacked :: {-# UNPACK #-} !Word64
  , gsHash   :: {-# UNPACK #-} !Word64
  } deriving (Eq)

instance Show GameState where
  show gs = "GameState { turn = " ++ show (turn gs) ++
            ", castlingRights = " ++ show (castlingRights gs) ++
            ", epSquare = " ++ show (epSquare gs) ++
            ", halfmoveClock = " ++ show (halfmoveClock gs) ++
            ", fullmoveNumber = " ++ show (fullmoveNumber gs) ++
            ", zobristHash = " ++ show (zobristHash gs) ++
            " }"

-- | Initial game state for standard chess.
initialGameState :: GameState
initialGameState =
    let packed = (0 :: Word64) -- White (0)
                 .|. (0xF `shiftL` 1) -- All castling (4 bits)
                 .|. (64 `shiftL` 5) -- EP None (64)
                 .|. (0 `shiftL` 12) -- Halfmove 0
                 .|. (1 `shiftL` 22) -- Fullmove 1
    in GameState packed 0

-- Getters

{-# INLINE turn #-}
turn :: GameState -> Color
turn (GameState p _) = if testBit p 0 then Black else White

{-# INLINE castlingRights #-}
castlingRights :: GameState -> CastlingRights
castlingRights (GameState p _) =
    let cr = (p `shiftR` 1) .&. 0xF
        a1 = if testBit cr 0 then BB_A1 else 0
        h1 = if testBit cr 1 then BB_H1 else 0
        a8 = if testBit cr 2 then BB_A8 else 0
        h8 = if testBit cr 3 then BB_H8 else 0
    in a1 .|. h1 .|. a8 .|. h8

{-# INLINE epSquare #-}
epSquare :: GameState -> Square
epSquare (GameState p _) = Square (fromIntegral ((p `shiftR` 5) .&. 0x7F))

{-# INLINE halfmoveClock #-}
halfmoveClock :: GameState -> HalfmoveClock
halfmoveClock (GameState p _) = HalfmoveClock (fromIntegral ((p `shiftR` 12) .&. 0x3FF))

{-# INLINE fullmoveNumber #-}
fullmoveNumber :: GameState -> FullmoveNumber
fullmoveNumber (GameState p _) = FullmoveNumber (fromIntegral ((p `shiftR` 22) .&. 0xFFFF))

{-# INLINE zobristHash #-}
zobristHash :: GameState -> Word64
zobristHash (GameState _ h) = h

-- Setters

{-# INLINE setTurn #-}
setTurn :: Color -> GameState -> GameState
setTurn c (GameState p h) =
    let bit = case c of White -> 0; Black -> 1
        p' = (p .&. complement 1) .|. bit
    in GameState p' h

{-# INLINE setCastlingRights #-}
setCastlingRights :: CastlingRights -> GameState -> GameState
setCastlingRights cr (GameState p h) =
    let bits = (if testBit cr (unSquare A1) then 1 else 0) .|.
               (if testBit cr (unSquare H1) then 2 else 0) .|.
               (if testBit cr (unSquare A8) then 4 else 0) .|.
               (if testBit cr (unSquare H8) then 8 else 0)
        p' = (p .&. complement (0xF `shiftL` 1)) .|. (bits `shiftL` 1)
    in GameState p' h

{-# INLINE setEpSquare #-}
setEpSquare :: Square -> GameState -> GameState
setEpSquare (Square sq) (GameState p h) =
    let val = fromIntegral sq .&. 0x7F
        p' = (p .&. complement (0x7F `shiftL` 5)) .|. (val `shiftL` 5)
    in GameState p' h

{-# INLINE setHalfmoveClock #-}
setHalfmoveClock :: HalfmoveClock -> GameState -> GameState
setHalfmoveClock (HalfmoveClock c) (GameState p h) =
    let val = fromIntegral c .&. 0x3FF
        p' = (p .&. complement (0x3FF `shiftL` 12)) .|. (val `shiftL` 12)
    in GameState p' h

{-# INLINE setFullmoveNumber #-}
setFullmoveNumber :: FullmoveNumber -> GameState -> GameState
setFullmoveNumber (FullmoveNumber c) (GameState p h) =
    let val = fromIntegral c .&. 0xFFFF
        p' = (p .&. complement (0xFFFF `shiftL` 22)) .|. (val `shiftL` 22)
    in GameState p' h

{-# INLINE setZobristHash #-}
setZobristHash :: Word64 -> GameState -> GameState
setZobristHash h (GameState p _) = GameState p h

-- Helpers

-- | Check if the given side has kingside castling rights.
{-# INLINE canCastleKingside #-}
canCastleKingside :: GameState -> Color -> Bool
canCastleKingside gs c =
    let cr = (gsPacked gs `shiftR` 1) .&. 0xF
    in case c of
        White -> testBit cr 1 -- H1
        Black -> testBit cr 3 -- H8

-- | Check if the given side has queenside castling rights.
{-# INLINE canCastleQueenside #-}
canCastleQueenside :: GameState -> Color -> Bool
canCastleQueenside gs c =
    let cr = (gsPacked gs `shiftR` 1) .&. 0xF
    in case c of
        White -> testBit cr 0 -- A1
        Black -> testBit cr 2 -- A8

-- | Remove castling rights for a color (e.g. king moved).
removeColorCastlingRights :: GameState -> Color -> GameState
removeColorCastlingRights gs White =
    let p = gsPacked gs
        -- Clear A1 (bit 0) and H1 (bit 1) -> Mask 0x3 shifted by 1 = 0x6? No.
        -- Bit 0 of CR is A1, Bit 1 is H1.
        -- We want to clear bits 1 and 2 of packed word? No.
        -- Bits 1-4 are CR.
        -- A1 is at 1+0=1. H1 is at 1+1=2.
        -- Mask to clear is ~(0x3 << 1) = ~(0x6)
        p' = p .&. complement (0x3 `shiftL` 1)
    in gs { gsPacked = p' }
removeColorCastlingRights gs Black =
    let p = gsPacked gs
        -- A8 is at 1+2=3. H8 is at 1+3=4.
        -- Mask to clear is ~(0xC << 1) = ~(0x18)
        p' = p .&. complement (0xC `shiftL` 1)
    in gs { gsPacked = p' }

-- | Remove castling rights for a specific rook square (e.g. rook moved or captured).
removeCastlingRight :: GameState -> Square -> GameState
removeCastlingRight gs sq =
    let p = gsPacked gs
        bit = case sq of
                A1 -> 0
                H1 -> 1
                A8 -> 2
                H8 -> 3
                _  -> -1
    in if bit == -1
       then gs
       else gs { gsPacked = p `clearBit` (1 + bit) }
