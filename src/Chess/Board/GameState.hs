{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
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
-- Packed into 2 Word64s to avoid heap allocation.
-- Word 1: _packed
--   Bit 0      (1): Turn (0=White, 1=Black)
--   Bits 1-4   (4): White Rook 1 (3 bits File, 1 bit Present)
--   Bits 5-8   (4): White Rook 2
--   Bits 9-12  (4): Black Rook 1
--   Bits 13-16 (4): Black Rook 2
--   Bits 17-23 (7): EP Square
--   Bits 24-33 (10): Halfmove Clock
--   Bits 34-49 (16): Fullmove Number
--   Bits 50-63 (14): Unused
-- Word 2: _zobrist (64 bits)
data GameState = GameStatePacked
  { _packed      :: {-# UNPACK #-} !Word64
  , _zobristHash :: {-# UNPACK #-} !Word64
  } deriving (Eq)

instance Show GameState where
  show gs = "GameState {turn = " ++ show (turn gs) ++
            ", castlingRights = " ++ show (castlingRights gs) ++
            ", epSquare = " ++ show (epSquare gs) ++
            ", halfmoveClock = " ++ show (halfmoveClock gs) ++
            ", fullmoveNumber = " ++ show (fullmoveNumber gs) ++
            ", zobristHash = " ++ show (zobristHash gs) ++
            "}"

-- | Pattern synonym to expose the record interface.
pattern GameState :: Color -> CastlingRights -> Square -> HalfmoveClock -> FullmoveNumber -> Word64 -> GameState
pattern GameState { turn, castlingRights, epSquare, halfmoveClock, fullmoveNumber, zobristHash } <-
    (unpackGameState -> (turn, castlingRights, epSquare, halfmoveClock, fullmoveNumber, zobristHash))
    where
        GameState t c e h f z = mkGameState t c e h f z

{-# COMPLETE GameState #-}

-- HOTPATH: Zero-cost Getter ohne Pattern-Synonyms
{-# INLINE gsPacked #-}
gsPacked :: GameState -> Word64
gsPacked (GameStatePacked p _) = p

{-# INLINE gsTurn #-}
gsTurn :: GameState -> Color
gsTurn gs = if testBit (gsPacked gs) 0 then Black else White

{-# INLINE gsEPSquareRaw #-}
gsEPSquareRaw :: GameState -> Word64
gsEPSquareRaw gs = (gsPacked gs `unsafeShiftR` 17) .&. 0x7F

{-# INLINE gsEPSquare #-}
gsEPSquare :: GameState -> Square
gsEPSquare gs = Square (fromIntegral (gsEPSquareRaw gs))

{-# INLINE unpackGameState #-}
unpackGameState :: GameState -> (Color, CastlingRights, Square, HalfmoveClock, FullmoveNumber, Word64)
unpackGameState (GameStatePacked p z) =
    ( if testBit p 0 then Black else White
    , unpackCastling ((p `shiftR` 1) .&. 0xFFFF)
    , Square (fromIntegral ((p `shiftR` 17) .&. 0x7F))
    , HalfmoveClock (fromIntegral ((p `shiftR` 24) .&. 0x3FF))
    , FullmoveNumber (fromIntegral ((p `shiftR` 34) .&. 0xFFFF))
    , z
    )

{-# INLINE mkGameState #-}
mkGameState :: Color -> CastlingRights -> Square -> HalfmoveClock -> FullmoveNumber -> Word64 -> GameState
mkGameState t c e h f z =
    let !p = (if t == Black then 1 else 0) .|.
             (packCastling c `shiftL` 1) .|.
             (fromIntegral (unSquare e) `shiftL` 17) .|.
             (fromIntegral (unHalfmoveClock h) `shiftL` 24) .|.
             (fromIntegral (unFullmoveNumber f) `shiftL` 34)
    in GameStatePacked p z

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

-- | Initial game state for standard chess.
initialGameState :: GameState
initialGameState = GameState
  { turn = White
  , castlingRights = allCastling
  , epSquare = NoSquare
  , halfmoveClock = 0
  , fullmoveNumber = 1
  , zobristHash = 0
  }

-- | Check if the given side has kingside castling rights.
-- For standard chess, checks H1/H8.
{-# INLINE canCastleStandardKingside #-}
canCastleStandardKingside :: GameState -> Color -> Bool
canCastleStandardKingside (GameStatePacked p _) c =
    let shiftVal = if c == White then 1 else 9
        p' = p `shiftR` shiftVal
        s1 = p' .&. 0xF
        s2 = (p' `shiftR` 4) .&. 0xF
        target = 15 -- Present(8) | File 7(H)
    in s1 == target || s2 == target

-- | Check if the given side has queenside castling rights.
-- For standard chess, checks A1/A8.
{-# INLINE canCastleStandardQueenside #-}
canCastleStandardQueenside :: GameState -> Color -> Bool
canCastleStandardQueenside (GameStatePacked p _) c =
    let shiftVal = if c == White then 1 else 9
        p' = p `shiftR` shiftVal
        s1 = p' .&. 0xF
        s2 = (p' `shiftR` 4) .&. 0xF
        target = 8 -- Present(8) | File 0(A)
    in s1 == target || s2 == target

-- | Remove castling rights for a color (e.g. king moved).
{-# INLINE removeColorCastlingRights #-}
removeColorCastlingRights :: GameState -> Color -> GameState
removeColorCastlingRights (GameStatePacked p z) White = GameStatePacked (p .&. complement (0xFF `shiftL` 1)) z -- Clear White slots (8 bits at 1)
removeColorCastlingRights (GameStatePacked p z) Black = GameStatePacked (p .&. complement (0xFF `shiftL` 9)) z -- Clear Black slots (8 bits at 9)

-- | Remove castling rights for a specific rook square (e.g. rook moved or captured).
{-# INLINE removeCastlingRight #-}
removeCastlingRight :: GameState -> Square -> GameState
removeCastlingRight (GameStatePacked p z) sq =
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
           in GameStatePacked p'' z
       else if rank == 7 -- Black
       then
           let mask1 = (1 `shiftL` 3) .|. (fromIntegral file)
               s1 = (p `shiftR` 9) .&. 0xF
               s2 = (p `shiftR` 13) .&. 0xF

               p' = if s1 == mask1 then p `clearBit` (9 + 3) else p
               p'' = if s2 == mask1 then p' `clearBit` (13 + 3) else p'
           in GameStatePacked p'' z
       else GameStatePacked p z
