{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ViewPatterns #-}
module Chess.Board.GameState where

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
-- Packed representation:
-- gsHash: Zobrist Hash (64 bits)
-- gsCastling: Castling Rights (64 bits)
-- gsPacked: Packed fields (64 bits)
--   Bits 0-0: Turn (0=White, 1=Black)
--   Bits 1-7: EpSquare (0-63, 64=None)
--   Bits 8-23: Halfmove Clock (16 bits)
--   Bits 24-39: Fullmove Number (16 bits)
data GameState = GameStatePacked
  { gsHash     :: {-# UNPACK #-} !Word64
  , gsCastling :: {-# UNPACK #-} !Word64
  , gsPacked   :: {-# UNPACK #-} !Word64
  } deriving (Eq, Show)

-- | Pattern Synonym for GameState to maintain API compatibility.
-- Note: This is read-only or construction-only. Updates must use setters.
pattern GameState :: Color -> CastlingRights -> Square -> HalfmoveClock -> FullmoveNumber -> Word64 -> GameState
pattern GameState t c ep hm fm z <- (unpackGameState -> (t, c, ep, hm, fm, z))
  where GameState t c ep hm fm z = mkGameState t c ep hm fm z

{-# COMPLETE GameState #-}

-- | Helper to unpack GameState.
unpackGameState :: GameState -> (Color, CastlingRights, Square, HalfmoveClock, FullmoveNumber, Word64)
unpackGameState (GameStatePacked h c p) =
    let t = if testBit p 0 then Black else White
        epVal = (p `shiftR` 1) .&. 0x7F
        ep = Square (fromIntegral epVal)
        hm = HalfmoveClock (fromIntegral ((p `shiftR` 8) .&. 0xFFFF))
        fm = FullmoveNumber (fromIntegral ((p `shiftR` 24) .&. 0xFFFF))
    in (t, c, ep, hm, fm, h)

-- | Helper to pack GameState.
mkGameState :: Color -> CastlingRights -> Square -> HalfmoveClock -> FullmoveNumber -> Word64 -> GameState
mkGameState t c ep (HalfmoveClock hm) (FullmoveNumber fm) z =
    let tVal = if t == White then 0 else 1
        epVal = unSquare ep
        hmVal = fromIntegral hm
        fmVal = fromIntegral fm
        p = tVal .|. (fromIntegral epVal `shiftL` 1) .|. (hmVal `shiftL` 8) .|. (fmVal `shiftL` 24)
    in GameStatePacked z c p

-- | Initial game state for standard chess.
initialGameState :: GameState
initialGameState = GameState White allCastling NoSquare 0 1 0

-- | Get turn.
turn :: GameState -> Color
turn (GameStatePacked _ _ p) = if testBit p 0 then Black else White

-- | Set turn.
setTurn :: GameState -> Color -> GameState
setTurn (GameStatePacked h c p) t =
    let tVal = if t == White then 0 else 1
        p' = (p .&. complement 1) .|. tVal
    in GameStatePacked h c p'

-- | Get castling rights.
castlingRights :: GameState -> CastlingRights
castlingRights (GameStatePacked _ c _) = c

-- | Set castling rights.
setCastlingRights :: GameState -> CastlingRights -> GameState
setCastlingRights (GameStatePacked h _ p) c = GameStatePacked h c p

-- | Get en passant square.
epSquare :: GameState -> Square
epSquare (GameStatePacked _ _ p) = Square (fromIntegral ((p `shiftR` 1) .&. 0x7F))

-- | Set en passant square.
setEpSquare :: GameState -> Square -> GameState
setEpSquare (GameStatePacked h c p) ep =
    let epVal = fromIntegral (unSquare ep)
        p' = (p .&. complement (0x7F `shiftL` 1)) .|. (epVal `shiftL` 1)
    in GameStatePacked h c p'

-- | Get halfmove clock.
halfmoveClock :: GameState -> HalfmoveClock
halfmoveClock (GameStatePacked _ _ p) = HalfmoveClock (fromIntegral ((p `shiftR` 8) .&. 0xFFFF))

-- | Set halfmove clock.
setHalfmoveClock :: GameState -> HalfmoveClock -> GameState
setHalfmoveClock (GameStatePacked h c p) (HalfmoveClock hm) =
    let hmVal = fromIntegral hm
        p' = (p .&. complement (0xFFFF `shiftL` 8)) .|. (hmVal `shiftL` 8)
    in GameStatePacked h c p'

-- | Get fullmove number.
fullmoveNumber :: GameState -> FullmoveNumber
fullmoveNumber (GameStatePacked _ _ p) = FullmoveNumber (fromIntegral ((p `shiftR` 24) .&. 0xFFFF))

-- | Set fullmove number.
setFullmoveNumber :: GameState -> FullmoveNumber -> GameState
setFullmoveNumber (GameStatePacked h c p) (FullmoveNumber fm) =
    let fmVal = fromIntegral fm
        p' = (p .&. complement (0xFFFF `shiftL` 24)) .|. (fmVal `shiftL` 24)
    in GameStatePacked h c p'

-- | Get Zobrist hash.
zobristHash :: GameState -> Word64
zobristHash (GameStatePacked h _ _) = h

-- | Set Zobrist hash.
setZobristHash :: GameState -> Word64 -> GameState
setZobristHash (GameStatePacked _ c p) h = GameStatePacked h c p

-- | Check if the given side has kingside castling rights.
canCastleKingside :: GameState -> Color -> Bool
canCastleKingside gs White = testBit (castlingRights gs) (unSquare H1)
canCastleKingside gs Black = testBit (castlingRights gs) (unSquare H8)

-- | Check if the given side has queenside castling rights.
canCastleQueenside :: GameState -> Color -> Bool
canCastleQueenside gs White = testBit (castlingRights gs) (unSquare A1)
canCastleQueenside gs Black = testBit (castlingRights gs) (unSquare A8)

-- | Remove castling rights for a color (e.g. king moved).
removeColorCastlingRights :: GameState -> Color -> GameState
removeColorCastlingRights gs White = setCastlingRights gs (castlingRights gs .&. complement (BB_A1 .|. BB_H1))
removeColorCastlingRights gs Black = setCastlingRights gs (castlingRights gs .&. complement (BB_A8 .|. BB_H8))

-- | Remove castling rights for a specific rook square (e.g. rook moved or captured).
removeCastlingRight :: GameState -> Square -> GameState
removeCastlingRight gs sq = setCastlingRights gs (castlingRights gs .&. complement (bbFromSquare sq))
