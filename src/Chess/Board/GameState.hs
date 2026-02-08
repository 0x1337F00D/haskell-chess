{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

module Chess.Board.GameState
  ( GameState(GameState)
  , turn
  , castlingRights
  , epSquare
  , halfmoveClock
  , fullmoveNumber
  , zobristHash
  , CastlingRights
  , noCastling
  , allCastling
  , initialGameState
  , canCastleKingside
  , canCastleQueenside
  , removeColorCastlingRights
  , removeCastlingRight
  -- * Setters
  , setTurn
  , setCastlingRights
  , setEpSquare
  , setHalfmoveClock
  , setFullmoveNumber
  , setZobristHash
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
-- Packed representation:
-- _zobristHash :: {-# UNPACK #-} !Word64
-- _castlingRights :: {-# UNPACK #-} !Word64
-- _packedFields :: {-# UNPACK #-} !Word64
--   bits 0-0: turn (0=White, 1=Black)
--   bits 1-7: epSquare (0-63, 64=NoSquare)
--   bits 8-23: halfmoveClock (16 bits)
--   bits 24-39: fullmoveNumber (16 bits)
data GameState = GameStatePacked
  { _zobristHash   :: {-# UNPACK #-} !Word64
  , _castlingRights :: {-# UNPACK #-} !Word64
  , _packedFields   :: {-# UNPACK #-} !Word64
  } deriving (Eq)

instance Show GameState where
  show (GameState t c e h f z) =
    "GameState {turn = " ++ show t ++
    ", castlingRights = " ++ show c ++
    ", epSquare = " ++ show e ++
    ", halfmoveClock = " ++ show h ++
    ", fullmoveNumber = " ++ show f ++
    ", zobristHash = " ++ show z ++ "}"

-- | Pattern Synonym to expose GameState as if it were a record.
-- Note: This allows pattern matching and construction, but NOT record updates.
pattern GameState :: Color -> CastlingRights -> Square -> HalfmoveClock -> FullmoveNumber -> Word64 -> GameState
pattern GameState t c e h f z <- (unpackGameState -> (t, c, e, h, f, z))
  where GameState t c e h f z = packGameState t c e h f z

{-# COMPLETE GameState #-}

unpackGameState :: GameState -> (Color, CastlingRights, Square, HalfmoveClock, FullmoveNumber, Word64)
unpackGameState (GameStatePacked z c p) =
  let t = if testBit p 0 then Black else White
      e = Square (fromIntegral ((p `shiftR` 1) .&. 0x7F))
      h = HalfmoveClock (fromIntegral ((p `shiftR` 8) .&. 0xFFFF))
      f = FullmoveNumber (fromIntegral ((p `shiftR` 24) .&. 0xFFFF))
  in (t, c, e, h, f, z)

packGameState :: Color -> CastlingRights -> Square -> HalfmoveClock -> FullmoveNumber -> Word64 -> GameState
packGameState t c e h f z =
  let tBit = if t == White then 0 else 1
      eVal = fromIntegral (unSquare e) :: Word64
      hVal = fromIntegral (unHalfmoveClock h) :: Word64
      fVal = fromIntegral (unFullmoveNumber f) :: Word64
      p = (tBit .&. 1) .|.
          ((eVal .&. 0x7F) `shiftL` 1) .|.
          ((hVal .&. 0xFFFF) `shiftL` 8) .|.
          ((fVal .&. 0xFFFF) `shiftL` 24)
  in GameStatePacked z c p

-- Accessors

turn :: GameState -> Color
turn (GameStatePacked _ _ p) = if testBit p 0 then Black else White

castlingRights :: GameState -> CastlingRights
castlingRights (GameStatePacked _ c _) = c

epSquare :: GameState -> Square
epSquare (GameStatePacked _ _ p) = Square (fromIntegral ((p `shiftR` 1) .&. 0x7F))

halfmoveClock :: GameState -> HalfmoveClock
halfmoveClock (GameStatePacked _ _ p) = HalfmoveClock (fromIntegral ((p `shiftR` 8) .&. 0xFFFF))

fullmoveNumber :: GameState -> FullmoveNumber
fullmoveNumber (GameStatePacked _ _ p) = FullmoveNumber (fromIntegral ((p `shiftR` 24) .&. 0xFFFF))

zobristHash :: GameState -> Word64
zobristHash (GameStatePacked z _ _) = z

-- Setters

setTurn :: Color -> GameState -> GameState
setTurn t (GameStatePacked z c p) =
  let tBit = if t == White then 0 else 1
      p' = (p .&. complement 1) .|. (tBit .&. 1)
  in GameStatePacked z c p'

setCastlingRights :: CastlingRights -> GameState -> GameState
setCastlingRights c (GameStatePacked z _ p) = GameStatePacked z c p

setEpSquare :: Square -> GameState -> GameState
setEpSquare e (GameStatePacked z c p) =
  let eVal = fromIntegral (unSquare e) :: Word64
      p' = (p .&. complement (0x7F `shiftL` 1)) .|. ((eVal .&. 0x7F) `shiftL` 1)
  in GameStatePacked z c p'

setHalfmoveClock :: HalfmoveClock -> GameState -> GameState
setHalfmoveClock h (GameStatePacked z c p) =
  let hVal = fromIntegral (unHalfmoveClock h) :: Word64
      p' = (p .&. complement (0xFFFF `shiftL` 8)) .|. ((hVal .&. 0xFFFF) `shiftL` 8)
  in GameStatePacked z c p'

setFullmoveNumber :: FullmoveNumber -> GameState -> GameState
setFullmoveNumber f (GameStatePacked z c p) =
  let fVal = fromIntegral (unFullmoveNumber f) :: Word64
      p' = (p .&. complement (0xFFFF `shiftL` 24)) .|. ((fVal .&. 0xFFFF) `shiftL` 24)
  in GameStatePacked z c p'

setZobristHash :: Word64 -> GameState -> GameState
setZobristHash z (GameStatePacked _ c p) = GameStatePacked z c p

-- | Initial game state for standard chess.
initialGameState :: GameState
initialGameState = GameState White allCastling NoSquare 0 1 0

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
removeColorCastlingRights gs White = setCastlingRights (castlingRights gs .&. complement (BB_A1 .|. BB_H1)) gs
removeColorCastlingRights gs Black = setCastlingRights (castlingRights gs .&. complement (BB_A8 .|. BB_H8)) gs

-- | Remove castling rights for a specific rook square (e.g. rook moved or captured).
removeCastlingRight :: GameState -> Square -> GameState
removeCastlingRight gs sq = setCastlingRights (castlingRights gs .&. complement (bbFromSquare sq)) gs
