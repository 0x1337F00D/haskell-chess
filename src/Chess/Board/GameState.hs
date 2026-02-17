{-# LANGUAGE PatternSynonyms #-}
<<<<<<< HEAD
module Chess.Board.GameState
  ( GameState(..)
  , CastlingRights
  , noCastling
  , allCastling
  , initialGameState
  , mkGameState
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
=======
module Chess.Board.GameState where
>>>>>>> origin/main

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
<<<<<<< HEAD
-- Packed GameState
data GameState = GameState
  { gsHash     :: {-# UNPACK #-} !Word64
  , gsCastling :: {-# UNPACK #-} !Word64
  , gsPacked   :: {-# UNPACK #-} !Word64
  } deriving (Eq, Show)

-- Packing Layout
-- Bit 0: Turn (0=White, 1=Black)
-- Bits 1-7: EpSquare (0-63, 64=NoSquare)
-- Bits 8-23: HalfmoveClock
-- Bits 24-55: FullmoveNumber

mkGameState :: Color -> CastlingRights -> Square -> HalfmoveClock -> FullmoveNumber -> Word64 -> GameState
mkGameState c cr ep hm fm hash =
    let tVal = if c == White then 0 else 1
        epVal = fromIntegral (unSquare ep) :: Word64
        hmVal = fromIntegral (unHalfmoveClock hm) :: Word64
        fmVal = fromIntegral (unFullmoveNumber fm) :: Word64
        packed = tVal .|. (epVal `shiftL` 1) .|. (hmVal `shiftL` 8) .|. (fmVal `shiftL` 24)
    in GameState hash cr packed

initialGameState :: GameState
initialGameState = mkGameState White allCastling NoSquare 0 1 0

-- Accessors

turn :: GameState -> Color
turn gs = if testBit (gsPacked gs) 0 then Black else White

castlingRights :: GameState -> CastlingRights
castlingRights = gsCastling

epSquare :: GameState -> Square
epSquare gs = Square $ fromIntegral $ (gsPacked gs `shiftR` 1) .&. 0x7F

halfmoveClock :: GameState -> HalfmoveClock
halfmoveClock gs = HalfmoveClock $ fromIntegral $ (gsPacked gs `shiftR` 8) .&. 0xFFFF

fullmoveNumber :: GameState -> FullmoveNumber
fullmoveNumber gs = FullmoveNumber $ fromIntegral $ (gsPacked gs `shiftR` 24) .&. 0xFFFFFFFF

zobristHash :: GameState -> Word64
zobristHash = gsHash

-- Setters

setTurn :: GameState -> Color -> GameState
setTurn gs c =
    let tVal = if c == White then 0 else 1
        packed = (gsPacked gs .&. complement 1) .|. tVal
    in gs { gsPacked = packed }

setCastlingRights :: GameState -> CastlingRights -> GameState
setCastlingRights gs cr = gs { gsCastling = cr }

setEpSquare :: GameState -> Square -> GameState
setEpSquare gs sq =
    let epVal = fromIntegral (unSquare sq) :: Word64
        packed = (gsPacked gs .&. complement (0x7F `shiftL` 1)) .|. (epVal `shiftL` 1)
    in gs { gsPacked = packed }

setHalfmoveClock :: GameState -> HalfmoveClock -> GameState
setHalfmoveClock gs hm =
    let hmVal = fromIntegral (unHalfmoveClock hm) :: Word64
        packed = (gsPacked gs .&. complement (0xFFFF `shiftL` 8)) .|. (hmVal `shiftL` 8)
    in gs { gsPacked = packed }

setFullmoveNumber :: GameState -> FullmoveNumber -> GameState
setFullmoveNumber gs fm =
    let fmVal = fromIntegral (unFullmoveNumber fm) :: Word64
        packed = (gsPacked gs .&. complement (0xFFFFFFFF `shiftL` 24)) .|. (fmVal `shiftL` 24)
    in gs { gsPacked = packed }

setZobristHash :: GameState -> Word64 -> GameState
setZobristHash gs h = gs { gsHash = h }

-- Logic functions

canCastleKingside :: GameState -> Color -> Bool
canCastleKingside gs White = testBit (gsCastling gs) (unSquare H1)
canCastleKingside gs Black = testBit (gsCastling gs) (unSquare H8)

canCastleQueenside :: GameState -> Color -> Bool
canCastleQueenside gs White = testBit (gsCastling gs) (unSquare A1)
canCastleQueenside gs Black = testBit (gsCastling gs) (unSquare A8)

removeColorCastlingRights :: GameState -> Color -> GameState
removeColorCastlingRights gs White = gs { gsCastling = gsCastling gs .&. complement (BB_A1 .|. BB_H1) }
removeColorCastlingRights gs Black = gs { gsCastling = gsCastling gs .&. complement (BB_A8 .|. BB_H8) }

removeCastlingRight :: GameState -> Square -> GameState
removeCastlingRight gs sq = gs { gsCastling = gsCastling gs .&. complement (bbFromSquare sq) }
=======
data GameState = GameState
  { turn           :: !Color
  , castlingRights :: !CastlingRights
  , epSquare       :: !Square
  , halfmoveClock  :: !HalfmoveClock
  , fullmoveNumber :: !FullmoveNumber
  , zobristHash    :: !Word64
  } deriving (Eq, Show)

-- | Initial game state for standard chess.
-- Note: zobristHash is set to 0 here and should be updated when combined with a board.
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
canCastleKingside :: GameState -> Color -> Bool
canCastleKingside gs White = testBit (castlingRights gs) (unSquare H1)
canCastleKingside gs Black = testBit (castlingRights gs) (unSquare H8)

-- | Check if the given side has queenside castling rights.
canCastleQueenside :: GameState -> Color -> Bool
canCastleQueenside gs White = testBit (castlingRights gs) (unSquare A1)
canCastleQueenside gs Black = testBit (castlingRights gs) (unSquare A8)

-- | Remove castling rights for a color (e.g. king moved).
removeColorCastlingRights :: GameState -> Color -> GameState
removeColorCastlingRights gs White = gs { castlingRights = castlingRights gs .&. complement (BB_A1 .|. BB_H1) }
removeColorCastlingRights gs Black = gs { castlingRights = castlingRights gs .&. complement (BB_A8 .|. BB_H8) }

-- | Remove castling rights for a specific rook square (e.g. rook moved or captured).
removeCastlingRight :: GameState -> Square -> GameState
removeCastlingRight gs sq = gs { castlingRights = castlingRights gs .&. complement (bbFromSquare sq) }
>>>>>>> origin/main
