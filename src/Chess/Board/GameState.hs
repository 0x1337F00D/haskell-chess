{-# LANGUAGE PatternSynonyms #-}
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
data GameState = GameState
  { turn           :: !Color
  , castlingRights :: !CastlingRights
  , epSquare       :: !(Maybe Square)
  , halfmoveClock  :: !Int
  , fullmoveNumber :: !Int
  , zobristHash    :: !Word64
  } deriving (Eq, Show)

-- | Initial game state for standard chess.
-- Note: zobristHash is set to 0 here and should be updated when combined with a board.
initialGameState :: GameState
initialGameState = GameState
  { turn = White
  , castlingRights = allCastling
  , epSquare = Nothing
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
