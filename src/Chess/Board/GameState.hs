{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE BangPatterns #-}
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
-- Packed into two Word64s for efficiency and unboxing.
data GameState = GameState
  { gsPacked    :: {-# UNPACK #-} !Word64
  , zobristHash :: {-# UNPACK #-} !Word64
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
-- Note: zobristHash is set to 0 here and should be updated when combined with a board.
initialGameState :: GameState
initialGameState = GameState
  { gsPacked = pack initialTurn initialCastling initialEp initialHalf initialFull
  , zobristHash = 0
  }
  where
    initialTurn = White
    initialCastling = allCastling
    initialEp = Nothing
    initialHalf = 0
    initialFull = 1

-- Packing / Unpacking logic --------------------------------------------------

-- Offsets & Masks
offCastling, offTurn, offEp, offHalf, offFull :: Int
offCastling = 0
offTurn     = 16
offEp       = 17
offHalf     = 24
offFull     = 34

maskCastling, maskTurn, maskEp, maskHalf, maskFull :: Word64
maskCastling = 0xFFFF
maskTurn     = 0x1
maskEp       = 0x7F
maskHalf     = 0x3FF
maskFull     = 0xFFFF

pack :: Color -> CastlingRights -> Maybe Square -> HalfmoveClock -> FullmoveNumber -> Word64
pack c cr ep hm fm =
    let crPacked = (cr .&. 0xFF) .|. ((cr `shiftR` 56) `shiftL` 8)
        cVal     = fromIntegral (fromEnum c)
        epVal    = case ep of Nothing -> 64; Just (Square s) -> fromIntegral s
        hmVal    = fromIntegral (unHalfmoveClock hm)
        fmVal    = fromIntegral (unFullmoveNumber fm)
    in (crPacked .&. maskCastling) .|.
       ((cVal .&. maskTurn) `shiftL` offTurn) .|.
       ((epVal .&. maskEp) `shiftL` offEp) .|.
       ((hmVal .&. maskHalf) `shiftL` offHalf) .|.
       ((fmVal .&. maskFull) `shiftL` offFull)

-- Accessors ------------------------------------------------------------------

{-# INLINE turn #-}
turn :: GameState -> Color
turn gs = toEnum $ fromIntegral $ (gsPacked gs `shiftR` offTurn) .&. maskTurn

{-# INLINE castlingRights #-}
castlingRights :: GameState -> CastlingRights
castlingRights gs =
    let packed = (gsPacked gs `shiftR` offCastling) .&. maskCastling
        low  = packed .&. 0xFF
        high = (packed `shiftR` 8) .&. 0xFF
    in low .|. (high `shiftL` 56)

{-# INLINE epSquare #-}
epSquare :: GameState -> Maybe Square
epSquare gs =
    let val = (gsPacked gs `shiftR` offEp) .&. maskEp
    in if val == 64 then Nothing else Just (Square (fromIntegral val))

{-# INLINE halfmoveClock #-}
halfmoveClock :: GameState -> HalfmoveClock
halfmoveClock gs = HalfmoveClock $ fromIntegral $ (gsPacked gs `shiftR` offHalf) .&. maskHalf

{-# INLINE fullmoveNumber #-}
fullmoveNumber :: GameState -> FullmoveNumber
fullmoveNumber gs = FullmoveNumber $ fromIntegral $ (gsPacked gs `shiftR` offFull) .&. maskFull

-- Setters --------------------------------------------------------------------

{-# INLINE setTurn #-}
setTurn :: Color -> GameState -> GameState
setTurn c gs =
    let val = fromIntegral (fromEnum c)
        mask = maskTurn `shiftL` offTurn
        packed' = (gsPacked gs .&. complement mask) .|. ((val .&. maskTurn) `shiftL` offTurn)
    in gs { gsPacked = packed' }

{-# INLINE setCastlingRights #-}
setCastlingRights :: CastlingRights -> GameState -> GameState
setCastlingRights cr gs =
    let crPacked = (cr .&. 0xFF) .|. ((cr `shiftR` 56) `shiftL` 8)
        mask = maskCastling `shiftL` offCastling
        packed' = (gsPacked gs .&. complement mask) .|. ((crPacked .&. maskCastling) `shiftL` offCastling)
    in gs { gsPacked = packed' }

{-# INLINE setEpSquare #-}
setEpSquare :: Maybe Square -> GameState -> GameState
setEpSquare ep gs =
    let val = case ep of Nothing -> 64; Just (Square s) -> fromIntegral s
        mask = maskEp `shiftL` offEp
        packed' = (gsPacked gs .&. complement mask) .|. ((val .&. maskEp) `shiftL` offEp)
    in gs { gsPacked = packed' }

{-# INLINE setHalfmoveClock #-}
setHalfmoveClock :: HalfmoveClock -> GameState -> GameState
setHalfmoveClock hm gs =
    let val = fromIntegral (unHalfmoveClock hm)
        mask = maskHalf `shiftL` offHalf
        packed' = (gsPacked gs .&. complement mask) .|. ((val .&. maskHalf) `shiftL` offHalf)
    in gs { gsPacked = packed' }

{-# INLINE setFullmoveNumber #-}
setFullmoveNumber :: FullmoveNumber -> GameState -> GameState
setFullmoveNumber fm gs =
    let val = fromIntegral (unFullmoveNumber fm)
        mask = maskFull `shiftL` offFull
        packed' = (gsPacked gs .&. complement mask) .|. ((val .&. maskFull) `shiftL` offFull)
    in gs { gsPacked = packed' }

{-# INLINE setZobristHash #-}
setZobristHash :: Word64 -> GameState -> GameState
setZobristHash h gs = gs { zobristHash = h }

-- Helpers --------------------------------------------------------------------

{-# INLINE canCastleKingside #-}
canCastleKingside :: GameState -> Color -> Bool
canCastleKingside gs White = testBit (castlingRights gs) (unSquare H1)
canCastleKingside gs Black = testBit (castlingRights gs) (unSquare H8)

{-# INLINE canCastleQueenside #-}
canCastleQueenside :: GameState -> Color -> Bool
canCastleQueenside gs White = testBit (castlingRights gs) (unSquare A1)
canCastleQueenside gs Black = testBit (castlingRights gs) (unSquare A8)

{-# INLINE removeColorCastlingRights #-}
removeColorCastlingRights :: GameState -> Color -> GameState
removeColorCastlingRights gs White = setCastlingRights (castlingRights gs .&. complement (BB_A1 .|. BB_H1)) gs
removeColorCastlingRights gs Black = setCastlingRights (castlingRights gs .&. complement (BB_A8 .|. BB_H8)) gs

{-# INLINE removeCastlingRight #-}
removeCastlingRight :: GameState -> Square -> GameState
removeCastlingRight gs sq = setCastlingRights (castlingRights gs .&. complement (bbFromSquare sq)) gs

-- | Constructor helper for manual creation (e.g. FEN parsing)
mkGameState :: Color -> CastlingRights -> Maybe Square -> HalfmoveClock -> FullmoveNumber -> Word64 -> GameState
mkGameState c cr ep hm fm h = GameState
    { gsPacked = pack c cr ep hm fm
    , zobristHash = h
    }
