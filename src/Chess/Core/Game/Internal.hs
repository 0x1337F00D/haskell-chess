{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StrictData #-}

module Chess.Core.Game.Internal where

import Chess.Core.Board.Internal
import qualified Chess.Board.Base as Base
import Data.Word (Word8)
import Data.Bits ((.|.), (.&.), complement, testBit, setBit, clearBit)
import Chess.Bitboard (Bitboard)

-- 3. Game Phases as Type States

-- Phases
data Phase = Setup | Active | Finished
  deriving (Eq, Show)

-- Variants
data Variant = Standard | Atomic | KingOfTheHill | RacingKings | ThreeCheck | Crazyhouse | FischerRandom | Antichess | Horde
  deriving (Eq, Show)

-- FischerRandom State
data FischerRandomState = FischerRandomState
  { whiteRookFiles :: Bitboard
  , blackRookFiles :: Bitboard
  } deriving (Eq, Show)

-- Crazyhouse State
data Pockets = Pockets
  { pocketPawns   :: Int
  , pocketKnights :: Int
  , pocketBishops :: Int
  , pocketRooks   :: Int
  , pocketQueens  :: Int
  } deriving (Eq, Show)

emptyPockets :: Pockets
emptyPockets = Pockets 0 0 0 0 0

data CrazyhouseState = CrazyhouseState
  { whitePocket :: Pockets
  , blackPocket :: Pockets
  , promoted    :: Bitboard
  } deriving (Eq, Show)

-- Variant State Data Family
type family VariantState (v :: Variant) where
  VariantState 'ThreeCheck = (Int, Int) -- (White Checks, Black Checks)
  VariantState 'Crazyhouse = CrazyhouseState
  VariantState 'FischerRandom = FischerRandomState
  VariantState _ = ()

-- Check Status (Section 7)
data CheckStatus = Safe | Checked
  deriving (Eq, Show)

data SCheckStatus (s :: CheckStatus) where
  SSafe    :: SCheckStatus 'Safe
  SChecked :: SCheckStatus 'Checked

deriving instance Show (SCheckStatus s)
deriving instance Eq (SCheckStatus s)

data Outcome = Winner Color | Draw
  deriving (Eq, Show)

-- Castling Rights
-- Packed into a Word8
-- Bit 0: White King Side
-- Bit 1: White Queen Side
-- Bit 2: Black King Side
-- Bit 3: Black Queen Side
newtype CastlingRights = CastlingRights Word8
  deriving (Eq, Show)

castlingWhiteKingSide :: Word8
castlingWhiteKingSide = 0x1

castlingWhiteQueenSide :: Word8
castlingWhiteQueenSide = 0x2

castlingBlackKingSide :: Word8
castlingBlackKingSide = 0x4

castlingBlackQueenSide :: Word8
castlingBlackQueenSide = 0x8

-- 4. Turn Safety and Dynamic State

-- The Active Game State
-- Indexed by the current turn, check status, and variant.
data ActiveGame (v :: Variant) (turn :: Color) (status :: CheckStatus) = ActiveGame
  { internalBoard :: Base.Board
  , castlingRights :: CastlingRights
  , enPassantTarget :: Maybe File -- File of the pawn that moved two squares, if any
  , halfMoveClock :: Int
  , fullMoveNumber :: Int
  , variantState :: VariantState v
  , checkStatus :: SCheckStatus status
  }

deriving instance Show (VariantState v) => Show (ActiveGame v turn status)
deriving instance Eq (VariantState v) => Eq (ActiveGame v turn status)

-- The Game Container
data Game (v :: Variant) (p :: Phase) where
  -- Setup Phase: Allows arbitrary placement of pieces
  SetupGame :: Board -> Game v 'Setup

  -- Active Phase: The only phase where makeMove is callable
  InProgressGame :: KnownColor turn => ActiveGame v turn status -> Game v 'Active

  -- Finished Phase: Contains the result
  FinishedGame :: Outcome -> Game v 'Finished

deriving instance Show (VariantState v) => Show (Game v p)

-- | View the internal board as a high-level Board.
-- Useful for tests and visualization.
viewBoard :: ActiveGame v c s -> Board
viewBoard ag = case fromBaseBoard (internalBoard ag) of
                 Just b -> b
                 Nothing -> error "Internal board corruption: ActiveGame contains invalid Base.Board"
