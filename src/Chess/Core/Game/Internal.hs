{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Chess.Core.Game.Internal where

import Chess.Core.Board.Internal
import qualified Chess.Board.Base as Base
import Data.Map (Map)
import Data.Set (Set)

-- 3. Game Phases as Type States

-- Phases
data Phase = Setup | Active | Finished
  deriving (Eq, Show)

-- Variants
data Variant = Standard | Atomic | KingOfTheHill | RacingKings | ThreeCheck | Crazyhouse | Antichess | Horde | FischerRandom
  deriving (Eq, Show)

-- Variant State Data Family
type family VariantState (v :: Variant) where
  VariantState 'ThreeCheck = (Int, Int) -- (White Checks, Black Checks)
  VariantState 'Crazyhouse = (Map PieceType Int, Map PieceType Int, Set Square) -- (White Pocket, Black Pocket, Promoted Pieces)
  VariantState 'FischerRandom = (Maybe Square, Maybe Square, Maybe Square, Maybe Square) -- (WK, WQ, BK, BQ rook squares)
  VariantState _ = ()

-- Check Status (Section 7)
data CheckStatus = Safe | Checked
  deriving (Eq, Show)

data Outcome = Winner Color | Draw
  deriving (Eq, Show)

-- Castling Rights
data CastlingRights = CastlingRights
  { whiteKingSide :: Bool
  , whiteQueenSide :: Bool
  , blackKingSide :: Bool
  , blackQueenSide :: Bool
  } deriving (Show, Eq)

-- 4. Turn Safety and Dynamic State

-- The Active Game State
-- Indexed by the current turn, check status, and variant.
data ActiveGame (v :: Variant) (turn :: Color) (status :: CheckStatus) = ActiveGame
  { gameBoard :: Board
  , internalBoard :: Base.Board
  , castlingRights :: CastlingRights
  , enPassantTarget :: Maybe File -- File of the pawn that moved two squares, if any
  , halfMoveClock :: Int
  , fullMoveNumber :: Int
  , variantState :: VariantState v
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
