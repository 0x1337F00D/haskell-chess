{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Chess.Core.Game.Internal where

import Chess.Core.Board.Internal
import qualified Chess.Board.Base as Base

-- 3. Game Phases as Type States

-- Phases
data Phase = Setup | Active | Finished
  deriving (Eq, Show)

-- Variants
data Variant = Standard | Atomic | KingOfTheHill | RacingKings
  deriving (Eq, Show)

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
  } deriving (Show, Eq)

-- The Game Container
data Game (v :: Variant) (p :: Phase) where
  -- Setup Phase: Allows arbitrary placement of pieces
  SetupGame :: Board -> Game v 'Setup

  -- Active Phase: The only phase where makeMove is callable
  InProgressGame :: ActiveGame v turn status -> Game v 'Active

  -- Finished Phase: Contains the result
  FinishedGame :: Outcome -> Game v 'Finished

deriving instance Show (Game v p)
