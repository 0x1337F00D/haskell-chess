{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Chess.Core.Game where

import Chess.Core.Board

-- 3. Game Phases as Type States

-- Phases
data Phase = Setup | Active | Finished
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
-- Indexed by the current turn and check status.
data ActiveGame (turn :: Color) (status :: CheckStatus) = ActiveGame
  { gameBoard :: Board
  , castlingRights :: CastlingRights
  , enPassantTarget :: Maybe File -- File of the pawn that moved two squares, if any
  , halfMoveClock :: Int
  , fullMoveNumber :: Int
  } deriving (Show, Eq)

-- The Game Container
data Game (p :: Phase) where
  -- Setup Phase: Allows arbitrary placement of pieces
  SetupGame :: Board -> Game 'Setup

  -- Active Phase: The only phase where makeMove is callable
  -- We wrap ActiveGame. Since 'turn' and 'status' are hidden types here,
  -- we might need an existential wrapper if we want `Game 'Active` to be a concrete type
  -- without parameters.
  -- "data Game (p :: Phase) ..." implies p is the only param.
  -- So InProgressGame must hide 'turn' and 'status'.
  InProgressGame :: ActiveGame turn status -> Game 'Active

  -- Finished Phase: Contains the result
  FinishedGame :: Outcome -> Game 'Finished

deriving instance Show (Game p)
