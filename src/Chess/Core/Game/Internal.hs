{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

module Chess.Core.Game.Internal where

import Chess.Core.Board.Internal

-- 3. Game Phases as Type States

-- Phases
data Phase = Setup | Active | Finished
  deriving (Eq, Show)

-- Variants
data Variant = Standard | Atomic | KingOfTheHill | RacingKings | ThreeCheck
  deriving (Eq, Show)

-- Variant Data
type family VariantData (v :: Variant) where
  VariantData 'Standard = ()
  VariantData 'Atomic = ()
  VariantData 'KingOfTheHill = ()
  VariantData 'RacingKings = ()
  VariantData 'ThreeCheck = (Int, Int) -- (White Checks, Black Checks)

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
  , castlingRights :: CastlingRights
  , enPassantTarget :: Maybe File -- File of the pawn that moved two squares, if any
  , halfMoveClock :: Int
  , fullMoveNumber :: Int
  , variantData :: VariantData v
  }

deriving instance (Show (VariantData v)) => Show (ActiveGame v turn status)
deriving instance (Eq (VariantData v)) => Eq (ActiveGame v turn status)

-- The Game Container
data Game (v :: Variant) (p :: Phase) where
  -- Setup Phase: Allows arbitrary placement of pieces
  SetupGame :: Board -> Game v 'Setup

  -- Active Phase: The only phase where makeMove is callable
  InProgressGame :: ActiveGame v turn status -> Game v 'Active

  -- Finished Phase: Contains the result
  FinishedGame :: Outcome -> Game v 'Finished

deriving instance Show (Game v p)
