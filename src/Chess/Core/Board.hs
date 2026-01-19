{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Chess.Core.Board where

import Data.Map (Map)
import qualified Data.Map as Map

-- 1. Foundation: The Finite Space

data Color = White | Black
  deriving (Eq, Ord, Show, Enum, Bounded)

-- Opposite color function is useful
opposite :: Color -> Color
opposite White = Black
opposite Black = White

data File = FileA | FileB | FileC | FileD | FileE | FileF | FileG | FileH
  deriving (Eq, Ord, Show, Enum, Bounded)

data Rank = Rank1 | Rank2 | Rank3 | Rank4 | Rank5 | Rank6 | Rank7 | Rank8
  deriving (Eq, Ord, Show, Enum, Bounded)

data PawnRank = PRank2 | PRank3 | PRank4 | PRank5 | PRank6 | PRank7
  deriving (Eq, Ord, Show, Enum, Bounded)

-- Isomorphism between Rank and PawnRank for valid subset
toRank :: PawnRank -> Rank
toRank PRank2 = Rank2
toRank PRank3 = Rank3
toRank PRank4 = Rank4
toRank PRank5 = Rank5
toRank PRank6 = Rank6
toRank PRank7 = Rank7

-- The Board Topology
data Square = Square File Rank
  deriving (Eq, Ord, Show)

-- 2. The Physical Board: Structural Invariants

-- Piece Classification
data PieceType = King | Queen | Rook | Bishop | Knight | Pawn
  deriving (Eq, Ord, Show, Enum)

-- The Piece GADT
data Piece (c :: Color) where
  WKing   :: Piece 'White
  WQueen  :: Piece 'White
  WRook   :: Piece 'White
  WBishop :: Piece 'White
  WKnight :: Piece 'White
  WPawn   :: Piece 'White
  BKing   :: Piece 'Black
  BQueen  :: Piece 'Black
  BRook   :: Piece 'Black
  BBishop :: Piece 'Black
  BKnight :: Piece 'Black
  BPawn   :: Piece 'Black

deriving instance Show (Piece c)
deriving instance Eq (Piece c)

-- Helper for non-king, non-pawn pieces (Major/Minor pieces)
-- This corresponds to "NonKingPiece" in ARCHITECTURE.md, assuming Pawns are handled separately in PawnMap.
data MajorMinorPiece (c :: Color) where
  MQueen  :: MajorMinorPiece c
  MRook   :: MajorMinorPiece c
  MBishop :: MajorMinorPiece c
  MKnight :: MajorMinorPiece c

deriving instance Show (MajorMinorPiece c)
deriving instance Eq (MajorMinorPiece c)

-- The Composite Board Structure
data Board = Board
  { whiteKing   :: Square
  , blackKing   :: Square
  , pawns       :: Map (File, PawnRank) Color -- Tracks color of pawn at coordinate
  , whitePieces :: Map Square (MajorMinorPiece 'White)
  , blackPieces :: Map Square (MajorMinorPiece 'Black)
  } deriving (Show, Eq)

initialBoard :: Board
initialBoard = Board
  { whiteKing = Square FileE Rank1
  , blackKing = Square FileE Rank8
  , pawns = Map.fromList $
      [ ((f, PRank2), White) | f <- [FileA .. FileH] ] ++
      [ ((f, PRank7), Black) | f <- [FileA .. FileH] ]
  , whitePieces = Map.fromList
      [ (Square FileA Rank1, MRook)
      , (Square FileB Rank1, MKnight)
      , (Square FileC Rank1, MBishop)
      , (Square FileD Rank1, MQueen)
      , (Square FileH Rank1, MRook)
      , (Square FileG Rank1, MKnight)
      , (Square FileF Rank1, MBishop)
      ]
  , blackPieces = Map.fromList
      [ (Square FileA Rank8, MRook)
      , (Square FileB Rank8, MKnight)
      , (Square FileC Rank8, MBishop)
      , (Square FileD Rank8, MQueen)
      , (Square FileH Rank8, MRook)
      , (Square FileG Rank8, MKnight)
      , (Square FileF Rank8, MBishop)
      ]
  }

-- Stub for fromFEN
fromFEN :: String -> Maybe Board
fromFEN _ = Just initialBoard -- TODO: Implement proper FEN parsing
