{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}

module Chess.Core.Board.Internal where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Bits (countTrailingZeros, popCount, clearBit)
import Data.Char (toLower)
import qualified Data.ByteString.Builder as B

import qualified Chess.Board.Fen as Fen
import qualified Chess.Board.Base as Base
import qualified Chess.Types as T
import Chess.Bitboard (Bitboard)

-- 1. Foundation: The Finite Space

data Color = White | Black
  deriving (Eq, Ord, Show, Enum, Bounded)

-- Opposite color function is useful
opposite :: Color -> Color
opposite White = Black
opposite Black = White

-- | Singleton for Color to allow type refinement
data SColor (c :: Color) where
  SWhite :: SColor 'White
  SBlack :: SColor 'Black

-- | Class to reify type-level Color to value-level Color
class KnownColor (c :: Color) where
  sColor :: SColor c

instance KnownColor 'White where sColor = SWhite
instance KnownColor 'Black where sColor = SBlack

-- | Helper to get value from class
colorVal :: forall c. KnownColor c => Color
colorVal = case sColor @c of
             SWhite -> White
             SBlack -> Black

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

-- UCI Helpers
showFile :: File -> String
showFile f = [toLower (head (show f))] -- "FileA" -> "a" roughly. Actually show FileA is "FileA".
                                       -- We want "a", "b", ...

-- Explicit implementation is safer
fileToString :: File -> String
fileToString FileA = "a"
fileToString FileB = "b"
fileToString FileC = "c"
fileToString FileD = "d"
fileToString FileE = "e"
fileToString FileF = "f"
fileToString FileG = "g"
fileToString FileH = "h"

rankToString :: Rank -> String
rankToString Rank1 = "1"
rankToString Rank2 = "2"
rankToString Rank3 = "3"
rankToString Rank4 = "4"
rankToString Rank5 = "5"
rankToString Rank6 = "6"
rankToString Rank7 = "7"
rankToString Rank8 = "8"

squareToString :: Square -> String
squareToString (Square f r) = fileToString f ++ rankToString r

-- Builder Variants

fileToBuilder :: File -> B.Builder
fileToBuilder FileA = B.char7 'a'
fileToBuilder FileB = B.char7 'b'
fileToBuilder FileC = B.char7 'c'
fileToBuilder FileD = B.char7 'd'
fileToBuilder FileE = B.char7 'e'
fileToBuilder FileF = B.char7 'f'
fileToBuilder FileG = B.char7 'g'
fileToBuilder FileH = B.char7 'h'

rankToBuilder :: Rank -> B.Builder
rankToBuilder Rank1 = B.char7 '1'
rankToBuilder Rank2 = B.char7 '2'
rankToBuilder Rank3 = B.char7 '3'
rankToBuilder Rank4 = B.char7 '4'
rankToBuilder Rank5 = B.char7 '5'
rankToBuilder Rank6 = B.char7 '6'
rankToBuilder Rank7 = B.char7 '7'
rankToBuilder Rank8 = B.char7 '8'

squareToBuilder :: Square -> B.Builder
squareToBuilder (Square f r) = fileToBuilder f <> rankToBuilder r

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

-- fromFEN implementation
fromFEN :: String -> Maybe Board
fromFEN s = do
  (baseBoard, _) <- Fen.parseFen s
  fromBaseBoard baseBoard

-- Helper to convert Base Square to Core Square
fromBaseSquare :: T.Square -> Square
fromBaseSquare (T.Square i) = Square (toEnum (i `mod` 8)) (toEnum (i `div` 8))

-- Convert Base.Board to Core.Board
fromBaseBoard :: Base.Board -> Maybe Board
fromBaseBoard bb = do
  -- Validate Kings: Exactly one per side
  if popCount (Base.whiteKings bb) /= 1 then Nothing else return ()
  if popCount (Base.blackKings bb) /= 1 then Nothing else return ()

  let wKingSq = fromBaseSquare (T.Square (countTrailingZeros (Base.whiteKings bb)))
  let bKingSq = fromBaseSquare (T.Square (countTrailingZeros (Base.blackKings bb)))

  -- Collect Pawns
  wPawns <- collectPawns (Base.whitePawns bb) White
  bPawns <- collectPawns (Base.blackPawns bb) Black
  let allPawns = Map.union wPawns bPawns

  -- Collect Pieces
  let wPieces = collectPieces bb White
  let bPieces = collectPieces bb Black

  return Board
    { whiteKing = wKingSq
    , blackKing = bKingSq
    , pawns = allPawns
    , whitePieces = wPieces
    , blackPieces = bPieces
    }

collectPawns :: Bitboard -> Color -> Maybe (Map (File, PawnRank) Color)
collectPawns bb c =
    let sqs = bitboardToSquares bb
        addPawn m (Square f r) = do
           pr <- toPawnRank r
           return $ Map.insert (f, pr) c m
    in foldM addPawn Map.empty sqs
  where
    foldM _ z [] = Just z
    foldM f z (x:xs) = do
      z' <- f z x
      foldM f z' xs

collectPieces :: Base.Board -> Color -> Map Square (MajorMinorPiece c)
collectPieces bb c =
    let
        queens = if c == White then Base.whiteQueens bb else Base.blackQueens bb
        rooks = if c == White then Base.whiteRooks bb else Base.blackRooks bb
        bishops = if c == White then Base.whiteBishops bb else Base.blackBishops bb
        knights = if c == White then Base.whiteKnights bb else Base.blackKnights bb

        insertPieces pt pieces m =
            foldr (\sq acc -> Map.insert (fromBaseSquare sq) pt acc) m (rawBitboardToSquares pieces)

        m1 = insertPieces MQueen queens Map.empty
        m2 = insertPieces MRook rooks m1
        m3 = insertPieces MBishop bishops m2
        m4 = insertPieces MKnight knights m3
    in m4

bitboardToSquares :: Bitboard -> [Square]
bitboardToSquares bb = map fromBaseSquare (rawBitboardToSquares bb)

rawBitboardToSquares :: Bitboard -> [T.Square]
rawBitboardToSquares bb
  | bb == 0 = []
  | otherwise =
      let i = countTrailingZeros bb
      in T.Square i : rawBitboardToSquares (clearBit bb i)


-- Existential wrapper for Piece
data SomePiece where
  SomePiece :: Piece c -> SomePiece

deriving instance Show SomePiece

-- Helper to convert specific pieces to MajorMinor if applicable
toMajorMinor :: Piece c -> Maybe (MajorMinorPiece c)
toMajorMinor WQueen = Just MQueen
toMajorMinor WRook = Just MRook
toMajorMinor WBishop = Just MBishop
toMajorMinor WKnight = Just MKnight
toMajorMinor BQueen = Just MQueen
toMajorMinor BRook = Just MRook
toMajorMinor BBishop = Just MBishop
toMajorMinor BKnight = Just MKnight
toMajorMinor _ = Nothing

pieceColor :: Piece c -> Color
pieceColor WKing = White
pieceColor WQueen = White
pieceColor WRook = White
pieceColor WBishop = White
pieceColor WKnight = White
pieceColor WPawn = White
pieceColor BKing = Black
pieceColor BQueen = Black
pieceColor BRook = Black
pieceColor BBishop = Black
pieceColor BKnight = Black
pieceColor BPawn = Black

pieceType :: Piece c -> PieceType
pieceType WKing = King
pieceType WQueen = Queen
pieceType WRook = Rook
pieceType WBishop = Bishop
pieceType WKnight = Knight
pieceType WPawn = Pawn
pieceType BKing = King
pieceType BQueen = Queen
pieceType BRook = Rook
pieceType BBishop = Bishop
pieceType BKnight = Knight
pieceType BPawn = Pawn

instance Eq SomePiece where
  SomePiece p1 == SomePiece p2 =
    pieceColor p1 == pieceColor p2 && pieceType p1 == pieceType p2

-- Helper to convert Rank to PawnRank
toPawnRank :: Rank -> Maybe PawnRank
toPawnRank Rank2 = Just PRank2
toPawnRank Rank3 = Just PRank3
toPawnRank Rank4 = Just PRank4
toPawnRank Rank5 = Just PRank5
toPawnRank Rank6 = Just PRank6
toPawnRank Rank7 = Just PRank7
toPawnRank _     = Nothing

-- Helper to convert Square to (File, PawnRank)
toPawnSquare :: Square -> Maybe (File, PawnRank)
toPawnSquare (Square f r) = case toPawnRank r of
  Just pr -> Just (f, pr)
  Nothing -> Nothing

-- Get piece at a square
getPieceAt :: Square -> Board -> Maybe SomePiece
getPieceAt sq b
  | whiteKing b == sq = Just (SomePiece WKing)
  | blackKing b == sq = Just (SomePiece BKing)
  | otherwise =
      case Map.lookup sq (whitePieces b) of
        Just MQueen  -> Just (SomePiece WQueen)
        Just MRook   -> Just (SomePiece WRook)
        Just MBishop -> Just (SomePiece WBishop)
        Just MKnight -> Just (SomePiece WKnight)
        Nothing ->
          case Map.lookup sq (blackPieces b) of
            Just MQueen  -> Just (SomePiece BQueen)
            Just MRook   -> Just (SomePiece BRook)
            Just MBishop -> Just (SomePiece BBishop)
            Just MKnight -> Just (SomePiece BKnight)
            Nothing ->
               -- Check pawns
               case toPawnSquare sq of
                 Just psq -> case Map.lookup psq (pawns b) of
                               Just White -> Just (SomePiece WPawn)
                               Just Black -> Just (SomePiece BPawn)
                               Nothing -> Nothing
                 Nothing -> Nothing

-- Remove piece at square
removePieceAt :: Square -> Board -> Board
removePieceAt sq b = b
  { whitePieces = Map.delete sq (whitePieces b)
  , blackPieces = Map.delete sq (blackPieces b)
  , pawns = case toPawnSquare sq of
              Just psq -> Map.delete psq (pawns b)
              Nothing -> pawns b
  -- We don't remove Kings (invariant)
  }

-- Put piece at square (overwriting)
putPieceAt :: Square -> SomePiece -> Board -> Board
putPieceAt sq (SomePiece p) b =
  let b' = removePieceAt sq b -- Ensure square is empty first
  in case p of
       WKing -> b' { whiteKing = sq }
       BKing -> b' { blackKing = sq }
       WPawn -> case toPawnSquare sq of
                  Just psq -> b' { pawns = Map.insert psq White (pawns b') }
                  Nothing -> b' -- Invalid pawn placement, ignore or error?
       BPawn -> case toPawnSquare sq of
                  Just psq -> b' { pawns = Map.insert psq Black (pawns b') }
                  Nothing -> b'
       WQueen  -> b' { whitePieces = Map.insert sq MQueen (whitePieces b') }
       WRook   -> b' { whitePieces = Map.insert sq MRook (whitePieces b') }
       WBishop -> b' { whitePieces = Map.insert sq MBishop (whitePieces b') }
       WKnight -> b' { whitePieces = Map.insert sq MKnight (whitePieces b') }
       BQueen  -> b' { blackPieces = Map.insert sq MQueen (blackPieces b') }
       BRook   -> b' { blackPieces = Map.insert sq MRook (blackPieces b') }
       BBishop -> b' { blackPieces = Map.insert sq MBishop (blackPieces b') }
       BKnight -> b' { blackPieces = Map.insert sq MKnight (blackPieces b') }

-- Move piece
movePiece :: Square -> Square -> Board -> Board
movePiece from to b =
  case getPieceAt from b of
    Nothing -> b
    Just sp -> putPieceAt to sp (removePieceAt from b)
