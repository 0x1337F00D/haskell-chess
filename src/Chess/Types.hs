{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Chess.Types where

import Control.Exception (Exception)

import Data.Char (toLower, chr, ord)
import Data.Word (Word64)
import Data.Bits

-- | Color of a chess piece or side to move.
data Color = White | Black
  deriving (Eq, Ord, Enum, Bounded, Show)

-- | Convenience constants for colors.
pattern WHITE, BLACK :: Color
pattern WHITE = White
pattern BLACK = Black

-- | All colors.
colors :: [Color]
colors = [White, Black]

colorName :: Color -> String
colorName White = "white"
colorName Black = "black"

-- | Type of chess pieces.
data PieceType
  = Pawn | Knight | Bishop | Rook | Queen | King
  deriving (Eq, Ord, Enum, Bounded, Show)

pieceTypes :: [PieceType]
pieceTypes = [Pawn .. King]

pieceSymbol :: PieceType -> Char
pieceSymbol Pawn   = 'P'
pieceSymbol Knight = 'N'
pieceSymbol Bishop = 'B'
pieceSymbol Rook   = 'R'
pieceSymbol Queen  = 'Q'
pieceSymbol King   = 'K'

pieceName :: PieceType -> String
pieceName Pawn   = "pawn"
pieceName Knight = "knight"
pieceName Bishop = "bishop"
pieceName Rook   = "rook"
pieceName Queen  = "queen"
pieceName King   = "king"

unicodeSymbol :: Color -> PieceType -> Char
unicodeSymbol White Pawn   = '♙'
unicodeSymbol White Knight = '♘'
unicodeSymbol White Bishop = '♗'
unicodeSymbol White Rook   = '♖'
unicodeSymbol White Queen  = '♕'
unicodeSymbol White King   = '♔'
unicodeSymbol Black Pawn   = '♟'
unicodeSymbol Black Knight = '♞'
unicodeSymbol Black Bishop = '♝'
unicodeSymbol Black Rook   = '♜'
unicodeSymbol Black Queen  = '♛'
unicodeSymbol Black King   = '♚'

-- | Squares represented as integers 0..63 (a1=0).
newtype Square = Square { unSquare :: Int }
  deriving stock (Eq, Ord)
  deriving newtype (Enum)

instance Show Square where
  show = squareName

square :: Int -> Maybe Square
square n
  | 0 <= n && n < 64 = Just (Square n)
  | otherwise = Nothing

-- | Halfmove clock for the fifty-move rule (counts half-moves).
newtype HalfmoveClock = HalfmoveClock { unHalfmoveClock :: Int }
  deriving stock (Eq, Ord)
  deriving newtype (Enum, Num, Real, Integral)

instance Show HalfmoveClock where
  show = show . unHalfmoveClock

-- | Fullmove number (increments after Black's move).
newtype FullmoveNumber = FullmoveNumber { unFullmoveNumber :: Int }
  deriving stock (Eq, Ord)
  deriving newtype (Enum, Num, Real, Integral)

instance Show FullmoveNumber where
  show = show . unFullmoveNumber

-- | Search depth measured in plies.
newtype Depth = Depth { unDepth :: Int }
  deriving stock (Eq, Ord)
  deriving newtype (Enum, Num, Real, Integral)

instance Show Depth where
  show = show . unDepth

-- | Pattern synonyms for individual squares in A1..H8 order.
pattern A1, B1, C1, D1, E1, F1, G1, H1 :: Square
pattern A1 = Square 0
pattern B1 = Square 1
pattern C1 = Square 2
pattern D1 = Square 3
pattern E1 = Square 4
pattern F1 = Square 5
pattern G1 = Square 6
pattern H1 = Square 7

pattern A2, B2, C2, D2, E2, F2, G2, H2 :: Square
pattern A2 = Square 8
pattern B2 = Square 9
pattern C2 = Square 10
pattern D2 = Square 11
pattern E2 = Square 12
pattern F2 = Square 13
pattern G2 = Square 14
pattern H2 = Square 15

pattern A3, B3, C3, D3, E3, F3, G3, H3 :: Square
pattern A3 = Square 16
pattern B3 = Square 17
pattern C3 = Square 18
pattern D3 = Square 19
pattern E3 = Square 20
pattern F3 = Square 21
pattern G3 = Square 22
pattern H3 = Square 23

pattern A4, B4, C4, D4, E4, F4, G4, H4 :: Square
pattern A4 = Square 24
pattern B4 = Square 25
pattern C4 = Square 26
pattern D4 = Square 27
pattern E4 = Square 28
pattern F4 = Square 29
pattern G4 = Square 30
pattern H4 = Square 31

pattern A5, B5, C5, D5, E5, F5, G5, H5 :: Square
pattern A5 = Square 32
pattern B5 = Square 33
pattern C5 = Square 34
pattern D5 = Square 35
pattern E5 = Square 36
pattern F5 = Square 37
pattern G5 = Square 38
pattern H5 = Square 39

pattern A6, B6, C6, D6, E6, F6, G6, H6 :: Square
pattern A6 = Square 40
pattern B6 = Square 41
pattern C6 = Square 42
pattern D6 = Square 43
pattern E6 = Square 44
pattern F6 = Square 45
pattern G6 = Square 46
pattern H6 = Square 47

pattern A7, B7, C7, D7, E7, F7, G7, H7 :: Square
pattern A7 = Square 48
pattern B7 = Square 49
pattern C7 = Square 50
pattern D7 = Square 51
pattern E7 = Square 52
pattern F7 = Square 53
pattern G7 = Square 54
pattern H7 = Square 55

pattern A8, B8, C8, D8, E8, F8, G8, H8 :: Square
pattern A8 = Square 56
pattern B8 = Square 57
pattern C8 = Square 58
pattern D8 = Square 59
pattern E8 = Square 60
pattern F8 = Square 61
pattern G8 = Square 62
pattern H8 = Square 63

-- | All squares from A1 to H8.
squares :: [Square]
squares = [Square n | n <- [0..63]]

-- | Mirror a square by rotating the board 180 degrees.
squareMirror :: Square -> Square
squareMirror (Square n) = Square (63 - n)

-- | Precomputed table of mirrored squares.
squares180 :: [Square]
squares180 = map squareMirror squares

-- | Chebyshev distance between two squares.
squareDistance :: Square -> Square -> Int
squareDistance a b =
  max (abs (squareFile a - squareFile b)) (abs (squareRank a - squareRank b))

-- | Manhattan distance between two squares.
squareManhattanDistance :: Square -> Square -> Int
squareManhattanDistance a b =
  abs (squareFile a - squareFile b) + abs (squareRank a - squareRank b)

-- | Minimum number of knight moves between two squares using BFS.
squareKnightDistance :: Square -> Square -> Int
squareKnightDistance (Square start) (Square goal)
  | start == goal = 0
  | otherwise = bfs (bit start :: Word64) (bit start :: Word64) 0
  where
    targetBB = bit goal :: Word64

    bfs visited frontier d
      | (frontier .&. targetBB) /= 0 = d
      | frontier == 0 = 0
      | otherwise =
          let notA  = 0xfefefefefefefefe
              notH  = 0x7f7f7f7f7f7f7f7f
              notAB = 0xfcfcfcfcfcfcfcfc
              notGH = 0x3f3f3f3f3f3f3f3f

              nextFrontierPotential =
                  ((frontier `shiftL` 17) .&. notA) .|.
                  ((frontier `shiftL` 15) .&. notH) .|.
                  ((frontier `shiftL` 10) .&. notAB) .|.
                  ((frontier `shiftL` 6)  .&. notGH) .|.
                  ((frontier `shiftR` 17) .&. notH) .|.
                  ((frontier `shiftR` 15) .&. notA) .|.
                  ((frontier `shiftR` 10) .&. notGH) .|.
                  ((frontier `shiftR` 6)  .&. notAB)

              visited' = visited .|. frontier
              frontier' = nextFrontierPotential .&. complement visited'
          in bfs visited' frontier' (d+1)


squareFile :: Square -> Int
squareFile (Square n) = n `mod` 8

squareRank :: Square -> Int
squareRank (Square n) = n `div` 8

-- | File names and rank names.
fileNames :: [Char]
fileNames = ['a'..'h']

rankNames :: [Char]
rankNames = ['1'..'8']

-- | Convert square to algebraic notation (e.g. a1).
squareName :: Square -> String
squareName (Square n) = [chr (ord 'a' + file), chr (ord '1' + rank)]
  where
    file = n `mod` 8
    rank = n `div` 8

-- | Parse square from algebraic notation.
parseSquare :: String -> Maybe Square
parseSquare [f,r]
  | f >= 'a' && f <= 'h' && r >= '1' && r <= '8' =
      let file = ord f - ord 'a'
          rank = ord r - ord '1'
      in Just (Square (rank*8 + file))
  | otherwise = Nothing
parseSquare _ = Nothing

-- | Starting FEN string for standard chess.
startingBoardFEN :: String
startingBoardFEN = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR"

startingFEN :: String
startingFEN = startingBoardFEN ++ " w KQkq - 0 1"

-- | Status of a board after validation. Only a subset of the Python
-- chess library statuses are represented for now.
data Status
    = Valid
    | NoWhiteKing
    | NoBlackKing
    | TooManyPieces
    deriving (Eq, Show)

-- | Termination reason of a finished game.
data Termination
    = Normal
    | Checkmate
    | Stalemate
    | FiftyMoves
    | ThreefoldRepetition
    | FivefoldRepetition
    | InsufficientMaterial
    | Timeout
    | Resignation
    deriving (Eq, Show)

-- | Result of a finished game.
data Outcome = Outcome
    { outcomeTermination :: Termination
    , outcomeWinner :: Maybe Color
    } deriving (Eq, Show)

result :: Outcome -> String
result (Outcome _ (Just White)) = "1-0"
result (Outcome _ (Just Black)) = "0-1"
result _                        = "1/2-1/2"

-- | Error types for illegal or invalid moves.
data InvalidMoveError = InvalidMoveError String deriving (Show)
instance Exception InvalidMoveError

data IllegalMoveError = IllegalMoveError String deriving (Show)
instance Exception IllegalMoveError

data AmbiguousMoveError = AmbiguousMoveError String deriving (Show)
instance Exception AmbiguousMoveError

-- | A piece of a certain color and type.
data Piece = Piece
    { pieceColor :: Color
    , pieceType  :: PieceType
    } deriving (Eq, Ord, Show)

symbol :: Piece -> Char
symbol (Piece c pt) = case c of
    White -> pieceSymbol pt
    Black -> toLower (pieceSymbol pt)

unicodeSymbolPiece :: Piece -> Char
unicodeSymbolPiece (Piece c pt) = unicodeSymbol c pt

charToPieceType :: Char -> Maybe PieceType
charToPieceType c = case c of
  'P' -> Just Pawn
  'N' -> Just Knight
  'B' -> Just Bishop
  'R' -> Just Rook
  'Q' -> Just Queen
  'K' -> Just King
  'p' -> Just Pawn
  'n' -> Just Knight
  'b' -> Just Bishop
  'r' -> Just Rook
  'q' -> Just Queen
  'k' -> Just King
  _   -> Nothing

fromSymbol :: Char -> Maybe Piece
fromSymbol ch = do
    pt <- charToPieceType ch
    let col = if ch `elem` ['a'..'z'] then Black else White
    return (Piece col pt)

-- | Representation of a move. Standard move connects two squares and optional promotion.
-- Null move is a special case. Drop moves are used in Crazyhouse.
data Move
    = Move
      { mFrom :: !Square
      , mTo   :: !Square
      , mProm :: !(Maybe PieceType)
      }
    | DropMove
      { mDropPiece :: !PieceType
      , mTo        :: !Square
      }
    | NullMove
    deriving (Eq, Ord, Show)

-- | The null move (does nothing).
nullMove :: Move
nullMove = NullMove

isNullMove :: Move -> Bool
isNullMove NullMove = True
isNullMove _        = False

-- | Helper to access fromSquare safely (compatibility/helper)
fromSquare :: Move -> Maybe Square
fromSquare (Move f _ _) = Just f
fromSquare _            = Nothing

-- | Helper to access toSquare safely (compatibility/helper)
toSquare :: Move -> Maybe Square
toSquare (Move _ t _)   = Just t
toSquare (DropMove _ t) = Just t
toSquare NullMove       = Nothing

-- | Helper to access promotion safely (compatibility/helper)
promotion :: Move -> Maybe PieceType
promotion (Move _ _ p) = p
promotion _            = Nothing
