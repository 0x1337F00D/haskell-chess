{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Chess.Types where

import Control.Exception (Exception)

import Data.List (elemIndex)
import Data.Char (toLower)
import qualified Data.Map as M
import qualified Data.Set as S

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

pieceSymbols :: M.Map PieceType Char
pieceSymbols = M.fromList [(pt, pieceSymbol pt) | pt <- pieceTypes]

pieceNames :: M.Map PieceType String
pieceNames = M.fromList [(pt, pieceName pt) | pt <- pieceTypes]

pieceSymbol :: PieceType -> Char
pieceSymbol Pawn = 'P'
pieceSymbol Knight = 'N'
pieceSymbol Bishop = 'B'
pieceSymbol Rook = 'R'
pieceSymbol Queen = 'Q'
pieceSymbol King = 'K'

pieceName :: PieceType -> String
pieceName Pawn = "pawn"
pieceName Knight = "knight"
pieceName Bishop = "bishop"
pieceName Rook = "rook"
pieceName Queen = "queen"
pieceName King = "king"

-- | Unicode symbols for pieces, keyed by color and piece type.
unicodePieceSymbols :: M.Map (Color, PieceType) Char
unicodePieceSymbols = M.fromList [((c, pt), unicodeSymbol c pt) | c <- colors, pt <- pieceTypes]

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
squareKnightDistance start goal = bfs S.empty [(sf,sr)] 0
  where
    (sf, sr) = (squareFile start, squareRank start)
    target = (squareFile goal, squareRank goal)

    bfs _ [] _ = 0  -- should not happen on an 8x8 board
    bfs visited frontier d
      | target `elem` frontier = d
      | otherwise =
          let visited' = S.union visited (S.fromList frontier)
              next = S.toList . S.fromList $
                       [ p | pos <- frontier, p <- knightSteps pos, not (S.member p visited') ]
          in bfs visited' next (d+1)

    knightSteps (f,r) =
      filter onBoard [ (f+1,r+2), (f+2,r+1), (f+2,r-1), (f+1,r-2)
                     , (f-1,r-2), (f-2,r-1), (f-2,r+1), (f-1,r+2) ]

    onBoard (f,r) = f >= 0 && f < 8 && r >= 0 && r < 8


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
squareName (Square n) = [fileNames !! file, rankNames !! rank]
  where
    file = n `mod` 8
    rank = n `div` 8

-- | Parse square from algebraic notation.
parseSquare :: String -> Maybe Square
parseSquare [f,r] = do
  file <- elemIndex f fileNames
  rank <- elemIndex r rankNames
  square (rank*8 + file)
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
fromSymbol ch = case ch of
    'P' -> Just (Piece White Pawn)
    'N' -> Just (Piece White Knight)
    'B' -> Just (Piece White Bishop)
    'R' -> Just (Piece White Rook)
    'Q' -> Just (Piece White Queen)
    'K' -> Just (Piece White King)
    'p' -> Just (Piece Black Pawn)
    'n' -> Just (Piece Black Knight)
    'b' -> Just (Piece Black Bishop)
    'r' -> Just (Piece Black Rook)
    'q' -> Just (Piece Black Queen)
    'k' -> Just (Piece Black King)
    _   -> Nothing

-- | Representation of a move. Only basic coordinates and optional
-- promotion or drop piece are stored.
data Move = Move
    { fromSquare  :: Maybe Square
    , toSquare    :: Maybe Square
    , promotion   :: Maybe PieceType
    , dropPiece   :: Maybe PieceType
    } deriving (Eq, Ord, Show)

-- | The null move (does nothing).
nullMove :: Move
nullMove = Move Nothing Nothing Nothing Nothing

isNullMove :: Move -> Bool
isNullMove m = fromSquare m == Nothing && toSquare m == Nothing

uci :: Move -> String
uci (Move (Just f) (Just t) promo _) =
    squareName f ++ squareName t ++ maybe "" (\p -> [toLower (pieceSymbol p)]) promo
uci _ = ""

-- | Parse a move in long algebraic UCI form like "e2e4" or "e7e8q".
fromUci :: String -> Maybe Move
fromUci str = case splitAt 2 str of
    (f,tRest) -> do
        fromSq <- parseSquare f
        let (t, promoStr) = splitAt 2 tRest
        toSq <- parseSquare t
        let promo = case promoStr of
                [c] -> charToPieceType c
                _   -> Nothing
        return $ Move (Just fromSq) (Just toSq) promo Nothing
