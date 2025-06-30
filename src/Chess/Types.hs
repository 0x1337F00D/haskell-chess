{-# LANGUAGE PatternSynonyms #-}

module Chess.Types where

import Data.Maybe (fromMaybe)
import Data.List (elemIndex, elemIndices)
import qualified Data.Map as M

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
pieceSymbols = M.fromList
  [ (Pawn, 'P'), (Knight, 'N'), (Bishop, 'B')
  , (Rook, 'R'), (Queen, 'Q'), (King, 'K')
  ]

pieceNames :: M.Map PieceType String
pieceNames = M.fromList
  [ (Pawn, "pawn"), (Knight, "knight"), (Bishop, "bishop")
  , (Rook, "rook"), (Queen, "queen"), (King, "king")
  ]

pieceSymbol :: PieceType -> Char
pieceSymbol pt = fromMaybe '?' (M.lookup pt pieceSymbols)

pieceName :: PieceType -> String
pieceName pt = fromMaybe "" (M.lookup pt pieceNames)

-- | Unicode symbols for pieces, keyed by color and piece type.
unicodePieceSymbols :: M.Map (Color, PieceType) Char
unicodePieceSymbols = M.fromList
  [ ((White, Pawn), '♙'), ((White, Knight), '♘')
  , ((White, Bishop), '♗'), ((White, Rook), '♖')
  , ((White, Queen), '♕'), ((White, King), '♔')
  , ((Black, Pawn), '♟'), ((Black, Knight), '♞')
  , ((Black, Bishop), '♝'), ((Black, Rook), '♜')
  , ((Black, Queen), '♛'), ((Black, King), '♚')
  ]

unicodeSymbol :: Color -> PieceType -> Char
unicodeSymbol c pt =
  fromMaybe '?' (M.lookup (c, pt) unicodePieceSymbols)

-- | Squares represented as integers 0..63 (a1=0).
newtype Square = Square { unSquare :: Int }
  deriving (Eq, Ord)

instance Show Square where
  show = squareName

square :: Int -> Maybe Square
square n
  | 0 <= n && n < 64 = Just (Square n)
  | otherwise = Nothing

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
  where
    elemIndex x xs = case elemIndices x xs of
      (i:_) -> Just i
      []    -> Nothing

-- | Starting FEN string for standard chess.
startingFEN :: String
startingFEN = "rn1qkbnr/pppbpppp/8/1B1p4/8/8/PPPPPPPP/RNBQK1NR w KQkq - 0 1"
