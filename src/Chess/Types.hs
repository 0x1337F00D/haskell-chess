{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

module Chess.Types where

import Control.Exception (Exception)
import Control.Monad (liftM)

import Data.Char (toLower, chr, ord)
import Data.Word (Word16, Word64)
import Data.Bits
import Data.Coerce (coerce)
import Foreign.Storable (Storable(..))

import qualified Data.Vector.Generic         as G
import qualified Data.Vector.Generic.Mutable as M
import qualified Data.Vector.Unboxed         as U

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
  deriving newtype (Enum, Num, Real, Integral, Bits, Storable)

-- | Unboxing instances for Data.Vector.Unboxed
newtype instance U.MVector s Square = MV_Square (U.MVector s Int)
newtype instance U.Vector    Square = V_Square  (U.Vector    Int)

instance U.Unbox Square

instance M.MVector U.MVector Square where
  basicLength (MV_Square v) = M.basicLength v
  basicUnsafeSlice i n (MV_Square v) = MV_Square (M.basicUnsafeSlice i n v)
  basicOverlaps (MV_Square v1) (MV_Square v2) = M.basicOverlaps v1 v2
  basicUnsafeNew n = MV_Square `liftM` M.basicUnsafeNew n
  basicInitialize (MV_Square v) = M.basicInitialize v
  basicUnsafeReplicate n x = MV_Square `liftM` M.basicUnsafeReplicate n (coerce x)
  basicUnsafeRead (MV_Square v) i = coerce `liftM` M.basicUnsafeRead v i
  basicUnsafeWrite (MV_Square v) i x = M.basicUnsafeWrite v i (coerce x)
  basicClear (MV_Square v) = M.basicClear v
  basicSet (MV_Square v) x = M.basicSet v (coerce x)
  basicUnsafeCopy (MV_Square v1) (MV_Square v2) = M.basicUnsafeCopy v1 v2
  basicUnsafeMove (MV_Square v1) (MV_Square v2) = M.basicUnsafeMove v1 v2
  basicUnsafeGrow (MV_Square v) n = MV_Square `liftM` M.basicUnsafeGrow v n

instance G.Vector U.Vector Square where
  basicUnsafeFreeze (MV_Square v) = V_Square `liftM` G.basicUnsafeFreeze v
  basicUnsafeThaw (V_Square v) = MV_Square `liftM` G.basicUnsafeThaw v
  basicLength (V_Square v) = G.basicLength v
  basicUnsafeSlice i n (V_Square v) = V_Square (G.basicUnsafeSlice i n v)
  basicUnsafeIndexM (V_Square v) i = coerce `liftM` G.basicUnsafeIndexM v i
  basicUnsafeCopy (MV_Square mv) (V_Square v) = G.basicUnsafeCopy mv v
  elemseq _ = seq

instance Show Square where
  show = squareName

square :: Int -> Maybe Square
square n
  | 0 <= n && n < 64 = Just (Square n)
  | otherwise = Nothing

-- | Halfmove clock for the fifty-move rule (counts half-moves).
newtype HalfmoveClock = HalfmoveClock { unHalfmoveClock :: Int }
  deriving stock (Eq, Ord)
  deriving newtype (Enum, Num, Real, Integral, Show)

-- | Fullmove number (increments after Black's move).
newtype FullmoveNumber = FullmoveNumber { unFullmoveNumber :: Int }
  deriving stock (Eq, Ord)
  deriving newtype (Enum, Num, Real, Integral, Show)

-- | Search depth measured in plies.
newtype Depth = Depth { unDepth :: Int }
  deriving stock (Eq, Ord)

instance Show Depth where
  show = show . unDepth

mkDepth :: Int -> Depth
mkDepth = Depth

incDepth :: Depth -> Depth
incDepth (Depth d) = Depth (d + 1)

decDepth :: Depth -> Depth
decDepth (Depth d) = Depth (d - 1)

plusDepth :: Depth -> Depth -> Depth
plusDepth (Depth a) (Depth b) = Depth (a + b)

minusDepth :: Depth -> Depth -> Depth
minusDepth (Depth a) (Depth b) = Depth (a - b)

depthZero :: Depth
depthZero = Depth 0

depthOne :: Depth
depthOne = Depth 1

isZeroDepth :: Depth -> Bool
isZeroDepth (Depth d) = d <= 0

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

-- | Sentinel value for no square (used for En Passant)
pattern NoSquare :: Square
pattern NoSquare = Square 64

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
squareName (Square n)
  | n == 64 = "-"
  | otherwise = [chr (ord 'a' + file), chr (ord '1' + rank)]
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

-- | Representation of a move (Bit-Packed Word16).
-- Bits 0-5: To Square (0-63)
-- Bits 6-11: From Square (0-63)
-- Bits 12-14: Promotion/Piece Type
--             Standard (Bit 15=0):
--               0: Normal / Quiet / Capture
--               1: Promotion Knight
--               2: Promotion Bishop
--               3: Promotion Rook
--               4: Promotion Queen
--               7: Null Move
--             Drop (Bit 15=1):
--               0-5: Piece Type being dropped (Pawn..King)
-- Bit 15: Flag (0=Standard, 1=Drop)
newtype Move = MkMove Word16
    deriving stock (Eq, Ord)
    deriving newtype (Storable)

instance Show Move where
    show (Move f t p) = "Move " ++ show f ++ " " ++ show t ++ " " ++ show p
    show (DropMove p t) = "DropMove " ++ show p ++ " " ++ show t
    show NullMove = "NullMove"

-- | Unboxing instances for Data.Vector.Unboxed
newtype instance U.MVector s Move = MV_Move (U.MVector s Word16)
newtype instance U.Vector    Move = V_Move  (U.Vector    Word16)

instance U.Unbox Move

instance M.MVector U.MVector Move where
  basicLength (MV_Move v) = M.basicLength v
  basicUnsafeSlice i n (MV_Move v) = MV_Move (M.basicUnsafeSlice i n v)
  basicOverlaps (MV_Move v1) (MV_Move v2) = M.basicOverlaps v1 v2
  basicUnsafeNew n = MV_Move `liftM` M.basicUnsafeNew n
  basicInitialize (MV_Move v) = M.basicInitialize v
  basicUnsafeReplicate n x = MV_Move `liftM` M.basicUnsafeReplicate n (coerce x)
  basicUnsafeRead (MV_Move v) i = coerce `liftM` M.basicUnsafeRead v i
  basicUnsafeWrite (MV_Move v) i x = M.basicUnsafeWrite v i (coerce x)
  basicClear (MV_Move v) = M.basicClear v
  basicSet (MV_Move v) x = M.basicSet v (coerce x)
  basicUnsafeCopy (MV_Move v1) (MV_Move v2) = M.basicUnsafeCopy v1 v2
  basicUnsafeMove (MV_Move v1) (MV_Move v2) = M.basicUnsafeMove v1 v2
  basicUnsafeGrow (MV_Move v) n = MV_Move `liftM` M.basicUnsafeGrow v n

instance G.Vector U.Vector Move where
  basicUnsafeFreeze (MV_Move v) = V_Move `liftM` G.basicUnsafeFreeze v
  basicUnsafeThaw (V_Move v) = MV_Move `liftM` G.basicUnsafeThaw v
  basicLength (V_Move v) = G.basicLength v
  basicUnsafeSlice i n (V_Move v) = V_Move (G.basicUnsafeSlice i n v)
  basicUnsafeIndexM (V_Move v) i = coerce `liftM` G.basicUnsafeIndexM v i
  basicUnsafeCopy (MV_Move mv) (V_Move v) = G.basicUnsafeCopy mv v
  elemseq _ = seq

-- | Construct a standard move.
pattern Move :: Square -> Square -> Maybe PieceType -> Move
pattern Move f t p <- (unpackMove -> Just (f, t, p))
  where Move f t p = mkMove f t p

-- | Construct a drop move.
pattern DropMove :: PieceType -> Square -> Move
pattern DropMove pt t <- (unpackDrop -> Just (pt, t))
  where DropMove pt t = mkDrop pt t

-- | Construct a null move.
pattern NullMove :: Move
pattern NullMove <- (unpackNull -> True)
  where NullMove = mkNull

{-# COMPLETE Move, DropMove, NullMove #-}

mkMove :: Square -> Square -> Maybe PieceType -> Move
mkMove (Square f) (Square t) p =
    let prom = case p of
                    Nothing -> 0
                    Just Knight -> 1
                    Just Bishop -> 2
                    Just Rook -> 3
                    Just Queen -> 4
                    Just _ -> 0
    in MkMove $ fromIntegral t .|. (fromIntegral f `shiftL` 6) .|. (prom `shiftL` 12)

unpackMove :: Move -> Maybe (Square, Square, Maybe PieceType)
unpackMove (MkMove w) =
    let flag = (w `shiftR` 15) .&. 1
        prom = (w `shiftR` 12) .&. 0x7
    in if flag == 1 || prom == 7 then Nothing -- Drop or Null
       else Just (Square (fromIntegral ((w `shiftR` 6) .&. 0x3F)),
                  Square (fromIntegral (w .&. 0x3F)),
                  case prom of
                    1 -> Just Knight
                    2 -> Just Bishop
                    3 -> Just Rook
                    4 -> Just Queen
                    _ -> Nothing)

mkDrop :: PieceType -> Square -> Move
mkDrop pt (Square t) =
    MkMove $ fromIntegral t .|. (fromIntegral (fromEnum pt) `shiftL` 12) .|. (1 `shiftL` 15)

unpackDrop :: Move -> Maybe (PieceType, Square)
unpackDrop (MkMove w) =
    let flag = (w `shiftR` 15) .&. 1
        ptVal = (w `shiftR` 12) .&. 0x7
    in if flag == 1
       then Just (toEnum (fromIntegral ptVal), Square (fromIntegral (w .&. 0x3F)))
       else Nothing

mkNull :: Move
mkNull = MkMove (7 `shiftL` 12)

unpackNull :: Move -> Bool
unpackNull (MkMove w) =
    let flag = (w `shiftR` 15) .&. 1
        prom = (w `shiftR` 12) .&. 0x7
    in flag == 0 && prom == 7

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

-- | Evaluation score in centipawns.
type Score = Int

-- | Packed Score (MG in upper 32 bits, EG in lower 32 bits).
type PackedScore = Int
