{-# LANGUAGE TupleSections #-}
module Chess.Board.Fen (
  parseFen,
  fen,
  initialBoard,
  initialGameState
) where

import Data.Char (isDigit, ord)
import Data.List (intercalate)
import Data.Bits ((.|.), testBit)
import Text.Read (readMaybe)

import Chess.Types
import Chess.Bitboard
import Chess.Board.Base (Board)
import qualified Chess.Board.Base as Board
import Chess.Board.GameState (GameState(..), CastlingRights, noCastling)
import qualified Chess.Board.GameState as GameState

-- | Parse a FEN string into a Board and GameState.
parseFen :: String -> Maybe (Board, GameState)
parseFen fenStr = case words fenStr of
  [placement, activeColor, castling, epSquareStr, halfmove, fullmove] -> do
    board <- parsePlacement placement
    turnColor <- parseColor activeColor
    rights <- parseCastling castling
    epSq <- parseEpSquare epSquareStr
    hm <- readMaybe halfmove
    fm <- readMaybe fullmove
    let gs = GameState
          { turn = turnColor
          , castlingRights = rights
          , epSquare = epSq
          , halfmoveClock = hm
          , fullmoveNumber = fm
          }
    return (board, gs)
  _ -> Nothing

parsePlacement :: String -> Maybe Board
parsePlacement str = go Board.empty 0 7 str
  where
    go b _ _ [] = Just b
    go b file rank (c:cs)
      | c == '/' = if file == 8 then go b 0 (rank - 1) cs else Nothing
      | isDigit c = let skip = ord c - ord '0'
                    in if file + skip <= 8
                       then go b (file + skip) rank cs
                       else Nothing
      | otherwise = case fromSymbol c of
          Just piece -> if file < 8
                        then let sq = rank * 8 + file
                                 sq' = case square sq of
                                         Just s -> s
                                         Nothing -> error "Invalid square index" -- Should be safe
                                 b' = Board.putPiece b sq' piece
                             in go b' (file + 1) rank cs
                        else Nothing
          Nothing -> Nothing

parseColor :: String -> Maybe Color
parseColor "w" = Just White
parseColor "b" = Just Black
parseColor _   = Nothing

parseCastling :: String -> Maybe CastlingRights
parseCastling "-" = Just noCastling
parseCastling str = go 0 str
  where
    go acc [] = Just acc
    go acc (c:cs) = case c of
      'K' -> go (acc .|. BB_H1) cs
      'Q' -> go (acc .|. BB_A1) cs
      'k' -> go (acc .|. BB_H8) cs
      'q' -> go (acc .|. BB_A8) cs
      _   -> Nothing

parseEpSquare :: String -> Maybe (Maybe Square)
parseEpSquare "-" = Just Nothing
parseEpSquare s   = Just (parseSquare s)

-- | Convert Board and GameState to FEN string.
fen :: Board -> GameState -> String
fen board gs = unwords
  [ placementFen board
  , colorFen (turn gs)
  , castlingFen (castlingRights gs)
  , epSquareFen (epSquare gs)
  , show (halfmoveClock gs)
  , show (fullmoveNumber gs)
  ]

placementFen :: Board -> String
placementFen board = intercalate "/" [ rankFen board r | r <- [7,6..0] ]

rankFen :: Board -> Int -> String
rankFen board rank = go 0 0
  where
    go :: Int -> Int -> String
    go file emptyCount
      | file == 8 = if emptyCount > 0 then show emptyCount else ""
      | otherwise =
          let sq = case square (rank * 8 + file) of
                     Just s -> s
                     Nothing -> error "Invalid square"
          in case Board.pieceAt board sq of
               Nothing -> go (file + 1) (emptyCount + 1)
               Just p  -> (if emptyCount > 0 then show emptyCount else "") ++ [symbol p] ++ go (file + 1) 0

colorFen :: Color -> String
colorFen White = "w"
colorFen Black = "b"

castlingFen :: CastlingRights -> String
castlingFen 0 = "-"
castlingFen cr =
  let rights = (if testBit cr (unSquare H1) then "K" else "") ++
               (if testBit cr (unSquare A1) then "Q" else "") ++
               (if testBit cr (unSquare H8) then "k" else "") ++
               (if testBit cr (unSquare A8) then "q" else "")
  in if null rights then "-" else rights

epSquareFen :: Maybe Square -> String
epSquareFen Nothing = "-"
epSquareFen (Just sq) = squareName sq

initialBoard :: Board
initialBoard = case parseFen startingFEN of
  Just (b, _) -> b
  Nothing -> Board.empty

initialGameState :: GameState
initialGameState = GameState.initialGameState
