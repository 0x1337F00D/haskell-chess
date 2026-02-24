module Chess.Board.Fen where

import Control.Monad (foldM, guard)
import Data.Char (isDigit, ord, chr, toLower)
import Data.List (foldl', intercalate)
import Data.Bits ((.|.), testBit)
import Text.Read (readMaybe)

import Chess.Types
import Chess.Bitboard
import Chess.Board.Base (Board, statePacked, stateZobrist)
import qualified Chess.Board.Base as Board
import Chess.Board.GameState
import qualified Chess.Board.GameState as GS
import qualified Chess.Board.Zobrist as Zobrist

-- | Parse a FEN string into a Board and remaining parts.
parseFenRest :: String -> Maybe (Board, [String])
parseFenRest s = do
  let parts = words s
  guard (length parts >= 4)
  let (boardStrFull:turnStr:castlingStr:epStr:rest) = parts

      -- Extract pocket if attached to board string (e.g. "RNBQKBNR[P]")
      (boardStr, pocketPart) = span (/= '[') boardStrFull

      (halfmoveStr, fullmoveStr, extra) = case rest of
                                            (h:f:r) -> (h, f, r)
                                            [h] -> (h, "1", [])
                                            [] -> ("0", "1", [])

      extra' = if null pocketPart then extra else pocketPart : extra

  board <- parseBoard boardStr
  turnVal <- parseTurn turnStr
  castling <- parseCastling castlingStr
  ep <- parseEp epStr
  halfmove <- HalfmoveClock <$> readMaybe halfmoveStr
  fullmove <- FullmoveNumber <$> readMaybe fullmoveStr

  let sPacked = mkStatePacked turnVal castling ep halfmove fullmove
      bWithState = board { statePacked = sPacked }
      hash = Zobrist.computeHash bWithState
      bFinal = bWithState { stateZobrist = hash }

  return (bFinal, extra')

-- | Parse a FEN string into a Board.
parseFen :: String -> Maybe Board
parseFen s = do
  (b, _) <- parseFenRest s
  return b

-- | Serialize Board to FEN string.
fen :: Board -> String
fen board =
  let s = statePacked board
  in unwords
  [ showBoard board
  , showTurn (getTurn s)
  , showCastling (getCastlingRights s)
  , showEp (getEpSquare s)
  , show (getHalfmoveClock s)
  , show (getFullmoveNumber s)
  ]

-- Helper functions for parsing

parseBoard :: String -> Maybe Board
parseBoard s = do
  let ranks = splitOn '/' s
  if length ranks /= 8 then Nothing else do
    let rankSquares = zip [7,6..0] ranks
    foldM addRank Board.empty rankSquares
  where
    addRank b (r, str) = do
      pieces <- parseRank r str
      return $ foldl' (\acc (sq, p) -> Board.putPiece acc sq p) b pieces

    parseRank r str = go 0 str
      where
        go _ [] = Just []
        go f (c:cs)
          | f > 7 = Nothing -- too many files
          | isDigit c =
              let n = ord c - ord '0'
              in if n < 1 || n > 8 then Nothing else go (f + n) cs
          | otherwise = case fromSymbol c of
              Just p -> do
                rest <- go (f+1) cs
                return $ (Square (r*8 + f), p) : rest
              Nothing -> Nothing

-- | Split a list by a delimiter.
splitOn :: Eq a => a -> [a] -> [[a]]
splitOn delimiter = foldr f [[]]
  where f c l@(x:xs) | c == delimiter = [] : l
                     | otherwise = (c:x) : xs
        f _ [] = []

parseTurn :: String -> Maybe Color
parseTurn "w" = Just White
parseTurn "b" = Just Black
parseTurn _ = Nothing

parseCastling :: String -> Maybe CastlingRights
parseCastling "-" = Just GS.noCastling
parseCastling s = foldM addRight GS.noCastling s
  where
    addRight acc 'K' = Just (acc .|. BB_H1)
    addRight acc 'Q' = Just (acc .|. BB_A1)
    addRight acc 'k' = Just (acc .|. BB_H8)
    addRight acc 'q' = Just (acc .|. BB_A8)
    addRight acc c
      | c >= 'A' && c <= 'H' = Just (acc .|. bbFromSquare (Square (ord c - ord 'A')))
      | c >= 'a' && c <= 'h' = Just (acc .|. bbFromSquare (Square (ord c - ord 'a' + 56)))
      | otherwise = Nothing

parseEp :: String -> Maybe Square
parseEp "-" = Just NoSquare
parseEp s = parseSquare s

-- Helper functions for serialization

showBoard :: Board -> String
showBoard b = intercalate "/" [ showRank r | r <- [7,6..0] ]
  where
    showRank r = flushEmpty (0 :: Int) [0..7]
      where
        flushEmpty n [] = if n > 0 then show n else ""
        flushEmpty n (f:fs) =
          case Board.pieceAt b (Square (r*8 + f)) of
            Nothing -> flushEmpty (n+1) fs
            Just p -> (if n > 0 then show n else "") ++ [symbol p] ++ flushEmpty 0 fs

showTurn :: Color -> String
showTurn White = "w"
showTurn Black = "b"

showCastling :: CastlingRights -> String
showCastling cr
  | cr == GS.noCastling = "-"
  | otherwise =
      let whiteStr =
            (if testBit cr (unSquare H1) then "K" else "") ++
            (if testBit cr (unSquare A1) then "Q" else "") ++
            [ fileChar f | f <- [1..6], testBit cr (unSquare (Square f)) ]
          blackStr =
            (if testBit cr (unSquare H8) then "k" else "") ++
            (if testBit cr (unSquare A8) then "q" else "") ++
            [ toLower (fileChar f) | f <- [1..6], testBit cr (unSquare (Square (56+f))) ]
          res = whiteStr ++ blackStr
      in if null res then "-" else res
  where
    fileChar f = chr (ord 'A' + f)

showEp :: Square -> String
showEp = squareName
