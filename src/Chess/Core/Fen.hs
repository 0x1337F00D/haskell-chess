module Chess.Core.Fen where

import Chess.Board.Fen (parseFenRest, fen)
import Chess.Board.Base (Board)
import Chess.Board.GameState (GameState)
import Text.Read (readMaybe)
import Data.List (stripPrefix)

-- | Parse ThreeCheck FEN string.
-- Returns Board, GameState, and (WhiteChecks, BlackChecks)
parseThreeCheckFen :: String -> Maybe (Board, GameState, (Int, Int))
parseThreeCheckFen s = do
  (b, gs, extra) <- parseFenRest s
  checks <- case extra of
              (cStr:_) -> parseThreeCheckExtra cStr
              _ -> Just (0, 0) -- Default to 0+0 if missing
  return (b, gs, checks)

parseThreeCheckExtra :: String -> Maybe (Int, Int)
parseThreeCheckExtra s = do
  -- Format: +W+B
  -- E.g. +0+0, +2+1
  rest <- stripPrefix "+" s
  let (wStr, rest2) = break (== '+') rest
  rest3 <- stripPrefix "+" rest2
  w <- readMaybe wStr
  b <- readMaybe rest3
  return (w, b)

-- | Serialize ThreeCheck FEN.
threeCheckFen :: Board -> GameState -> (Int, Int) -> String
threeCheckFen b gs (wChecks, bChecks) =
  let baseFen = fen b gs
  in baseFen ++ " +" ++ show wChecks ++ "+" ++ show bChecks
