module Chess.Core.Fen where

import Chess.Board.Fen (fen)
import qualified Chess.Board.Fen as Fen
import Chess.Board.Base (Board)
import Chess.Board.GameState (GameState)
import Text.Read (readMaybe)
import Data.List (stripPrefix)

-- | Parse ThreeCheck FEN string.
-- Returns Board, GameState, and (WhiteChecks, BlackChecks)
parseThreeCheckFen :: String -> Maybe (Board, GameState, (Int, Int))
parseThreeCheckFen s = do
  let parts = words s
  if length parts >= 4 then do
      let (pre, post) = splitAt 4 parts
      -- pre = [board, turn, castle, ep]

      let (checksStr, rest) = case post of
             (x:xs) | '+' `elem` x -> (Just x, xs)
             _ -> (Nothing, post)

      -- Reconstruct standard FEN part for generic parsing
      let standardFen = unwords (pre ++ rest)
      (b, gs) <- Fen.parseFen standardFen

      checks <- case checksStr of
          Just cs -> parseThreeCheckExtra cs
          Nothing -> return (0, 0)

      return (b, gs, checks)
  else Nothing

parseThreeCheckExtra :: String -> Maybe (Int, Int)
parseThreeCheckExtra s = do
  -- Format: +W+B or 3+3 (PyChess uses 3+3 without + prefix sometimes? No, lboard uses checksChr.split('+'))
  -- PyChess output from PyChessData seems to be "2+2".
  -- parseThreeCheckExtra original implementation expects prefix "+".
  -- Let's support both "2+2" and "+2+2".

  let s' = if head s == '+' then tail s else s
  let (wStr, rest) = break (== '+') s'
  rest2 <- stripPrefix "+" rest
  w <- readMaybe wStr
  b <- readMaybe rest2
  return (w, b)

-- | Serialize ThreeCheck FEN.
threeCheckFen :: Board -> GameState -> (Int, Int) -> String
threeCheckFen b gs (wChecks, bChecks) =
  let baseFen = fen b gs
  in baseFen ++ " +" ++ show wChecks ++ "+" ++ show bChecks
