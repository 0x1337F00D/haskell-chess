module Chess.Core.Fen where

import Chess.Board.Fen (fen)
import qualified Chess.Board.Fen as Fen
import Chess.Board.Base (Board)
import Chess.Board.GameState (GameState)
import Text.Read (readMaybe)
import Data.List (stripPrefix, partition)

-- | Parse ThreeCheck FEN string.
-- Returns Board, GameState, and (WhiteChecks, BlackChecks)
parseThreeCheckFen :: String -> Maybe (Board, GameState, (Int, Int))
parseThreeCheckFen s = do
  let parts = words s
  if length parts >= 4 then do
      let (pre, post) = splitAt 4 parts
      -- pre = [board, turn, castle, ep]

      -- Find the part that looks like checks (contains '+')
      let (checkParts, otherParts) = partition (\x -> '+' `elem` x) post

      let checksStr = case checkParts of
                        [c] -> Just c
                        _ -> Nothing

      -- Reconstruct standard FEN part for generic parsing
      let standardFen = unwords (pre ++ otherParts)
      (b, gs) <- Fen.parseFen standardFen

      checks <- case checksStr of
          Just cs -> parseThreeCheckExtra cs
          Nothing -> return (0, 0)

      return (b, gs, checks)
  else Nothing

parseThreeCheckExtra :: String -> Maybe (Int, Int)
parseThreeCheckExtra s = do
  -- Format: +W+B or W+B
  let s' = if not (null s) && head s == '+' then tail s else s
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
