module Chess.Board.Uci where

import Control.Arrow
import Data.Char (toLower)
import Chess.Types

-- | Convert a move to UCI string.
uci :: Move -> String
uci (Move f t promo) =
    squareName f ++ squareName t ++ maybe "" (\p -> [toLower (pieceSymbol p)]) promo
uci NullMove = "0000" -- Standard UCI null move

-- | Parse a move in long algebraic UCI form like "e2e4" or "e7e8q".
-- Implemented using Arrows for composability.
fromUci :: String -> Maybe Move
fromUci "0000" = Just NullMove
fromUci s = runKleisli (
    Kleisli (Just . splitAt 2)
    >>> first (Kleisli parseSquare)
    >>> second (
          Kleisli (Just . splitAt 2)
          >>> first (Kleisli parseSquare)
          >>> second (arr parsePromo)
        )
    >>> arr (\(f, (t, p)) -> Move f t p)
    ) s
  where
    parsePromo :: String -> Maybe PieceType
    parsePromo [c] = charToPieceType c
    parsePromo _   = Nothing
