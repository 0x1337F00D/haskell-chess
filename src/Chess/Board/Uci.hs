module Chess.Board.Uci where

import Control.Arrow
import Data.Char (toLower)
import Chess.Types

-- | Convert a move to UCI string.
uci :: Move -> String
uci (Move (Just f) (Just t) promo _) =
    squareName f ++ squareName t ++ maybe "" (\p -> [toLower (pieceSymbol p)]) promo
uci _ = ""

-- | Parse a move in long algebraic UCI form like "e2e4" or "e7e8q".
-- Implemented using Arrows for composability.
fromUci :: String -> Maybe Move
fromUci = runKleisli $
    Kleisli (Just . splitAt 2)
    >>> first (Kleisli parseSquare)
    >>> second (
          Kleisli (Just . splitAt 2)
          >>> first (Kleisli parseSquare)
          >>> second (arr parsePromo)
        )
    >>> arr (\(f, (t, p)) -> Move (Just f) (Just t) p Nothing)
  where
    parsePromo :: String -> Maybe PieceType
    parsePromo [c] = charToPieceType c
    parsePromo _   = Nothing
