module Chess.Board.Uci where

import Data.Char (toLower)
import Chess.Types

-- | Convert a move to UCI string.
uci :: Move -> String
uci (Move f t promo) =
    squareName f ++ squareName t ++ maybe "" (\p -> [toLower (pieceSymbol p)]) promo
uci (DropMove p t) =
    [pieceSymbol p] ++ "@" ++ squareName t
uci NullMove = "0000" -- Standard UCI null move

-- | Parse a move in long algebraic UCI form like "e2e4" or "e7e8q".
fromUci :: String -> Maybe Move
fromUci "0000" = Just NullMove
fromUci s
    | '@' `elem` s =
        let (pStr, rest) = span (/= '@') s
        in case (pStr, rest) of
             ([p], '@':sqStr) -> do
                 pt <- charToPieceType p
                 sq <- parseSquare sqStr
                 return (DropMove pt sq)
             _ -> Nothing
    | length s == 4 = do
        f <- parseSquare (take 2 s)
        t <- parseSquare (drop 2 s)
        return (Move f t Nothing)
    | length s == 5 = do
        f <- parseSquare (take 2 s)
        t <- parseSquare (take 2 (drop 2 s))
        p <- charToPieceType (s !! 4)
        return (Move f t (Just p))
    | otherwise = Nothing
