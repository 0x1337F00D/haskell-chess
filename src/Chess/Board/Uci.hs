module Chess.Board.Uci where

import Data.Char (toLower)
import Chess.Types

-- | Convert a move to UCI string.
uci :: Move -> String
uci (Move (Just f) (Just t) promo _) =
    squareName f ++ squareName t ++ maybe "" (\p -> [toLower (pieceSymbol p)]) promo
uci _ = ""

-- | Parse a move in long algebraic UCI form like "e2e4" or "e7e8q".
fromUci :: String -> Maybe Move
fromUci str = case splitAt 2 str of
    (f,tRest) -> do
        fromSq <- parseSquare f
        let (t, promoStr) = splitAt 2 tRest
        toSq <- parseSquare t
        let promo = case promoStr of
                [c] -> charToPieceType c
                _   -> Nothing
        return $ Move (Just fromSq) (Just toSq) promo Nothing
