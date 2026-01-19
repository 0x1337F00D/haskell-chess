{-# LANGUAGE OverloadedStrings #-}

module Chess.Tablebase (
    SyzygyResult(..),
    WDL(..),
    probeSyzygy,
    probeOnline
) where

import System.Process (readProcess)
import Text.ParserCombinators.ReadP
import Data.Char (isDigit)
import Control.Exception (try, IOException)

data WDL = Win | Loss | Draw | CursedWin | BlessedLoss
    deriving (Show, Eq)

data SyzygyResult = SyzygyResult {
    srWDL :: WDL,
    srDTZ :: Int,
    srDTM :: Int
} deriving (Show, Eq)

-- | Probe a position (FEN). Currently defaults to online probing.
probeSyzygy :: String -> IO (Either String SyzygyResult)
probeSyzygy = probeOnline

-- | Probe the Lichess Online Tablebase API.
probeOnline :: String -> IO (Either String SyzygyResult)
probeOnline fen = do
    let url = "https://tablebase.lichess.ovh/standard?fen=" ++ mapSpace fen
    -- curl -s to be silent
    result <- try (readProcess "curl" ["-s", url] "") :: IO (Either IOException String)
    case result of
        Left err -> return $ Left $ "Network error: " ++ show err
        Right response -> return $ parseResponse response

mapSpace :: String -> String
mapSpace [] = []
mapSpace (' ':xs) = "%20" ++ mapSpace xs
mapSpace (x:xs) = x : mapSpace xs

parseResponse :: String -> Either String SyzygyResult
parseResponse json =
    case readP_to_S parseJson json of
        ((res, _):_) -> Right res
        _ -> Left "Failed to parse JSON response"

-- | A very specific parser for the Lichess API response
-- We look for "dtz", "dtm", and "category" keys in any order.
parseJson :: ReadP SyzygyResult
parseJson = do
    _ <- char '{'
    skipSpaces
    fields <- sepBy parseField (char ',' >> skipSpaces)
    _ <- char '}'

    let lookupField k = lookup k fields

    case (lookupField "dtz", lookupField "dtm", lookupField "category") of
        (Just (IntVal dtz), Just (IntVal dtm), Just (StringVal cat)) ->
            return $ SyzygyResult (parseCategory cat) dtz dtm
        _ -> pfail

data JsonValue = IntVal Int | StringVal String | BoolVal Bool | NullVal | ListVal | ObjVal
    deriving (Show)

parseField :: ReadP (String, JsonValue)
parseField = do
    key <- parseString
    skipSpaces
    _ <- char ':'
    skipSpaces
    val <- parseValue
    return (key, val)

parseValue :: ReadP JsonValue
parseValue =
    (IntVal <$> parseInt) +++
    (StringVal <$> parseString) +++
    (parseBool) +++
    (string "null" >> return NullVal) +++
    (parseList >> return ListVal) +++ -- We ignore list content for now
    (parseObj >> return ObjVal)       -- We ignore nested object content

parseString :: ReadP String
parseString = do
    _ <- char '"'
    s <- many (satisfy (\c -> c /= '"' && c /= '\\')) -- Simplified string parsing
    _ <- char '"'
    return s

parseInt :: ReadP Int
parseInt = do
    sign <- option 1 (char '-' >> return (-1))
    digits <- many1 (satisfy isDigit)
    return $ sign * read digits

parseBool :: ReadP JsonValue
parseBool = (string "true" >> return (BoolVal True)) +++ (string "false" >> return (BoolVal False))

parseList :: ReadP ()
parseList = do
    _ <- char '['
    _ <- many (satisfy (\c -> c /= ']')) -- Lazy skip
    _ <- char ']'
    return ()

parseObj :: ReadP ()
parseObj = do
    _ <- char '{'
    -- This is a bit weak for nested objects but sufficient for skipping if we don't need them
    -- A proper skipper would need to count braces.
    -- Since we only need top level fields, we might need a better skipper if "moves" contains objects.
    -- But "moves" is at the end usually.
    -- Let's improve the skipper to handle nested braces roughly.
    skipBalance
    return ()

skipBalance :: ReadP ()
skipBalance = do
    _ <- many (
           (satisfy (\c -> c /= '{' && c /= '}') >> return ()) +++
           (char '{' >> skipBalance)
         )
    _ <- char '}'
    return ()

parseCategory :: String -> WDL
parseCategory "win" = Win
parseCategory "loss" = Loss
parseCategory "draw" = Draw
parseCategory "cursed-win" = CursedWin
parseCategory "blessed-loss" = BlessedLoss
parseCategory _ = Draw -- Fallback
