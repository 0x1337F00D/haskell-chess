module Chess.Pgn (
    Game(..),
    parsePgn,
    readGameMoves,
    gameToUci
) where

import Text.ParserCombinators.ReadP
import Data.Char (isSpace)
import qualified Chess.Board as Board
import Chess.Types (Move)

data Game = Game
  { tags :: [(String, String)]
  , moves :: [String]
  , result :: String
  } deriving (Show, Eq)

parsePgn :: String -> Either String [Game]
parsePgn input =
    case readP_to_S (skipSpaces >> parseGames <* skipSpaces <* eof) input of
        [] -> Left "Failed to parse PGN"
        ((games, _):_) -> Right games -- Take the first valid full parse

parseGames :: ReadP [Game]
parseGames = sepBy parseGame skipSpaces

parseGame :: ReadP Game
parseGame = do
    ts <- parseTags
    (ms, res) <- parseMoveText
    return $ Game ts ms res

parseTags :: ReadP [(String, String)]
parseTags = many parseTag

parseTag :: ReadP (String, String)
parseTag = do
    skipSpaces
    _ <- char '['
    key <- munch1 (\c -> c /= ' ' && c /= ']')
    skipSpaces
    _ <- char '"'
    val <- munch (\c -> c /= '"')
    _ <- char '"'
    skipSpaces
    _ <- char ']'
    return (key, val)

parseMoveText :: ReadP ([String], String)
parseMoveText = do
    -- Parse tokens: move numbers, SANs, comments, RAVs, result
    tokens <- many1 parseToken
    let (ms, res) = extractMovesAndResult tokens
    -- Enforce that we consumed a Result token to avoid ambiguity with optional tags of next game
    -- We filter out comments/ravs from the end to find the result.
    let meaningfulTokens = dropWhile isCommentOrRav (reverse tokens)
    case meaningfulTokens of
        (Result _:_) -> return (ms, res)
        _ -> pfail

data Token = MoveNum String | San String | Result String | Comment | Rav deriving (Show)

isCommentOrRav :: Token -> Bool
isCommentOrRav Comment = True
isCommentOrRav Rav = True
isCommentOrRav _ = False

parseToken :: ReadP Token
parseToken = do
    skipSpaces
    parseTokenContent

parseTokenContent :: ReadP Token
parseTokenContent =
    (parseComment >> return Comment)
    <++ (parseRav >> return Rav)
    <++ (parseEscapeLine >> return Comment)
    <++ (parseLineComment >> return Comment)
    <++ parseRealToken

parseComment :: ReadP ()
parseComment = do
    _ <- char '{'
    _ <- munch (\c -> c /= '}')
    _ <- char '}'
    return ()

parseEscapeLine :: ReadP ()
parseEscapeLine = do
    _ <- char '%'
    _ <- munch (\c -> c /= '\n')
    return ()

parseLineComment :: ReadP ()
parseLineComment = do
    _ <- char ';'
    _ <- munch (\c -> c /= '\n')
    return ()

parseRav :: ReadP ()
parseRav = do
    _ <- char '('
    scanRav
    return ()

scanRav :: ReadP ()
scanRav = do
    c <- get
    case c of
        ')' -> return ()
        '(' -> scanRav >> scanRav
        _   -> scanRav

parseRealToken :: ReadP Token
parseRealToken = do
    -- Read a word. Allow digits, letters, symbols commonly in SAN/Result.
    -- Stop at space, or comment delimiters.
    s <- munch1 (\c -> not (isSpace c) && c `notElem` "{}()[]%;")
    if isResult s then return (Result s)
    else if last s == '.' then return (MoveNum s)
    else return (San s)

isResult :: String -> Bool
isResult s = s `elem` ["1-0", "0-1", "1/2-1/2", "*"]

extractMovesAndResult :: [Token] -> ([String], String)
extractMovesAndResult tokens =
    let realMoves = [s | San s <- tokens]
        res = case reverse tokens of
                (Result r : _) -> r
                _ -> "*" -- default
    in (realMoves, res)

-- | Converts a Game into a list of Moves by simulating the game.
-- Handles both SAN and UCI move formats.
-- Respects the "FEN" tag for initial position.
readGameMoves :: Game -> Either String [Move]
readGameMoves game = do
    let initialPos = case lookup "FEN" (tags game) of
            Just fenStr -> case Board.parseFen fenStr of
                Just b -> b
                Nothing -> Board.initialBoard -- Fallback to initial board if FEN invalid
            Nothing -> Board.initialBoard

    playMoves initialPos (moves game)

playMoves :: Board.Board -> [String] -> Either String [Move]
playMoves _ [] = Right []
playMoves b (mStr:rest) =
    let parsed = case Board.parseSan b mStr of
                    Just m -> Just m
                    Nothing -> Board.fromUci mStr
    in case parsed of
        Just m ->
            let b' = Board.applyMove b m
            in (m :) <$> playMoves b' rest
        Nothing -> Left $ "Invalid move: " ++ mStr

-- | Converts a Game into a list of UCI move strings.
gameToUci :: Game -> Either String [String]
gameToUci game = map Board.uci <$> readGameMoves game
