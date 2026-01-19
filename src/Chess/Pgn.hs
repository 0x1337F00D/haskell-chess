module Chess.Pgn (
    Game(..),
    PgnPly(..),
    parsePgn,
    readGameMoves,
    gameToUci
) where

import Text.ParserCombinators.ReadP
import Data.Char (isSpace, isDigit)
import qualified Chess.Board as Board
import Chess.Types (Move)

data Game = Game
  { tags :: [(String, String)]
  , plies :: [PgnPly]
  , result :: String
  } deriving (Show, Eq)

data PgnPly = PgnPly
  { plySan :: String
  , plyComment :: Maybe String
  , plyNags :: [Int]
  , plyRavs :: [[PgnPly]]
  } deriving (Show, Eq)

-- Tokenizer
data Token
    = MoveNum String
    | San String
    | Result String
    | Comment String
    | LineComment String
    | EscapeLine String
    | Nag Int
    | LParen
    | RParen
    | Unknown String
    deriving (Show, Eq)

parsePgn :: String -> Either String [Game]
parsePgn input =
    case readP_to_S (skipSpaces >> parseGames <* skipSpaces <* eof) input of
        [] -> Left "Failed to parse PGN"
        ((games, _):_) -> Right games

parseGames :: ReadP [Game]
parseGames = sepBy parseGame skipSpaces

parseGame :: ReadP Game
parseGame = do
    ts <- parseTags
    (ps, res) <- parseMoveText
    return $ Game ts ps res

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

parseMoveText :: ReadP ([PgnPly], String)
parseMoveText = do
    tokens <- tokenize
    let (ps, _, res) = parsePliesRecursive tokens
    return (ps, res)

tokenize :: ReadP [Token]
tokenize = do
    skipSpaces
    many parseToken

parseToken :: ReadP Token
parseToken = do
    skipSpaces
    (parseComment)
    <++ (parseLineComment)
    <++ (parseEscapeLine)
    <++ (parseRavStart)
    <++ (parseRavEnd)
    <++ (parseNag)
    <++ (parseRealToken)

parseComment :: ReadP Token
parseComment = do
    _ <- char '{'
    c <- munch (\c -> c /= '}')
    _ <- char '}'
    return $ Comment c

parseLineComment :: ReadP Token
parseLineComment = do
    _ <- char ';'
    c <- munch (\c -> c /= '\n')
    return $ LineComment c

parseEscapeLine :: ReadP Token
parseEscapeLine = do
    _ <- char '%'
    c <- munch (\c -> c /= '\n')
    return $ EscapeLine c

parseRavStart :: ReadP Token
parseRavStart = char '(' >> return LParen

parseRavEnd :: ReadP Token
parseRavEnd = char ')' >> return RParen

parseNag :: ReadP Token
parseNag = do
    _ <- char '$'
    n <- munch1 isDigit
    return $ Nag (read n)

parseRealToken :: ReadP Token
parseRealToken = do
    s <- munch1 (\c -> not (isSpace c) && c `notElem` "{}()[]%;$")
    if isResult s then return (Result s)
    else if last s == '.' then return (MoveNum s)
    else return (San s)

isResult :: String -> Bool
isResult s = s `elem` ["1-0", "0-1", "1/2-1/2", "*"]

-- Tree Parser

-- Returns (Parsed Plies, Remaining Tokens, Result)
parsePliesRecursive :: [Token] -> ([PgnPly], [Token], String)
parsePliesRecursive [] = ([], [], "*")
parsePliesRecursive (Result r : rest) = ([], rest, r)
parsePliesRecursive (RParen : rest) = ([], RParen : rest, "*") -- Stop at end of RAV
parsePliesRecursive (t:ts) =
    case t of
        MoveNum _ -> parsePliesRecursive ts
        San s ->
            let (nags, ts1) = extractNags ts
                (comment, ts2) = extractComment ts1
                (ravs, ts3) = extractRavs ts2
                (nextPlies, finalTs, res) = parsePliesRecursive ts3
            in (PgnPly s comment nags ravs : nextPlies, finalTs, res)
        -- Skip standalone annotations/comments not attached to a move (or attach to next?)
        -- For simplicity, skip them here.
        Comment _ -> parsePliesRecursive ts
        LineComment _ -> parsePliesRecursive ts
        EscapeLine _ -> parsePliesRecursive ts
        Nag _ -> parsePliesRecursive ts
        LParen ->
             -- Unexpected LParen (RAV without preceding move).
             -- Consume it to avoid infinite loop
             let (_, rest, _) = parsePliesRecursive ts
                 -- We called recursively on ts (which starts with something inside parens)
                 -- Wait, parsePliesRecursive stops at RParen.
                 -- If we are at LParen, we should call it on ts?
                 -- No, extractRavs handles LParen. If we hit it here, it's orphan.
                 -- We need to consume until matching RParen.
                 -- But parsePliesRecursive stops at RParen.
                 -- So we can just call it, then consume RParen.
                 (orphans, restAfterOrphan, _) = parsePliesRecursive ts
             in case restAfterOrphan of
                  (RParen : realRest) -> parsePliesRecursive realRest
                  _ -> parsePliesRecursive restAfterOrphan
        RParen -> ([], t:ts, "*")
        Unknown _ -> parsePliesRecursive ts

extractNags :: [Token] -> ([Int], [Token])
extractNags (Nag n : ts) =
    let (ns, rest) = extractNags ts
    in (n:ns, rest)
extractNags ts = ([], ts)

extractComment :: [Token] -> (Maybe String, [Token])
extractComment (Comment c : ts) = (Just c, ts)
extractComment (LineComment c : ts) = (Just c, ts)
extractComment (EscapeLine c : ts) = (Just c, ts)
extractComment ts = (Nothing, ts)

extractRavs :: [Token] -> ([[PgnPly]], [Token])
extractRavs (LParen : ts) =
    let (ravPlies, restAfterRav, _) = parsePliesRecursive ts
    in case restAfterRav of
            (RParen : rest) ->
                let (moreRavs, finalRest) = extractRavs rest
                in (ravPlies : moreRavs, finalRest)
            _ ->
                let (moreRavs, finalRest) = extractRavs restAfterRav
                in (ravPlies : moreRavs, finalRest)
extractRavs ts = ([], ts)


-- | Converts a Game into a list of Moves by simulating the game (Main Line).
readGameMoves :: Game -> Either String [Move]
readGameMoves game = do
    let initialPos = case lookup "FEN" (tags game) of
            Just fenStr -> case Board.parseFen fenStr of
                Just b -> b
                Nothing -> Board.initialBoard
            Nothing -> Board.initialBoard

    playMoves initialPos (map plySan (plies game))

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

-- | Converts a Game into a list of UCI move strings (Main Line).
gameToUci :: Game -> Either String [String]
gameToUci game = map Board.uci <$> readGameMoves game
