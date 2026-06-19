{-# LANGUAGE LambdaCase #-}
module Chess.Pgn (
    Game(..),
    PgnNode(..),
    GameTree(..),
    GameNode(..),
    parsePgn,
    buildGameTree,
    readGameMoves,
    gameToUci,
    moves,
    showGame
) where

import Text.ParserCombinators.ReadP
import Data.Char (isSpace, isDigit)
import Data.List (intercalate)
import qualified Chess.Board as Board
import qualified Chess.Board.GameState as GS
import Chess.Types (Move)

-- | A tree of validated moves and positions.
data GameTree = GameTree
  { gtRoot :: Board.Board
  , gtChildren :: [GameNode]
  } deriving (Show, Eq)

-- | A node in the validated game tree.
data GameNode = GameNode
  { gnMove :: Move
  , gnTarget :: Board.Board -- ^ Position after move
  , gnSan :: String
  , gnComment :: Maybe String
  , gnNags :: [Int]
  , gnChildren :: [GameNode]
  } deriving (Show, Eq)

-- | A node in the PGN move tree.
data PgnNode = PgnNode
  { pnSan :: String
  , pnNags :: [Int]
  , pnComment :: Maybe String
  , pnVariations :: [[PgnNode]]
  } deriving (Show, Eq)

data Game = Game
  { tags :: [(String, String)]
  , forest :: [PgnNode]
  , result :: String
  } deriving (Show, Eq)

-- | Extract the main line moves as SAN strings.
moves :: Game -> [String]
moves g = map pnSan (forest g)

parsePgn :: String -> Either String [Game]
parsePgn input =
    case readP_to_S (skipSpaces >> parseGames <* skipSpaces <* eof) input of
        [] -> Left "Failed to parse PGN"
        matches ->
             -- Pick the first valid full parse
             Right (fst $ last matches)

parseGames :: ReadP [Game]
parseGames = sepBy parseGame skipSpaces

parseGame :: ReadP Game
parseGame = do
    ts <- parseTags
    skipSpaces
    -- Body consists of moves, comments, ravs, eventually ending in result.
    (nodes, res) <- parseBody
    return $ Game ts nodes res

parseTags :: ReadP [(String, String)]
parseTags = many parseTag

parseTag :: ReadP (String, String)
parseTag = do
    skipSpaces
    _ <- char '['
    key <- munch1 (\c -> c /= ' ' && c /= ']')
    skipSpaces
    val <- parseString
    skipSpaces
    _ <- char ']'
    return (key, val)

parseString :: ReadP String
parseString = do
    _ <- char '"'
    s <- many stringChar
    _ <- char '"'
    return s

stringChar :: ReadP Char
stringChar = escapedChar +++ normalChar

escapedChar :: ReadP Char
escapedChar = do
    _ <- char '\\'
    get

normalChar :: ReadP Char
normalChar = satisfy (\c -> c /= '"' && c /= '\\')

parseBody :: ReadP ([PgnNode], String)
parseBody = do
    nodes <- parseNodes
    skipSpaces
    res <- parseResult
    return (nodes, res)

parseNodes :: ReadP [PgnNode]
parseNodes = many parseNode

parseNode :: ReadP PgnNode
parseNode = do
    skipSpaces
    -- Pre-move comments
    preCmt <- parseComments
    skipSpaces

    -- Optional move number (1. or 1...)
    _ <- parseMoveNumber <++ return ""

    skipSpaces
    -- Check if we are at result
    parseCheckNotResult

    san <- parseSan
    suffixNags <- parseSuffixNags

    -- Post-move stuff: NAGs, comments, RAVs
    (postCmt, nags, ravs) <- parsePostMoveStuff

    let combinedCmt = case (preCmt, postCmt) of
            (Nothing, Nothing) -> Nothing
            (Just a, Nothing) -> Just a
            (Nothing, Just b) -> Just b
            (Just a, Just b) -> Just (a ++ "\n" ++ b)

    return $ PgnNode san (suffixNags ++ nags) combinedCmt ravs

parseCheckNotResult :: ReadP ()
parseCheckNotResult = do
    s <- look
    let token = takeWhile (\c -> not (isSpace c) && c `notElem` "{}();") s
    if isResult token then pfail else return ()

parseMoveNumber :: ReadP String
parseMoveNumber = do
    d <- munch1 isDigit
    dot <- munch1 (== '.')
    return (d ++ dot)

parseSan :: ReadP String
parseSan = do
    -- Consume SAN characters.
    munch1 (\c -> not (isSpace c) && c `notElem` "{}()[]$;%!?")

parseSuffixNags :: ReadP [Int]
parseSuffixNags = do
    (string "!!" >> return [3])
    <++ (string "??" >> return [4])
    <++ (string "!?" >> return [5])
    <++ (string "?!" >> return [6])
    <++ (string "!" >> return [1])
    <++ (string "?" >> return [2])
    <++ return []

parseComments :: ReadP (Maybe String)
parseComments = do
    cmts <- many parseComment
    if null cmts then return Nothing else return (Just $ intercalate "\n" cmts)

parseComment :: ReadP String
parseComment = parseBraceComment +++ parseLineComment +++ parseEscapeLine

parseBraceComment :: ReadP String
parseBraceComment = do
    _ <- char '{'
    s <- munch (\c -> c /= '}')
    _ <- char '}'
    return s

parseLineComment :: ReadP String
parseLineComment = do
    _ <- char ';'
    s <- munch (\c -> c /= '\n')
    -- Consume newline?
    _ <- option '\n' (char '\n')
    return s

parseEscapeLine :: ReadP String
parseEscapeLine = do
    _ <- char '%'
    s <- munch (\c -> c /= '\n')
    _ <- option '\n' (char '\n')
    return s

parsePostMoveStuff :: ReadP (Maybe String, [Int], [[PgnNode]])
parsePostMoveStuff = do
    elements <- many parsePostElement
    let (cmts, nags, ravs) = foldr distribute ([], [], []) elements
    let combinedCmt = if null cmts then Nothing else Just (intercalate "\n" cmts)
    return (combinedCmt, nags, ravs)
  where
    distribute (PeComment c) (cs, ns, rs) = (c:cs, ns, rs)
    distribute (PeNag n)     (cs, ns, rs) = (cs, n:ns, rs)
    distribute (PeRav r)     (cs, ns, rs) = (cs, ns, r:rs)

data PostElement = PeComment String | PeNag Int | PeRav [PgnNode]

parsePostElement :: ReadP PostElement
parsePostElement =
    (skipSpaces >> parseComment >>= return . PeComment)
    +++ (skipSpaces >> parseNag >>= return . PeNag)
    +++ (skipSpaces >> parseRav >>= return . PeRav)

parseNag :: ReadP Int
parseNag = do
    _ <- char '$'
    d <- munch1 isDigit
    return (read d)

parseRav :: ReadP [PgnNode]
parseRav = do
    _ <- char '('
    skipSpaces
    nodes <- parseNodes
    skipSpaces
    _ <- char ')'
    return nodes

parseResult :: ReadP String
parseResult = do
    s <- munch1 (\c -> not (isSpace c))
    if isResult s then return s else pfail

isResult :: String -> Bool
isResult s = s `elem` ["1-0", "0-1", "1/2-1/2", "*"]

-- | Builds a proper game tree from a parsed Game, validating all moves and variations.
buildGameTree :: Game -> Either String GameTree
buildGameTree game = do
    initialPos <- initialPosition game

    nodes <- buildNodes initialPos (forest game)
    return $ GameTree initialPos nodes

buildNodes :: Board.Board -> [PgnNode] -> Either String [GameNode]
buildNodes _ [] = Right []
buildNodes b (n:rest) = do
    -- 1. Parse and validate main move
    move <- parseMove b (pnSan n)
    let nextBoard = Board.applyMove b move

    -- 2. Build main line children from 'rest'
    mainChildren <- buildNodes nextBoard rest

    -- 3. Build variation nodes (siblings)
    -- Each variation is a list of PgnNodes starting from the *current* board 'b'
    variationNodes <- concat <$> mapM (buildNodes b) (pnVariations n)

    let node = GameNode
            { gnMove = move
            , gnTarget = nextBoard
            , gnSan = pnSan n
            , gnComment = pnComment n
            , gnNags = pnNags n
            , gnChildren = mainChildren
            }

    return (node : variationNodes)

parseMove :: Board.Board -> String -> Either String Move
parseMove b sanStr =
    case Board.parseSan b sanStr of
        Just m -> Right m
        Nothing -> case Board.fromUci sanStr of
            Just m -> Right m
            Nothing -> Left $ "Invalid move: " ++ sanStr

-- | Converts a Game into a list of Moves by simulating the game (main line).
readGameMoves :: Game -> Either String [Move]
readGameMoves game = do
    initialPos <- initialPosition game

    playMoves initialPos (moves game)

initialPosition :: Game -> Either String Board.Board
initialPosition game =
    case lookup "FEN" (tags game) of
        Nothing -> Right Board.initialBoard
        Just fenStr -> case Board.parseFen fenStr of
            Just b -> Right b
            Nothing -> Left $ "Invalid FEN tag: " ++ fenStr

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

-- | Converts a Game into a list of UCI move strings (main line).
gameToUci :: Game -> Either String [String]
gameToUci game = map Board.uci <$> readGameMoves game

-- Printer

showGame :: Game -> String
showGame g =
    let initialColor = getStartColor (tags g)
        initialMoveNum = getStartMoveNum (tags g)
        t = showTags (tags g)
        b = formatMoves initialColor initialMoveNum True (forest g) ++ " " ++ result g
    in if null t then b else t ++ "\n" ++ b

showTags :: [(String, String)] -> String
showTags ts = unlines $ map showTag ts
  where
    showTag (k, v) = "[" ++ k ++ " \"" ++ escapeString v ++ "\"]"
    escapeString = concatMap escapeChar
    escapeChar '"' = "\\\""
    escapeChar '\\' = "\\\\"
    escapeChar c = [c]

getStartColor :: [(String, String)] -> Board.Color
getStartColor ts = case lookup "FEN" ts of
    Nothing -> Board.White
    Just fen -> case Board.parseFen fen of
        Just b -> GS.turn (Board.state b)
        Nothing -> Board.White

getStartMoveNum :: [(String, String)] -> Board.FullmoveNumber
getStartMoveNum ts = case lookup "FEN" ts of
    Nothing -> 1
    Just fen -> case Board.parseFen fen of
        Just b -> GS.fullmoveNumber (Board.state b)
        Nothing -> 1

formatMoves :: Board.Color -> Board.FullmoveNumber -> Bool -> [PgnNode] -> String
formatMoves _ _ _ [] = ""
formatMoves c mn isFirst (n:ns) =
    let
        isWhite = c == Board.White
        numStr
          | isWhite = show mn ++ ". "
          | isFirst = show mn ++ "... "
          | otherwise = ""

        moveStr = pnSan n
        nagStr = unwords $ map (\i -> "$" ++ show i) (pnNags n)
        commentStr = case pnComment n of
            Nothing -> ""
            Just s -> " {" ++ s ++ "} "
        ravStr = unwords $ map (\v -> "( " ++ formatMoves c mn True v ++ " )") (pnVariations n)

        nextC = if c == Board.White then Board.Black else Board.White
        nextMn = if c == Board.Black then mn + 1 else mn

        sep = if null ns then "" else " "
    in
        numStr ++ moveStr ++
        (if null nagStr then "" else " " ++ nagStr) ++
        commentStr ++
        (if null ravStr then "" else " " ++ ravStr) ++
        sep ++
        formatMoves nextC nextMn False ns
