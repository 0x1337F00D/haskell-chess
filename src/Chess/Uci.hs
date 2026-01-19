module Chess.Uci (
    Engine(..),
    EngineConfig(..),
    startEngine,
    stopEngine,
    sendCommand,
    EngineOutput(..),
    Score(..),
    Info(..),
    parseEngineLine,
    defaultConfig,
    defaultInfo
) where

import System.Process
import System.IO
import System.IO.Error (catchIOError)
import Control.Monad (void)
import Data.Char (isDigit)
import Text.ParserCombinators.ReadP hiding (get)

data Engine = Engine
    { processHandle :: ProcessHandle
    , stdIn :: Handle
    , stdOut :: Handle
    , stdErr :: Handle
    }

data EngineConfig = EngineConfig
    { cmd :: String
    , args :: [String]
    } deriving (Show, Eq)

defaultConfig :: EngineConfig
defaultConfig = EngineConfig "stockfish" []

startEngine :: EngineConfig -> IO Engine
startEngine config = do
    (Just hin, Just hout, Just herr, ph) <- createProcess (proc (cmd config) (args config))
        { std_in = CreatePipe
        , std_out = CreatePipe
        , std_err = CreatePipe
        , close_fds = True
        }
    hSetBuffering hin LineBuffering
    hSetBuffering hout LineBuffering
    return $ Engine ph hin hout herr

stopEngine :: Engine -> IO ()
stopEngine engine = do
    -- ignore errors on quit
    _ <- tryIO $ sendCommand engine "quit"
    void $ waitForProcess (processHandle engine)

tryIO :: IO a -> IO (Maybe a)
tryIO action = (Just <$> action) `catch` (\e -> return Nothing)
  where
    catch :: IO a -> (IOError -> IO a) -> IO a
    catch = catchIOError

-- Need to import catchIOError? Or just ignore errors.
-- System.IO.Error?
-- Let's just use simple IO for now. sendCommand might fail if pipe broken.

sendCommand :: Engine -> String -> IO ()
sendCommand engine command = do
    hPutStrLn (stdIn engine) command
    hFlush (stdIn engine)

-- Parsing engine output

data EngineOutput
    = Id { name :: String, author :: String }
    | UciOk
    | ReadyOk
    | BestMove { move :: String, ponder :: Maybe String }
    | InfoLine Info
    | Unknown String
    deriving (Show, Eq)

data Score
    = Cp Int
    | Mate Int
    deriving (Show, Eq)

data Info = Info
    { depth :: Maybe Int
    , seldepth :: Maybe Int
    , time :: Maybe Int
    , nodes :: Maybe Int
    , pv :: [String]
    , score :: Maybe Score
    , nps :: Maybe Int
    , tbhits :: Maybe Int
    , multicpv :: Maybe Int
    , infoString :: Maybe String
    } deriving (Show, Eq)

defaultInfo :: Info
defaultInfo = Info Nothing Nothing Nothing Nothing [] Nothing Nothing Nothing Nothing Nothing

parseEngineLine :: String -> EngineOutput
parseEngineLine line =
    case readP_to_S (parseLine <* skipSpaces <* eof) line of
        ((out, _):_) -> out
        _ -> Unknown line

parseLine :: ReadP EngineOutput
parseLine =
    (parseId)
    <++ (string "uciok" >> return UciOk)
    <++ (string "readyok" >> return ReadyOk)
    <++ (parseBestMove)
    <++ (parseInfo)

parseId :: ReadP EngineOutput
parseId = do
    _ <- string "id"
    skipSpaces
    k <- string "name" <++ string "author"
    skipSpaces
    v <- munch (const True)
    if k == "name" then return $ Id v "" else return $ Id "" v

parseBestMove :: ReadP EngineOutput
parseBestMove = do
    _ <- string "bestmove"
    skipSpaces
    m <- munch1 (\c -> c /= ' ')
    skipSpaces
    p <- option Nothing (string "ponder" >> skipSpaces >> munch1 (const True) >>= \x -> return (Just x))
    return $ BestMove m p

parseInfo :: ReadP EngineOutput
parseInfo = do
    _ <- string "info"
    skipSpaces
    i <- parseInfoItems defaultInfo
    return $ InfoLine i

parseInfoItems :: Info -> ReadP Info
parseInfoItems acc =
    (do
        skipSpaces
        eof
        return acc
    ) <++ (do
        skipSpaces
        item <- parseInfoItem
        parseInfoItems (updateInfo acc item)
    )

data InfoItem
    = IDepth Int
    | ISeldepth Int
    | ITime Int
    | INodes Int
    | IPv [String]
    | IScore Score
    | INps Int
    | ITbhits Int
    | IMulticpv Int
    | IString String

updateInfo :: Info -> InfoItem -> Info
updateInfo i (IDepth v) = i { depth = Just v }
updateInfo i (ISeldepth v) = i { seldepth = Just v }
updateInfo i (ITime v) = i { time = Just v }
updateInfo i (INodes v) = i { nodes = Just v }
updateInfo i (IPv v) = i { pv = v }
updateInfo i (IScore v) = i { score = Just v }
updateInfo i (INps v) = i { nps = Just v }
updateInfo i (ITbhits v) = i { tbhits = Just v }
updateInfo i (IMulticpv v) = i { multicpv = Just v }
updateInfo i (IString v) = i { infoString = Just v }

parseInfoItem :: ReadP InfoItem
parseInfoItem =
    (string "depth" >> skipSpaces >> readInt >>= return . IDepth)
    <++ (string "seldepth" >> skipSpaces >> readInt >>= return . ISeldepth)
    <++ (string "time" >> skipSpaces >> readInt >>= return . ITime)
    <++ (string "nodes" >> skipSpaces >> readInt >>= return . INodes)
    <++ (string "nps" >> skipSpaces >> readInt >>= return . INps)
    <++ (string "tbhits" >> skipSpaces >> readInt >>= return . ITbhits)
    <++ (string "multicpv" >> skipSpaces >> readInt >>= return . IMulticpv)
    <++ (parseScore)
    <++ (parsePv)
    <++ (string "string" >> skipSpaces >> munch (const True) >>= return . IString)

readInt :: ReadP Int
readInt = do
    s <- munch1 isDigit
    return (read s)

parseScore :: ReadP InfoItem
parseScore = do
    _ <- string "score"
    skipSpaces
    t <- string "cp" <++ string "mate"
    skipSpaces
    v <- do
        s <- option "" (string "-")
        d <- munch1 isDigit
        return (read (s ++ d))
    if t == "cp" then return $ IScore (Cp v) else return $ IScore (Mate v)

parsePv :: ReadP InfoItem
parsePv = do
    _ <- string "pv"
    skipSpaces
    moves <- sepBy1 (munch1 (\c -> c /= ' ')) skipSpaces
    return $ IPv moves
