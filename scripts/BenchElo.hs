{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Concurrent.Async (mapConcurrently)
import Control.Exception (bracket)
import Control.Monad (forM)
import Data.List (isPrefixOf)
import System.Environment (getArgs)
import System.IO (Handle, hClose, hFlush, hGetLine, hPutStrLn)
import System.Process (createProcess, std_in, std_out, std_err, StdStream(..), shell, ProcessHandle, waitForProcess)
import Text.Printf (printf)

import Chess.Board (Board, initialBoard, applyMove, uci, fromUci, outcome)
import Chess.Types (Move, Color(..), Outcome(..))
import Chess.Engine.Search (search)
import Chess.Engine.Search.Types (SearchLimits(..), defaultLimits)
import Chess.Engine.TT (TT, newTT)

-- | Configuration for an agent
data AgentConfig
    = Internal Int -- Depth
    | External String Int -- Cmd Depth
    deriving (Show, Eq)

-- | Handle to a running agent
data AgentHandle
    = InternalHandle
        { agentDepth :: Int
        , agentTT :: TT
        }
    | ExternalHandle
        { agentCmdStr :: String
        , agentExtDepth :: Int
        , agentIn :: Handle
        , agentOut :: Handle
        , agentProc :: ProcessHandle
        }

-- | Spawn an agent
spawnAgent :: AgentConfig -> IO AgentHandle
spawnAgent (Internal d) = do
    tt <- newTT 20 -- 1MB TT for now
    return $ InternalHandle d tt
spawnAgent (External cmd d) = do
    let p = (shell cmd) { std_in = CreatePipe, std_out = CreatePipe, std_err = Inherit }
    (Just hin, Just hout, _, ph) <- createProcess p
    hPutStrLn hin "uci"
    hFlush hin
    -- Wait for uciok
    waitForUciOk hout
    hPutStrLn hin "isready"
    hFlush hin
    -- Wait for readyok
    waitForReadyOk hout
    return $ ExternalHandle cmd d hin hout ph

waitForUciOk :: Handle -> IO ()
waitForUciOk h = do
    l <- hGetLine h
    if l == "uciok" then return () else waitForUciOk h

waitForReadyOk :: Handle -> IO ()
waitForReadyOk h = do
    l <- hGetLine h
    if l == "readyok" then return () else waitForReadyOk h

-- | Close an agent
closeAgent :: AgentHandle -> IO ()
closeAgent (InternalHandle _ _) = return ()
closeAgent (ExternalHandle _ _ hin hout ph) = do
    hPutStrLn hin "quit"
    hFlush hin
    hClose hin
    hClose hout
    _ <- waitForProcess ph
    return ()

-- | Get best move from an agent
getBestMove :: AgentHandle -> Board -> [Move] -> IO Move
getBestMove (InternalHandle d tt) board _ = do
    search board tt (defaultLimits { limitDepth = Just d })
getBestMove (ExternalHandle _ d hin hout _) _ moves = do
    let movesStr = unwords (map uci moves)
    hPutStrLn hin $ "position startpos moves " ++ movesStr
    hPutStrLn hin $ "go depth " ++ show d
    hFlush hin
    readBestMove hout

readBestMove :: Handle -> IO Move
readBestMove h = do
    l <- hGetLine h
    if "bestmove" `isPrefixOf` l
    then do
        let parts = words l
        case parts of
            (_:mStr:_) -> case fromUci mStr of
                Just m -> return m
                Nothing -> error $ "Invalid bestmove: " ++ l
            _ -> error $ "Invalid bestmove line: " ++ l
    else readBestMove h

-- | Game Result
data GameResult = ResultWin Color | ResultDraw
    deriving (Show, Eq)

scoreResult :: GameResult -> (Double, Double)
scoreResult (ResultWin White) = (1.0, 0.0)
scoreResult (ResultWin Black) = (0.0, 1.0)
scoreResult ResultDraw        = (0.5, 0.5)

-- | Play a single game
playGame :: AgentHandle -> AgentHandle -> IO GameResult
playGame whiteAgent blackAgent = do
    let loop board moves = do
            case outcome board of
                Just o -> return $ case outcomeWinner o of
                    Just White -> ResultWin White
                    Just Black -> ResultWin Black
                    Nothing    -> ResultDraw
                Nothing -> do
                    -- Determine whose turn it is
                    let isWhite = even (length moves)
                    let agent = if isWhite then whiteAgent else blackAgent

                    move <- getBestMove agent board moves
                    let newBoard = applyMove board move
                    loop newBoard (moves ++ [move])

    loop initialBoard []

-- | Invert result (for swapping colors)
invertResult :: GameResult -> GameResult
invertResult (ResultWin White) = ResultWin Black
invertResult (ResultWin Black) = ResultWin White
invertResult ResultDraw = ResultDraw

-- | Play a single game in the tournament (handling color swap)
playMatchPair :: (Int, AgentConfig, AgentConfig) -> IO GameResult
playMatchPair (gameIdx, c1, c2) = do
    bracket (spawnAgent c1) closeAgent $ \a1 ->
        bracket (spawnAgent c2) closeAgent $ \a2 -> do
            if even gameIdx
            then playGame a1 a2 -- E1 is White
            else do
                res <- playGame a2 a1 -- E1 is Black
                return $ invertResult res

chunkList :: Int -> [a] -> [[a]]
chunkList _ [] = []
chunkList n xs = take n xs : chunkList n (drop n xs)

-- | Main
main :: IO ()
main = do
    args <- getArgs
    let (c1, c2, games, conc) = parseArgs args ("internal", 5) ("internal", 5) 10 1

    printf "Running tournament: %d games\n" games
    printf "Engine 1: %s\n" (show c1)
    printf "Engine 2: %s\n" (show c2)
    printf "Concurrency: %d\n" conc

    let tasks = zip3 [0..games-1] (repeat c1) (repeat c2)
    let taskBatches = chunkList conc tasks

    allResults <- forM taskBatches $ \batch -> do
        mapConcurrently playMatchPair batch

    let flatResults = concat allResults
    let (s1, s2) = foldl (\(acc1, acc2) res -> let (p1, p2) = scoreResult res in (acc1 + p1, acc2 + p2)) (0, 0) flatResults

    printf "\nFinal Score:\n"
    printf "Engine 1: %.1f\n" s1
    printf "Engine 2: %.1f\n" s2

    if s1 + s2 > 0 then do
        let winRate = s1 / (s1 + s2)
        -- Avoid division by zero or log of zero/inf
        let eloDiff = if winRate > 0.99 then 800 else if winRate < 0.01 then -800 else -400 * logBase 10 (1 / winRate - 1)
        printf "Elo Difference (E1 - E2): %.2f\n" eloDiff
    else
        printf "No games played or invalid scores.\n"

mkConfig :: String -> Int -> AgentConfig
mkConfig "internal" d = Internal d
mkConfig cmd d = External cmd d

parseArgs :: [String] -> (String, Int) -> (String, Int) -> Int -> Int -> (AgentConfig, AgentConfig, Int, Int)
parseArgs args (c1, d1) (c2, d2) g conc = case args of
    [] -> (mkConfig c1 d1, mkConfig c2 d2, g, conc)
    ("--engine1":cmd:rest) -> parseArgs rest (cmd, d1) (c2, d2) g conc
    ("--engine2":cmd:rest) -> parseArgs rest (c1, d1) (cmd, d2) g conc
    ("--depth":dStr:rest) ->
        let d = read dStr
        in parseArgs rest (c1, d) (c2, d) g conc
    ("--depth1":dStr:rest) ->
        let d = read dStr
        in parseArgs rest (c1, d) (c2, d2) g conc
    ("--depth2":dStr:rest) ->
        let d = read dStr
        in parseArgs rest (c1, d1) (c2, d) g conc
    ("--games":gStr:rest) ->
        let g' = read gStr
        in parseArgs rest (c1, d1) (c2, d2) g' conc
    ("--concurrency":cStr:rest) ->
        let conc' = read cStr
        in parseArgs rest (c1, d1) (c2, d2) g conc'
    (_:rest) -> parseArgs rest (c1, d1) (c2, d2) g conc
