module Chess.Engine.Uci (run) where

import System.IO (hFlush, stdout, isEOF)
import Data.Maybe (fromMaybe)
import Control.Monad (unless)
import Control.Monad.State (StateT, evalStateT, get, put, liftIO)

import Chess.Board (Board, initialBoard, parseFen, fromUci, uci, applyMove)
import Chess.Engine.Search (search)

type UciM a = StateT Board IO a

-- | Run the UCI loop.
run :: IO ()
run = evalStateT loop initialBoard

loop :: UciM ()
loop = do
    eof <- liftIO isEOF
    unless eof $ do
        line <- liftIO getLine
        let cmd = words line
        case cmd of
            ("uci":_) -> do
                liftIO $ putStrLn "id name haskell-chess"
                liftIO $ putStrLn "id author Codex"
                liftIO $ putStrLn "uciok"
                liftIO $ hFlush stdout
                loop
            ("isready":_) -> do
                liftIO $ putStrLn "readyok"
                liftIO $ hFlush stdout
                loop
            ("quit":_) -> return ()
            ("position":rest) -> do
                let newBoard = parsePosition rest
                put newBoard
                loop
            ("go":_) -> do
                board <- get
                best <- liftIO $ search board 5
                liftIO $ putStrLn $ "bestmove " ++ uci best
                liftIO $ hFlush stdout
                loop
            _ -> loop

parsePosition :: [String] -> Board
parsePosition ("startpos":rest) =
    let moves = dropWhile (/= "moves") rest
    in case moves of
        ("moves":ms) -> applyMoves initialBoard ms
        _ -> initialBoard
parsePosition ("fen":rest) =
    let (fenParts, movesParts) = break (== "moves") rest
        fenStr = unwords fenParts
        b = fromMaybe initialBoard (parseFen fenStr)
    in case movesParts of
        ("moves":ms) -> applyMoves b ms
        _ -> b
parsePosition _ = initialBoard

applyMoves :: Board -> [String] -> Board
applyMoves b [] = b
applyMoves b (m:ms) =
    case fromUci m of
        Just move -> applyMoves (applyMove b move) ms
        Nothing -> applyMoves b ms
