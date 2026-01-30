module Chess.Engine.Uci (run) where

import System.IO (hFlush, stdout, isEOF)
import Data.Maybe (fromMaybe)
import Control.Monad (unless)
import Control.Monad.State (StateT, evalStateT, get, liftIO, modify)

import Chess.Board (Board, initialBoard, parseFen, fromUci, uci, applyMove)
import Chess.Engine.Search (search)
import Chess.Engine.TT (TT, newTT)

data EngineState = EngineState
    { esBoard :: !Board
    , esTT    :: !TT
    }

type UciM a = StateT EngineState IO a

-- | Run the UCI loop.
run :: IO ()
run = do
    tt <- newTT 20 -- 2^20 entries
    evalStateT loop (EngineState initialBoard tt)

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
                modify (\s -> s { esBoard = newBoard })
                loop
            ("go":_) -> do
                st <- get
                best <- liftIO $ search (esBoard st) (esTT st) 5
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
