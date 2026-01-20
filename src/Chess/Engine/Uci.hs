module Chess.Engine.Uci (run) where

import System.IO (hFlush, stdout, isEOF)
import Data.List (isPrefixOf)
import Data.Maybe (fromMaybe)
import Control.Monad (unless)

import Chess.Board (Board, initialBoard, parseFen, fromUci, uci, applyMove)
import Chess.Engine.Search (search)

-- | Run the UCI loop.
run :: IO ()
run = loop initialBoard

loop :: Board -> IO ()
loop board = do
    eof <- isEOF
    unless eof $ do
        line <- getLine
        let cmd = words line
        case cmd of
            ("uci":_) -> do
                putStrLn "id name haskell-chess"
                putStrLn "id author Codex"
                putStrLn "uciok"
                hFlush stdout
                loop board
            ("isready":_) -> do
                putStrLn "readyok"
                hFlush stdout
                loop board
            ("quit":_) -> return ()
            ("position":rest) -> do
                let newBoard = parsePosition rest
                loop newBoard
            ("go":_) -> do
                -- Fixed depth 5 for now
                best <- search board 5
                putStrLn $ "bestmove " ++ uci best
                hFlush stdout
                loop board
            _ -> loop board

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
