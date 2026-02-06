module Chess.Engine.Uci (run) where

import System.IO (hFlush, stdout, isEOF)
import Data.Maybe (fromMaybe)
import Control.Monad (unless)
import Control.Monad.State (StateT, evalStateT, get, liftIO, modify)
import Text.Read (readMaybe)

import Chess.Board (Board(..), initialBoard, parseFen, fromUci, uci, applyMove)
import qualified Chess.Board.GameState as GS
import Chess.Engine.Search (search)
import Chess.Engine.Search.Types (SearchLimits(..), defaultLimits)
import Chess.Engine.TT (TT, newTT)
import Chess.Types (Color(..))

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
            ("go":rest) -> do
                st <- get
                let board = esBoard st
                let turn = GS.turn (state board)
                let limits = parseGo rest turn
                best <- liftIO $ search board (esTT st) limits
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

-- | Parse 'go' command arguments into SearchLimits.
-- Handles time management.
parseGo :: [String] -> Color -> SearchLimits
parseGo args turn =
    let params = parseParams args defaultParams

        -- Time Allocation Logic
        time = case turn of White -> pWTime params; Black -> pBTime params
        inc  = case turn of White -> pWInc params;  Black -> pBInc params

        allocatedTime = case (time, inc) of
            (Just t, Just i) -> Just (t `div` 20 + i)
            (Just t, Nothing) -> Just (t `div` 20)
            _ -> Nothing

        -- Override if 'movetime' is set
        finalTime = case pMoveTime params of
            Just mt -> Just mt
            Nothing -> allocatedTime

    in defaultLimits
        { limitTime = finalTime
        , limitDepth = pDepth params
        , limitNodes = pNodes params
        , limitMate = pMate params
        , limitInfinite = pInfinite params
        }

-- Intermediate parameters structure
data GoParams = GoParams
    { pWTime :: Maybe Int
    , pBTime :: Maybe Int
    , pWInc  :: Maybe Int
    , pBInc  :: Maybe Int
    , pDepth :: Maybe Int
    , pNodes :: Maybe Int
    , pMate  :: Maybe Int
    , pMoveTime :: Maybe Int
    , pInfinite :: Bool
    }

defaultParams :: GoParams
defaultParams = GoParams Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing False

parseParams :: [String] -> GoParams -> GoParams
parseParams [] p = p
parseParams ("wtime":v:xs) p = parseParams xs (p { pWTime = readMaybe v })
parseParams ("btime":v:xs) p = parseParams xs (p { pBTime = readMaybe v })
parseParams ("winc":v:xs) p  = parseParams xs (p { pWInc = readMaybe v })
parseParams ("binc":v:xs) p  = parseParams xs (p { pBInc = readMaybe v })
parseParams ("depth":v:xs) p = parseParams xs (p { pDepth = readMaybe v })
parseParams ("nodes":v:xs) p = parseParams xs (p { pNodes = readMaybe v })
parseParams ("mate":v:xs) p  = parseParams xs (p { pMate = readMaybe v })
parseParams ("movetime":v:xs) p = parseParams xs (p { pMoveTime = readMaybe v })
parseParams ("infinite":xs) p = parseParams xs (p { pInfinite = True })
parseParams (_:xs) p = parseParams xs p
