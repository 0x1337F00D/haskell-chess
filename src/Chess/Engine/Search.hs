{-# LANGUAGE GADTs #-}
module Chess.Engine.Search (search) where

import Control.Concurrent (forkIO, threadDelay)
import Data.IORef (newIORef, writeIORef)

import Chess.Board (Board, trustBoard, SomeValidatedBoard(..))
import Chess.Engine.TT (TT)
import Chess.Types (Move)
import Chess.Board.Phase (classifyPhase, SomePhase(..), SPhase(..))
import Chess.Engine.Search.AlphaBeta (searchPhase)
import Chess.Engine.Search.Types (SearchLimits(..))

-- | Search for the best move using phase-indexed dispatch.
search :: Board -> TT -> SearchLimits -> IO Move
search board tt limits = do
    let someVBoard = trustBoard board
    stopFlag <- newIORef False

    -- Spawn Timer Thread if time limit is set
    case limitTime limits of
        Just t | not (limitInfinite limits) -> do
            _ <- forkIO $ do
                threadDelay (t * 1000) -- Convert ms to us
                writeIORef stopFlag True
            return ()
        _ -> return ()

    case someVBoard of
        SomeValidatedBoard vBoard ->
            case classifyPhase vBoard of
                SomePhase SOpening pos -> searchPhase pos tt limits stopFlag
                SomePhase SMiddlegame pos -> searchPhase pos tt limits stopFlag
                SomePhase SEndgame pos -> searchPhase pos tt limits stopFlag
