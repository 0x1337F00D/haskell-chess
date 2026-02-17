{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Chess.Engine.Search (search) where

import Control.Concurrent (forkIO, threadDelay)
import Data.IORef (newIORef, writeIORef, IORef)

import Chess.Board (Board, trustBoard, ValidatedBoard, SomeValidatedBoard(..), MoveGenerator)
import Chess.Engine.TT (TT)
import Chess.Types (Move)
import Chess.Board.Phase (classifyPhase, SomePhase(..), SPhase(..))
import Chess.Engine.Search.AlphaBeta (searchPhase)
import Chess.Engine.Search.Types (SearchLimits(..))

-- | Search for the best move using phase-indexed dispatch.
search :: Board -> TT -> SearchLimits -> IO Move
search board tt limits = do
    let svBoard = trustBoard board
    stopFlag <- newIORef False

    -- Spawn Timer Thread if time limit is set
    case limitTime limits of
        Just t | not (limitInfinite limits) -> do
            _ <- forkIO $ do
                threadDelay (t * 1000) -- Convert ms to us
                writeIORef stopFlag True
            return ()
        _ -> return ()

    case svBoard of
        InCheckBoard vb -> dispatch vb stopFlag
        NotInCheckBoard vb -> dispatch vb stopFlag
  where
    dispatch :: MoveGenerator s => ValidatedBoard s -> IORef Bool -> IO Move
    dispatch vb stopFlag = case classifyPhase vb of
        SomePhase SOpening pos -> searchPhase pos tt limits stopFlag
        SomePhase SMiddlegame pos -> searchPhase pos tt limits stopFlag
        SomePhase SEndgame pos -> searchPhase pos tt limits stopFlag
