{-# LANGUAGE GADTs #-}
module Chess.Engine.Search (search) where

import Chess.Board (Board, trustBoard)
import Chess.Engine.TT (TT)
import Chess.Types (Move)
import Chess.Board.Phase (classifyPhase, SomePhase(..), SPhase(..))
import Chess.Engine.Search.AlphaBeta (searchPhase)

-- | Search for the best move using phase-indexed dispatch.
search :: Board -> TT -> Int -> IO Move
search board tt maxDepth = do
    let vBoard = trustBoard board
    case classifyPhase vBoard of
        SomePhase SOpening pos -> searchPhase pos tt maxDepth
        SomePhase SMiddlegame pos -> searchPhase pos tt maxDepth
        SomePhase SEndgame pos -> searchPhase pos tt maxDepth
