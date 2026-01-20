{-# LANGUAGE Arrows #-}
module Chess.Engine.ArrowEval where

import Control.Arrow
import Chess.Board (Board(..))
import Chess.Board.GameState (GameState(..), turn)
import Chess.Types (Color(..))
import Chess.Engine.Evaluation (evalMaterial, evalPositional, Score)

-- | Arrow-based evaluation.
-- This demonstrates how to compose evaluation terms using Arrows.
-- evaluateArrow :: Arrow a => a Board Score
evaluateArrow :: (Arrow a) => a Board Score
evaluateArrow = proc (Board b gs _) -> do
    mat <- arr evalMaterial -< b
    pos <- arr evalPositional -< b
    let score = mat + pos
    returnA -< if turn gs == White then score else -score
