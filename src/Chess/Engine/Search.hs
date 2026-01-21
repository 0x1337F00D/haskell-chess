module Chess.Engine.Search (search) where

import Data.List (sortBy)
import Data.Ord (comparing, Down(..))
import Data.Maybe (isJust)

import Chess.Types
import Chess.Board (Board(..), legalMoves, applyMove, isCheck)
import qualified Chess.Board.Base as Base
import Chess.Engine.Evaluation (evaluate)

-- | Search for the best move.
search :: Board -> Int -> IO Move
search board depth = do
    -- Simple iterative deepening or just fixed depth for now.
    let (bestMove, _) = alphaBetaRoot board depth
    return bestMove

-- | Alpha-Beta at root.
alphaBetaRoot :: Board -> Int -> (Move, Int)
alphaBetaRoot board depth =
    let moves = orderMoves board (legalMoves board)
    in case moves of
        [] -> (nullMove, 0) -- Should check game over before calling search
        (m:ms) ->
            let (bestM, bestScore) = foldl searchMove (m, -infinity) (m:ms)
            in (bestM, bestScore)
  where
    searchMove (bestM, bestScore) move =
        let score = -alphaBeta (applyMove board move) (depth - 1) (-infinity) infinity
        in if score > bestScore then (move, score) else (bestM, bestScore)

-- | Alpha-Beta search.
alphaBeta :: Board -> Int -> Int -> Int -> Int
alphaBeta board depth alpha beta
    | depth <= 0 = quiescence board alpha beta
    | null moves = if isCheck board then -mateValue + (100 - depth) else 0 -- Mate or Stalemate
    | otherwise = go moves alpha
  where
    moves = orderMoves board (legalMoves board)

    go [] a = a
    go (m:ms) a =
        let score = -alphaBeta (applyMove board m) (depth - 1) (-beta) (-a)
        in if score >= beta
           then beta -- Cutoff
           else go ms (max a score)

-- | Quiescence Search (only captures).
quiescence :: Board -> Int -> Int -> Int
quiescence board alpha beta =
    let standPat = evaluate board
        a = max alpha standPat
    in if standPat >= beta
       then beta
       else
           let moves = filter (isCapture board) (legalMoves board)
               sortedMoves = orderMoves board moves
           in go sortedMoves a
  where
    go [] a = a
    go (m:ms) a =
        let score = -quiescence (applyMove board m) (-beta) (-a)
        in if score >= beta
           then beta
           else go ms (max a score)

-- | Infinity constant.
infinity :: Int
infinity = 100000

-- | Mate value.
mateValue :: Int
mateValue = 20000

-- | Move ordering: Captures first, then promotion.
orderMoves :: Board -> [Move] -> [Move]
orderMoves board moves = sortBy (comparing (Down . scoreMove)) moves
  where
    scoreMove :: Move -> Int
    scoreMove m =
        let captureBonus = if isCapture board m then 1000 else 0
            promoBonus = case promotion m of Nothing -> 0; Just _ -> 900
        in captureBonus + promoBonus

-- | Check if a move is a capture.
-- Note: This misses En Passant captures as checking for them strictly requires GameState access
-- which is inside Chess.Board. But for move ordering/QSearch this is an okay approximation for now.
isCapture :: Board -> Move -> Bool
isCapture (Chess.Board.Board b _ _) (Move _ to _) =
    isJust (Base.pieceAt b to)
isCapture _ NullMove = False
