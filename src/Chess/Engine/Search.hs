module Chess.Engine.Search (search) where

import Data.List (foldl')
import Data.Maybe (isJust)

import Chess.Types
import Chess.Board (Board(..), legalGenMoves, captureGenMoves, applyGenMove, isCheck, uci, GenMove(..))
import Chess.Engine.Evaluation (evaluate)

-- | Search for the best move.
search :: Board -> Int -> IO Move
search board maxDepth = do
    let loop depth best
          | depth > maxDepth = return best
          | otherwise = do
              let (move, score) = alphaBetaRoot board depth
              putStrLn $ "info depth " ++ show depth ++ " score cp " ++ show score ++ " pv " ++ uci move
              loop (depth + 1) move

    loop 1 nullMove

-- | Alpha-Beta at root.
alphaBetaRoot :: Board -> Int -> (Move, Int)
alphaBetaRoot board depth =
    let moves = orderGenMoves (legalGenMoves board)
    in case moves of
        [] -> (nullMove, 0) -- Should check game over before calling search
        (gm@(GenMove m _ _):gms) ->
            let (bestM, bestScore) = foldl' searchMove (m, -infinity) (gm:gms)
            in (bestM, bestScore)
  where
    searchMove (bestM, bestScore) gm@(GenMove m _ _) =
        let score = -alphaBeta (applyGenMove board gm) (depth - 1) (-infinity) infinity
        in if score > bestScore then (m, score) else (bestM, bestScore)

-- | Alpha-Beta search.
alphaBeta :: Board -> Int -> Int -> Int -> Int
alphaBeta board depth alpha beta
    | depth <= 0 = quiescence board alpha beta
    | null moves = if isCheck board then -mateValue + (100 - depth) else 0 -- Mate or Stalemate
    | otherwise = go moves alpha
  where
    moves = orderGenMoves (legalGenMoves board)

    go [] a = a
    go (gm:gms) a =
        let score = -alphaBeta (applyGenMove board gm) (depth - 1) (-beta) (-a)
        in if score >= beta
           then beta -- Cutoff
           else go gms (max a score)

-- | Quiescence Search (only captures).
quiescence :: Board -> Int -> Int -> Int
quiescence board alpha beta =
    let standPat = evaluate board
        a = max alpha standPat
    in if standPat >= beta
       then beta
       else
           let moves = captureGenMoves board
               sortedMoves = orderGenMoves moves
           in go sortedMoves a
  where
    go [] a = a
    go (gm:gms) a =
        let score = -quiescence (applyGenMove board gm) (-beta) (-a)
        in if score >= beta
           then beta
           else go gms (max a score)

-- | Infinity constant.
infinity :: Int
infinity = 100000

-- | Mate value.
mateValue :: Int
mateValue = 20000

-- | Move ordering: Captures first, then promotion.
orderGenMoves :: [GenMove] -> [GenMove]
orderGenMoves moves = capProms ++ caps ++ proms ++ quiets
  where
    (capProms, caps, proms, quiets) = foldr partitionMoves ([], [], [], []) moves

    partitionMoves gm@(GenMove m _ _) (cp, c, p, q)
        | isCapture gm =
            if isPromotion m
            then (gm:cp, c, p, q)
            else (cp, gm:c, p, q)
        | isPromotion m = (cp, c, gm:p, q)
        | otherwise     = (cp, c, p, gm:q)

    isPromotion m = isJust (promotion m)

    -- Efficient check for capture using GenMove data
    isCapture (GenMove (Move f t _) pt cap) =
        isJust cap || (pt == Pawn && squareFile f /= squareFile t)
    isCapture _ = False
