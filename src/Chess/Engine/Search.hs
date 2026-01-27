{-# LANGUAGE BangPatterns #-}
module Chess.Engine.Search (search) where

import Data.Maybe (isJust)

import Chess.Types
import Chess.Board (Board(..), legalGenMoves, captureGenMoves, applyGenMove, applyMove, isCheck, uci, GenMove(..))
import qualified Chess.Board.GameState as GS
import Chess.Engine.Evaluation (evaluate)
import Chess.Engine.TT (TT, probeTT, storeTT, TTFlag(..))

-- | Search Constants
infinity :: Int
infinity = 30000

mateValue :: Int
mateValue = 20000


-- | Search for the best move.
search :: Board -> TT -> Depth -> IO Move
search board tt maxDepth = do
    let loop depth bestM
          | depth > maxDepth = return bestM
          | otherwise = do
              (move, score) <- alphaBetaRoot board tt depth
              let scoreStr = if abs score > 10000
                             then "mate " ++ show ((if score > 0 then mateValue - score + 1 else -mateValue - score) `div` 2)
                             else "cp " ++ show score
              putStrLn $ "info depth " ++ show depth ++ " score " ++ scoreStr ++ " pv " ++ uci move
              loop (depth + 1) move

    -- Iterative Deepening
    loop 1 nullMove

-- | Root Search
alphaBetaRoot :: Board -> TT -> Depth -> IO (Move, Int)
alphaBetaRoot board tt depth = do
    let moves = legalGenMoves board
    -- Probe TT for root move ordering (optional but good)
    let hash = GS.zobristHash (state board)
    ttEntry <- probeTT tt hash
    let ttMove = case ttEntry of Just (m, _, _, _) -> Just m; Nothing -> Nothing

    let sortedMoves = orderGenMoves moves ttMove

    case sortedMoves of
        [] -> return (nullMove, 0) -- Should not happen if game not over
        (gm:gms) -> do
            score <- alphaBeta (applyGenMove board gm) tt (depth - 1) (-infinity) infinity True
            let bestMove = getMove gm
            go gms bestMove (-score) (-infinity) infinity
  where
    go [] bestM bestScore _ _ = return (bestM, bestScore)
    go (gm:gms) bestM bestScore alpha beta = do
        -- PVS: Search with null window first if we found a good move
        let newAlpha = max alpha bestScore
        s <- alphaBeta (applyGenMove board gm) tt (depth - 1) (-beta) (-newAlpha) True
        let score = -s

        if score > bestScore
        then go gms (getMove gm) score alpha beta
        else go gms bestM bestScore alpha beta

    getMove (GenMove m _ _) = m

-- | Alpha-Beta Search
alphaBeta :: Board -> TT -> Depth -> Int -> Int -> Bool -> IO Int
alphaBeta board tt depth alpha beta canNull = do
    let hash = GS.zobristHash (state board)

    -- 1. TT Probe
    ttEntry <- probeTT tt hash
    let (ttMove, ttScore, ttDepth, ttFlag) = case ttEntry of
            Just (m, s, d, f) -> (Just m, s, d, f)
            Nothing -> (Nothing, 0, -1, TTExact)

    -- TT Cutoff
    let ttHit = isJust ttEntry && ttDepth >= depth
    let ttCutoff = if ttHit
                   then case ttFlag of
                       TTExact -> True
                       TTLower -> ttScore >= beta
                       TTUpper -> ttScore <= alpha
                   else False

    if ttCutoff && abs ttScore < (mateValue - 100) -- Don't return mate scores from TT directly to avoid ply issues? Or adjust them.
    then return ttScore
    else do
        let inCheck = isCheck board

        -- Checkmate/Stalemate detection if no moves (handled later) or depth <= 0
        if depth <= 0
        then quiescence board alpha beta
        else do
            -- 2. Null Move Pruning
            -- R=2 if depth > 6 else R=2? Usually R=2.
            let r = if depth > 6 then 3 else 2
            let doNmp = canNull && not inCheck && depth >= r && beta < mateValue

            nmpResult <- if doNmp
                         then do
                             let nullB = Chess.Board.applyMove board NullMove
                             score <- alphaBeta nullB tt (depth - 1 - r) (-beta) (-beta + 1) False
                             return (if -score >= beta then Just beta else Nothing)
                         else return Nothing

            case nmpResult of
                Just cutoff -> return cutoff
                Nothing -> do
                    -- 3. Move Generation
                    let moves = legalGenMoves board

                    if null moves
                    then return $ if inCheck then -mateValue + (100 - unDepth depth) else 0 -- Mate or Stalemate
                    else do
                        let sortedMoves = orderGenMoves moves ttMove

                        -- 4. Search Loop
                        searchMoves sortedMoves alpha beta depth TTUpper 0 (nullMove)

  where
    searchMoves [] _ _ _ flag bestScore bestM = do
        -- Store TT
        let hash = GS.zobristHash (state board)
        storeTT tt hash depth bestScore flag bestM
        return bestScore

    searchMoves (gm:gms) a b d flag bestScore bestM = do
        let m = getMove gm
        let isCapture = isCap gm
        let isProm = isPromotion m

        -- LMR
        -- Reduce depth for quiet moves late in the order
        -- Count is implicitly 'length moves - length (gm:gms)'? No, I need an index.
        -- Let's pass index or just use pattern match structure if I can.
        -- I'll skip LMR logic complexity for now or add simple index.
        -- Assuming moves are sorted best to worst.

        let newBoard = applyGenMove board gm

        -- PVS
        score <- if bestScore == -infinity -- First move
                 then do
                     s <- alphaBeta newBoard tt (d - 1) (-b) (-a) True
                     return (-s)
                 else do
                     -- Late Move Reduction?
                     -- If quiet, depth > 2, not checking, etc.
                     let lmr = if d >= 3 && not isCapture && not isProm -- && index > ...
                               then 1 else 0
                     let d' = d - 1 - lmr

                     s <- alphaBeta newBoard tt d' (-a - 1) (-a) True -- Null window
                     if s > a && s < b
                     then do
                         -- Re-search with full window
                         s2 <- alphaBeta newBoard tt (d - 1) (-b) (-a) True
                         return (-s2)
                     else return (-s)

        let newBestScore = max bestScore score
        let newFlag = if score >= b then TTLower else if score > a then TTExact else flag
        let newBestM = if score > bestScore then m else bestM

        if score >= b
        then do
            -- Cutoff
            let hash = GS.zobristHash (state board)
            storeTT tt hash depth score TTLower m
            return score -- Fail high
        else do
            searchMoves gms (max a score) b d newFlag newBestScore newBestM

    getMove (GenMove m _ _) = m
    isCap (GenMove _ _ cap) = isJust cap
    isPromotion (Move _ _ p) = isJust p
    isPromotion _ = False

-- | Quiescence Search.
quiescence :: Board -> Int -> Int -> IO Int
quiescence board alpha beta = do
    let standPat = evaluate board
    if standPat >= beta
    then return beta
    else do
        let a = max alpha standPat
        let moves = captureGenMoves board
        let sortedMoves = orderGenMoves moves Nothing

        go sortedMoves a
  where
    go [] a = return a
    go (gm:gms) a = do
        score <- do
            s <- quiescence (applyGenMove board gm) (-beta) (-a)
            return (-s)
        if score >= beta
        then return beta
        else go gms (max a score)

-- | Move Ordering
orderGenMoves :: [GenMove] -> Maybe Move -> [GenMove]
orderGenMoves moves Nothing =
    let (capProms, caps, proms, quiets) = partition moves
    in capProms ++ caps ++ proms ++ quiets
orderGenMoves moves (Just ttM) =
    let (ttMoves, others) = foldr (\gm (t, o) -> if getMove gm == ttM then (gm:t, o) else (t, gm:o)) ([], []) moves
        (capProms, caps, proms, quiets) = partition others
    in ttMoves ++ capProms ++ caps ++ proms ++ quiets
  where
    getMove (GenMove m _ _) = m

partition :: [GenMove] -> ([GenMove], [GenMove], [GenMove], [GenMove])
partition moves = foldr part ([], [], [], []) moves
  where
    part gm@(GenMove m _ _) (cp, c, p, q)
        | isCapture gm =
            if isPromotion m
            then (gm:cp, c, p, q)
            else (cp, gm:c, p, q)
        | isPromotion m = (cp, c, gm:p, q)
        | otherwise     = (cp, c, p, gm:q)

    isPromotion m = isJust (promotion m)
    isCapture (GenMove (Move f t _) pt cap) =
        isJust cap || (pt == Pawn && squareFile f /= squareFile t)
    isCapture _ = False
