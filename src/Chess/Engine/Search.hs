{-# LANGUAGE BangPatterns #-}
module Chess.Engine.Search (search) where

import Data.Maybe (isJust)
import Data.IORef (IORef, newIORef, readIORef, modifyIORef')
import Control.Concurrent (forkIO, newEmptyMVar, putMVar, takeMVar, getNumCapabilities)

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
    nodes <- newIORef 0
    let loop depth bestM
          | depth > maxDepth = return bestM
          | otherwise = do
              (move, score) <- alphaBetaRoot board tt depth nodes
              n <- readIORef nodes
              let scoreStr = if abs score > 10000
                             then "mate " ++ show ((if score > 0 then mateValue - score + 1 else -mateValue - score) `div` 2)
                             else "cp " ++ show score
              putStrLn $ "info depth " ++ show depth ++ " score " ++ scoreStr ++ " nodes " ++ show n ++ " pv " ++ uci move
              loop (depth + 1) move

    -- Iterative Deepening
    loop 1 nullMove

-- | Root Search
alphaBetaRoot :: Board -> TT -> Depth -> IORef Int -> IO (Move, Int)
alphaBetaRoot board tt depth nodes = do
    let moves = legalGenMoves board
    -- Probe TT for root move ordering (optional but good)
    let hash = GS.zobristHash (state board)
    ttEntry <- probeTT tt hash
    let ttMove = case ttEntry of Just (m, _, _, _) -> Just m; Nothing -> Nothing

    let sortedMoves = orderGenMoves moves ttMove

    case sortedMoves of
        [] -> return (nullMove, 0) -- Should not happen if game not over
        (gm:gms) -> do
            score <- alphaBeta (applyGenMove board gm) tt (depth - 1) (-infinity) infinity True nodes
            let bestMove = getMove gm
            let bestScore = -score

            if null gms then return (bestMove, bestScore)
            else do
                caps <- getNumCapabilities
                if caps <= 1
                then go gms bestMove bestScore (-infinity) infinity
                else do
                    let chunks = roundRobin caps gms
                    results <- mapConcurrently (searchChunk board tt (depth - 1) bestScore infinity) chunks

                    let (finalM, finalS) = foldl merge (bestMove, bestScore) (map fst results)
                    let totalNodes = sum (map snd results)
                    modifyIORef' nodes (+ totalNodes)
                    return (finalM, finalS)

  where
    merge (bm, bs) Nothing = (bm, bs)
    merge (bm, bs) (Just (m, s)) = if s > bs then (m, s) else (bm, bs)

    go [] bestM bestScore _ _ = return (bestM, bestScore)
    go (gm:gms) bestM bestScore alpha beta = do
        -- PVS: Search with null window first if we found a good move
        let newAlpha = max alpha bestScore
        s <- alphaBeta (applyGenMove board gm) tt (depth - 1) (-beta) (-newAlpha) True nodes
        let score = -s

        if score > bestScore
        then go gms (getMove gm) score alpha beta
        else go gms bestM bestScore alpha beta

    getMove (GenQuiet f t _) = Move f t Nothing
    getMove (GenCapture f t _ _) = Move f t Nothing
    getMove (GenEnPassant f t) = Move f t Nothing
    getMove (GenCastling f t) = Move f t Nothing
    getMove (GenPromotion f t p) = Move f t (Just p)
    getMove (GenPromotionCapture f t p _) = Move f t (Just p)

searchChunk :: Board -> TT -> Depth -> Int -> Int -> [GenMove] -> IO (Maybe (Move, Int), Int)
searchChunk board tt depth alpha beta moves = do
    localNodes <- newIORef 0
    res <- go localNodes moves Nothing alpha
    n <- readIORef localNodes
    return (res, n)
  where
    go _ [] bestRes _ = return bestRes
    go ln (gm:gms) bestRes currentAlpha = do
        s <- alphaBeta (applyGenMove board gm) tt depth (-beta) (-currentAlpha) True ln
        let score = -s
        let (newRes, newAlpha) = case bestRes of
                Nothing -> (Just (getMove gm, score), max currentAlpha score)
                Just (_, bestScore) ->
                    if score > bestScore
                    then (Just (getMove gm, score), max currentAlpha score)
                    else (bestRes, currentAlpha)
        go ln gms newRes newAlpha

    getMove (GenQuiet f t _) = Move f t Nothing
    getMove (GenCapture f t _ _) = Move f t Nothing
    getMove (GenEnPassant f t) = Move f t Nothing
    getMove (GenCastling f t) = Move f t Nothing
    getMove (GenPromotion f t p) = Move f t (Just p)
    getMove (GenPromotionCapture f t p _) = Move f t (Just p)

mapConcurrently :: (a -> IO b) -> [a] -> IO [b]
mapConcurrently f xs = do
    vars <- mapM (\x -> do
        v <- newEmptyMVar
        forkIO $ do
            res <- f x
            putMVar v res
        return v) xs
    mapM takeMVar vars

roundRobin :: Int -> [a] -> [[a]]
roundRobin n xs = [ [ x | (i, x) <- zip [0..] xs, i `mod` n == k ] | k <- [0..n-1] ]

-- | Alpha-Beta Search
alphaBeta :: Board -> TT -> Depth -> Int -> Int -> Bool -> IORef Int -> IO Int
alphaBeta board tt depth alpha beta canNull nodes = do
    modifyIORef' nodes (+1)
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
        then quiescence board alpha beta nodes
        else do
            -- 2. Null Move Pruning
            -- R=2 if depth > 6 else R=2? Usually R=2.
            let r = if depth > 6 then 3 else 2
            let doNmp = canNull && not inCheck && depth >= r && beta < mateValue

            nmpResult <- if doNmp
                         then do
                             let nullB = Chess.Board.applyMove board NullMove
                             score <- alphaBeta nullB tt (depth - 1 - r) (-beta) (-beta + 1) False nodes
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
        let isProm = isPromotion gm

        let newBoard = applyGenMove board gm

        -- PVS
        score <- if bestScore == -infinity -- First move
                 then do
                     s <- alphaBeta newBoard tt (d - 1) (-b) (-a) True nodes
                     return (-s)
                 else do
                     -- Late Move Reduction?
                     -- If quiet, depth > 2, not checking, etc.
                     let lmr = if d >= 3 && not isCapture && not isProm -- && index > ...
                               then 1 else 0
                     let d' = d - 1 - lmr

                     s <- alphaBeta newBoard tt d' (-a - 1) (-a) True nodes -- Null window
                     if s > a && s < b
                     then do
                         -- Re-search with full window
                         s2 <- alphaBeta newBoard tt (d - 1) (-b) (-a) True nodes
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

    getMove (GenQuiet f t _) = Move f t Nothing
    getMove (GenCapture f t _ _) = Move f t Nothing
    getMove (GenEnPassant f t) = Move f t Nothing
    getMove (GenCastling f t) = Move f t Nothing
    getMove (GenPromotion f t p) = Move f t (Just p)
    getMove (GenPromotionCapture f t p _) = Move f t (Just p)

    isCap (GenCapture {}) = True
    isCap (GenPromotionCapture {}) = True
    isCap (GenEnPassant {}) = True
    isCap _ = False

    isPromotion (GenPromotion {}) = True
    isPromotion (GenPromotionCapture {}) = True
    isPromotion _ = False

-- | Quiescence Search.
quiescence :: Board -> Int -> Int -> IORef Int -> IO Int
quiescence board alpha beta nodes = do
    modifyIORef' nodes (+1)
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
            s <- quiescence (applyGenMove board gm) (-beta) (-a) nodes
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
    getMove (GenQuiet f t _) = Move f t Nothing
    getMove (GenCapture f t _ _) = Move f t Nothing
    getMove (GenEnPassant f t) = Move f t Nothing
    getMove (GenCastling f t) = Move f t Nothing
    getMove (GenPromotion f t p) = Move f t (Just p)
    getMove (GenPromotionCapture f t p _) = Move f t (Just p)

partition :: [GenMove] -> ([GenMove], [GenMove], [GenMove], [GenMove])
partition moves = foldr part ([], [], [], []) moves
  where
    part gm (cp, c, p, q) = case gm of
        GenPromotionCapture {} -> (gm:cp, c, p, q)
        GenCapture {} -> (cp, gm:c, p, q)
        GenEnPassant {} -> (cp, gm:c, p, q)
        GenPromotion {} -> (cp, c, gm:p, q)
        GenQuiet {} -> (cp, c, p, gm:q)
        GenCastling {} -> (cp, c, p, gm:q)
