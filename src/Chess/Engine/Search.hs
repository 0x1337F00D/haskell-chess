{-# LANGUAGE BangPatterns #-}
module Chess.Engine.Search (search) where

import Data.Maybe (isJust)
import Data.IORef (IORef, newIORef, readIORef, modifyIORef')
import Control.Concurrent (forkIO, newEmptyMVar, putMVar, takeMVar, getNumCapabilities, newMVar, modifyMVar)

import Chess.Types
import Chess.Board (Board(..), applyMove, isCheck, uci, GenMove(..)
                   , ValidatedBoard, LegalMove, trustBoard, getBoard, getGenMove, legalMovesValidated, captureMovesValidated, applyLegalMove)
import qualified Chess.Board.GameState as GS
import Chess.Engine.Evaluation (evaluate)
import Chess.Engine.TT (TT, probeTT, storeTT, TTFlag(..))

-- | Search Constants
infinity :: Int
infinity = 30000

mateValue :: Int
mateValue = 20000


-- | Search for the best move.
search :: Board -> TT -> Int -> IO Move
search board tt maxDepthInt = do
    let maxDepth = mkDepth maxDepthInt
    nodes <- newIORef 0
    let vBoard = trustBoard board
    let loop depth bestM
          | depth > maxDepth = return bestM
          | otherwise = do
              (move, score) <- alphaBetaRoot vBoard tt depth nodes
              n <- readIORef nodes
              let scoreStr = if abs score > 10000
                             then "mate " ++ show ((if score > 0 then mateValue - score + 1 else -mateValue - score) `div` 2)
                             else "cp " ++ show score
              putStrLn $ "info depth " ++ show depth ++ " score " ++ scoreStr ++ " nodes " ++ show n ++ " pv " ++ uci move
              loop (incDepth depth) move

    -- Iterative Deepening
    loop depthOne nullMove

-- | Root Search
alphaBetaRoot :: ValidatedBoard -> TT -> Depth -> IORef Int -> IO (Move, Int)
alphaBetaRoot vBoard tt depth nodes = do
    let moves = legalMovesValidated vBoard
    let board = getBoard vBoard
    -- Probe TT for root move ordering (optional but good)
    let hash = GS.zobristHash (state board)
    ttEntry <- probeTT tt hash
    let ttMove = case ttEntry of Just (m, _, _, _) -> Just m; Nothing -> Nothing

    let sortedMoves = orderGenMoves moves ttMove

    case sortedMoves of
        [] -> return (nullMove, 0) -- Should not happen if game not over
        (lm:lms) -> do
            let gm = getGenMove lm
            score <- alphaBeta (applyLegalMove vBoard lm) tt (decDepth depth) (-infinity) infinity True nodes
            let bestMove = getMove gm
            let bestScore = -score

            if null lms then return (bestMove, bestScore)
            else do
                caps <- getNumCapabilities
                if caps <= 1
                then go lms bestMove bestScore (-infinity) infinity
                else do
                    queue <- newMVar lms

                    let worker _ = do
                            localNodes <- newIORef 0

                            let loop bestRes = do
                                    mbMove <- modifyMVar queue $ \ms -> case ms of
                                        [] -> return ([], Nothing)
                                        (m:rest) -> return (rest, Just m)

                                    case mbMove of
                                        Nothing -> do
                                            n <- readIORef localNodes
                                            return (bestRes, n)
                                        Just lmWorker -> do
                                            let gmWorker = getGenMove lmWorker
                                            -- search gm
                                            s <- alphaBeta (applyLegalMove vBoard lmWorker) tt (decDepth depth) (-infinity) (-bestScore) True localNodes
                                            let score = -s

                                            let newBestRes = case bestRes of
                                                    Nothing -> Just (getMove gmWorker, score)
                                                    Just (_, bs) -> if score > bs then Just (getMove gmWorker, score) else bestRes

                                            loop newBestRes

                            loop Nothing

                    results <- mapConcurrently worker [1..caps]

                    let (finalM, finalS) = foldl merge (bestMove, bestScore) (map fst results)
                    let totalNodes = sum (map snd results)
                    modifyIORef' nodes (+ totalNodes)
                    return (finalM, finalS)

  where
    merge (bm, bs) Nothing = (bm, bs)
    merge (bm, bs) (Just (m, s)) = if s > bs then (m, s) else (bm, bs)

    go [] bestM bestScore _ _ = return (bestM, bestScore)
    go (lm:lms) bestM bestScore alpha beta = do
        let gm = getGenMove lm
        -- PVS: Search with null window first if we found a good move
        let newAlpha = max alpha bestScore
        s <- alphaBeta (applyLegalMove vBoard lm) tt (decDepth depth) (-beta) (-newAlpha) True nodes
        let score = -s

        if score > bestScore
        then go lms (getMove gm) score alpha beta
        else go lms bestM bestScore alpha beta

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
        _ <- forkIO $ do
            res <- f x
            putMVar v res
        return v) xs
    mapM takeMVar vars

-- | Alpha-Beta Search
alphaBeta :: ValidatedBoard -> TT -> Depth -> Int -> Int -> Bool -> IORef Int -> IO Int
alphaBeta vBoard tt depth alpha beta canNull nodes = do
    modifyIORef' nodes (+1)
    let board = getBoard vBoard
    let hash = GS.zobristHash (state board)

    -- 1. TT Probe
    ttEntry <- probeTT tt hash
    let (ttMove, ttScore, ttDepth, ttFlag) = case ttEntry of
            Just (m, s, d, f) -> (Just m, s, d, f)
            Nothing -> (Nothing, 0, mkDepth (-1), TTExact)

    -- TT Cutoff
    let ttHit = isJust ttEntry && ttDepth >= depth
    let ttCutoff = if ttHit
                   then case ttFlag of
                       TTExact -> True
                       TTLower -> ttScore >= beta
                       TTUpper -> ttScore <= alpha
                       TTEval  -> False -- Evaluation is not a search result
                   else False

    if ttCutoff && abs ttScore < (mateValue - 100) -- Don't return mate scores from TT directly to avoid ply issues? Or adjust them.
    then return ttScore
    else do
        let inCheck = isCheck board

        -- Checkmate/Stalemate detection if no moves (handled later) or depth <= 0
        if isZeroDepth depth
        then quiescence vBoard tt alpha beta nodes
        else do
            -- 2. Null Move Pruning
            -- R=2 if depth > 6 else R=2? Usually R=2.
            let r = if depth > mkDepth 6 then mkDepth 3 else mkDepth 2
            let doNmp = canNull && not inCheck && depth >= r && beta < mateValue

            nmpResult <- if doNmp
                         then do
                             let nullB = Chess.Board.applyMove board NullMove
                             let nullVB = trustBoard nullB -- Assuming null move is safe if inCheck is false
                             -- depth - 1 - r
                             let d' = depth `minusDepth` depthOne `minusDepth` r
                             score <- alphaBeta nullVB tt d' (-beta) (-beta + 1) False nodes
                             return (if -score >= beta then Just beta else Nothing)
                         else return Nothing

            case nmpResult of
                Just cutoff -> return cutoff
                Nothing -> do
                    -- 3. Move Generation
                    let moves = legalMovesValidated vBoard

                    if null moves
                    then return $ if inCheck then -mateValue + (100 - unDepth depth) else 0 -- Mate or Stalemate
                    else do
                        let sortedMoves = orderGenMoves moves ttMove

                        -- 4. Search Loop
                        searchMoves sortedMoves alpha beta depth TTUpper 0 (nullMove)

  where
    searchMoves [] _ _ _ flag bestScore bestM = do
        let board = getBoard vBoard
        -- Store TT
        let hash = GS.zobristHash (state board)
        storeTT tt hash depth bestScore flag bestM
        return bestScore

    searchMoves (lm:lms) a b d flag bestScore bestM = do
        let gm = getGenMove lm
        let m = getMove gm
        let isCapture = isCap gm
        let isProm = isPromotion gm

        let newVBoard = applyLegalMove vBoard lm

        -- PVS
        score <- if bestScore == -infinity -- First move
                 then do
                     s <- alphaBeta newVBoard tt (decDepth d) (-b) (-a) True nodes
                     return (-s)
                 else do
                     -- Late Move Reduction?
                     -- If quiet, depth > 2, not checking, etc.
                     let lmr = if d >= mkDepth 3 && not isCapture && not isProm -- && index > ...
                               then depthOne else depthZero
                     let d' = d `minusDepth` depthOne `minusDepth` lmr

                     s <- alphaBeta newVBoard tt d' (-a - 1) (-a) True nodes -- Null window
                     if s > a && s < b
                     then do
                         -- Re-search with full window
                         s2 <- alphaBeta newVBoard tt (decDepth d) (-b) (-a) True nodes
                         return (-s2)
                     else return (-s)

        let newBestScore = max bestScore score
        let newFlag = if score >= b then TTLower else if score > a then TTExact else flag
        let newBestM = if score > bestScore then m else bestM

        if score >= b
        then do
            let board = getBoard vBoard
            -- Cutoff
            let hash = GS.zobristHash (state board)
            storeTT tt hash depth score TTLower m
            return score -- Fail high
        else do
            searchMoves lms (max a score) b d newFlag newBestScore newBestM

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
quiescence :: ValidatedBoard -> TT -> Int -> Int -> IORef Int -> IO Int
quiescence vBoard tt alpha beta nodes = do
    modifyIORef' nodes (+1)
    let board = getBoard vBoard

    -- Check for cached evaluation in TT
    let hash = GS.zobristHash (state board)
    ttEntry <- probeTT tt hash

    -- Use cached eval if available (depth 0 is convention for static eval in this context,
    -- or we check specifically for TTEval flag)
    let staticEval = case ttEntry of
            Just (_, s, d, TTEval) -> Just s
            _ -> Nothing

    standPat <- case staticEval of
        Just s -> return s
        Nothing -> do
            let s = evaluate vBoard
            -- Store static eval in TT
            storeTT tt hash depthZero s TTEval nullMove
            return s

    if standPat >= beta
    then return beta
    else do
        let a = max alpha standPat
        let moves = captureMovesValidated vBoard
        let sortedMoves = orderGenMoves moves Nothing

        go sortedMoves a
  where
    go [] a = return a
    go (lm:lms) a = do
        score <- do
            s <- quiescence (applyLegalMove vBoard lm) tt (-beta) (-a) nodes
            return (-s)
        if score >= beta
        then return beta
        else go lms (max a score)

-- | Move Ordering
orderGenMoves :: [LegalMove] -> Maybe Move -> [LegalMove]
orderGenMoves moves Nothing =
    let (capProms, caps, proms, quiets) = partition moves
    in capProms ++ caps ++ proms ++ quiets
orderGenMoves moves (Just ttM) =
    let (ttMoves, others) = foldr (\lm (t, o) -> if getMove (getGenMove lm) == ttM then (lm:t, o) else (t, lm:o)) ([], []) moves
        (capProms, caps, proms, quiets) = partition others
    in ttMoves ++ capProms ++ caps ++ proms ++ quiets
  where
    getMove (GenQuiet f t _) = Move f t Nothing
    getMove (GenCapture f t _ _) = Move f t Nothing
    getMove (GenEnPassant f t) = Move f t Nothing
    getMove (GenCastling f t) = Move f t Nothing
    getMove (GenPromotion f t p) = Move f t (Just p)
    getMove (GenPromotionCapture f t p _) = Move f t (Just p)

partition :: [LegalMove] -> ([LegalMove], [LegalMove], [LegalMove], [LegalMove])
partition moves = foldr part ([], [], [], []) moves
  where
    part lm (cp, c, p, q) = case getGenMove lm of
        GenPromotionCapture {} -> (lm:cp, c, p, q)
        GenCapture {} -> (cp, lm:c, p, q)
        GenEnPassant {} -> (cp, lm:c, p, q)
        GenPromotion {} -> (cp, c, lm:p, q)
        GenQuiet {} -> (cp, c, p, lm:q)
        GenCastling {} -> (cp, c, p, lm:q)
