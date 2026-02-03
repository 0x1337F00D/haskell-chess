{-# LANGUAGE BangPatterns #-}
module Chess.Engine.Search.AlphaBeta where

import Data.Maybe (fromMaybe, isJust)
import Data.List (foldl')
import Data.Bits (popCount)
import Data.IORef (IORef, newIORef, readIORef, modifyIORef')
import Control.Concurrent (forkIO, newEmptyMVar, putMVar, takeMVar, getNumCapabilities, newMVar, modifyMVar)
import qualified Data.Vector.Unboxed.Mutable as UM
import qualified Data.Vector.Unboxed as U

import Chess.Types
import Chess.Board (Board(..), applyMove, isCheck, uci, GenMove(..)
                   , ValidatedBoard, trustBoard, getBoard, getGenMove
                   , applyLegalMove, isCapture, isPromotion, toGenMove, isLegalMove, mkLegalMove)
import qualified Chess.Board
import qualified Chess.Board.Base as Base
import qualified Chess.Board.GameState as GS
import Chess.Engine.Evaluation (evaluate)
import Chess.Engine.TT (TT, probeTT, storeTT, TTFlag(..))
import Chess.Engine.Search.Types
import Chess.Engine.Search.Pruning (lmrTable)
import Chess.Engine.Search.Ordering
import Chess.Engine.Search.Quiescence (quiescence)

-- | Search for the best move.
search :: Board -> TT -> Int -> IO Move
search board tt maxDepthInt = do
    let maxDepth = mkDepth maxDepthInt
    nodes <- newIORef 0

    -- Initialize Search Context
    -- Killers: 2 per ply. Let's assume max depth 128. Size = 128 * 2 = 256.
    killers <- UM.replicate 256 nullMove
    -- History: 64 * 64 = 4096.
    historyVec <- UM.replicate 4096 0
    -- Counter Moves: 64 * 64 = 4096.
    counterMove <- UM.replicate 4096 nullMove
    let ctx = SearchContext killers historyVec counterMove 128

    let vBoard = trustBoard board
    let loop depth bestM
          | depth > maxDepth = return bestM
          | otherwise = do
              (move, score) <- alphaBetaRoot ctx vBoard tt depth nodes
              n <- readIORef nodes
              let scoreStr = if abs score > 10000
                             then "mate " ++ show ((if score > 0 then mateValue - score + 1 else -mateValue - score) `div` 2)
                             else "cp " ++ show score
              putStrLn $ "info depth " ++ show depth ++ " score " ++ scoreStr ++ " nodes " ++ show n ++ " pv " ++ uci move
              loop (incDepth depth) move

    -- Iterative Deepening
    loop depthOne nullMove

-- | Root Search
alphaBetaRoot :: SearchContext -> ValidatedBoard -> TT -> Depth -> IORef Int -> IO (Move, Int)
alphaBetaRoot ctx vBoard tt depth nodes = do
    let moves = Chess.Board.legalMovesValidated vBoard
    let board = getBoard vBoard
    -- Probe TT for root move ordering (optional but good)
    let hash = GS.zobristHash (state board)
    ttEntry <- probeTT tt hash
    let ttMove = case ttEntry of Just (m, _, _, _) -> Just m; Nothing -> Nothing

    let sortedMoves = orderGenMoves vBoard moves ttMove
    let inCheck = isCheck board

    case sortedMoves of
        [] -> return (nullMove, 0) -- Should not happen if game not over
        (lm:lms) -> do
            let gm = getGenMove lm
            let m = getMove gm
            let newVBoard = applyLegalMove vBoard lm
            let givesCheck = isCheck (getBoard newVBoard)
            let newCheckState = if givesCheck then InCheck else NotInCheck

            s <- alphaBeta ctx newVBoard tt (Just m) (decDepth depth) (-infinity) infinity True PV newCheckState nodes
            let bestMove = getMove gm
            let bestScore = stepScore s

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
                                            let mWorker = getMove gmWorker
                                            let newVBWorker = applyLegalMove vBoard lmWorker
                                            let givesCheckWorker = isCheck (getBoard newVBWorker)
                                            let newCSWorker = if givesCheckWorker then InCheck else NotInCheck

                                            -- search gm
                                            s <- alphaBeta ctx newVBWorker tt (Just mWorker) (decDepth depth) (-infinity) (-bestScore) True PV newCSWorker localNodes
                                            let searchScore = stepScore s

                                            let newBestRes = case bestRes of
                                                    Nothing -> Just (mWorker, searchScore)
                                                    Just (_, bs) -> if searchScore > bs then Just (mWorker, searchScore) else bestRes

                                            loop newBestRes

                            loop Nothing

                    results <- mapConcurrently worker [1..caps]

                    let (finalM, finalS) = foldl' merge (bestMove, bestScore) (map fst results)
                    let totalNodes = sum (map snd results)
                    modifyIORef' nodes (+ totalNodes)
                    return (finalM, finalS)

  where
    merge (bm, bs) Nothing = (bm, bs)
    merge (bm, bs) (Just (m, s)) = if s > bs then (m, s) else (bm, bs)

    go [] bestM bestScore _ _ = return (bestM, bestScore)
    go (lm:lms) bestM bestScore alpha beta = do
        let gm = getGenMove lm
        let m = getMove gm
        let newVBoard = applyLegalMove vBoard lm
        let givesCheck = isCheck (getBoard newVBoard)
        let newCheckState = if givesCheck then InCheck else NotInCheck

        -- PVS: Search with null window first if we found a good move
        let newAlpha = max alpha bestScore
        -- NonPV node (null window)
        s <- alphaBeta ctx newVBoard tt (Just m) (decDepth depth) (-beta) (-newAlpha) True NonPV newCheckState nodes
        let score = stepScore s

        if score > bestScore
        then go lms m score alpha beta
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

-- | Alpha-Beta Search (Wrapper for Repetition Check)
alphaBeta :: SearchContext -> ValidatedBoard -> TT -> Maybe Move -> Depth -> Int -> Int -> Bool -> NodeKind -> CheckState -> IORef Int -> IO Int
alphaBeta ctx vBoard tt lastMove depth alpha beta canNull nodeKind checkState nodes = do
    modifyIORef' nodes (+1)
    let board = getBoard vBoard
    let hash = GS.zobristHash (state board)

    -- Repetition Check
    -- If hash is in history, it's a draw (0).
    let isRep = hash `elem` history board
    if isRep && not (isZeroDepth depth)
    then return 0
    else alphaBetaBody ctx vBoard tt lastMove depth alpha beta canNull nodeKind checkState nodes

-- | Alpha-Beta Search Body
alphaBetaBody :: SearchContext -> ValidatedBoard -> TT -> Maybe Move -> Depth -> Int -> Int -> Bool -> NodeKind -> CheckState -> IORef Int -> IO Int
alphaBetaBody ctx vBoard tt lastMove depth alpha beta canNull nodeKind checkState nodes = do
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

    if ttCutoff && abs ttScore < 15000 -- Don't return mate scores from TT directly to avoid distance issues
    then return ttScore
    else do
        let inCheck = case checkState of InCheck -> True; NotInCheck -> False
        -- Static Evaluation
        let staticEval = evaluate vBoard

        -- Checkmate/Stalemate detection if no moves (handled later) or depth <= 0
        if isZeroDepth depth
        then quiescence vBoard tt alpha beta nodes depth
        else do
            -- 3. Null Move Pruning
            -- R=2 if depth > 6 else R=2? Usually R=2.
            let r = if depth > mkDepth 6 then mkDepth 3 else mkDepth 2
            let doNmp = canNull && not inCheck && depth >= r && beta < mateValue
                        && staticEval >= beta -- NMP Verification
                        && popCount (Base.occupiedTotal (pieces board)) > 5 -- Endgame protection

            nmpResult <- if doNmp
                         then do
                             let nullB = Chess.Board.applyMove board NullMove
                             let nullVB = trustBoard nullB -- Assuming null move is safe if inCheck is false
                             -- depth - 1 - r
                             let d' = depth `minusDepth` depthOne `minusDepth` r
                             -- Pass checkState = NotInCheck because null move cannot give check?
                             -- Actually null move passes turn. If we were not in check, opponent *might* be in check?
                             -- No, if we make null move, we do nothing. The board doesn't change except turn.
                             -- So if we were not in check, the opponent (now us) is not in check.
                             -- Wait, Null Move is giving turn to opponent.
                             -- Opponent is to move. Are they in check?
                             -- If we are not in check, and we do nothing, they are not in check.
                             score <- alphaBeta ctx nullVB tt Nothing d' (-beta) (-beta + 1) False NonPV NotInCheck nodes
                             return (if stepScore score >= beta then Just beta else Nothing)
                         else return Nothing

            case nmpResult of
                Just cutoff -> return cutoff
                Nothing -> do
                    -- 4. Staged Move Generation

                    let hasTT = isJust ttMove
                    let ttM = fromMaybe nullMove ttMove

                    -- Stage 0: Explicit TT Move
                    (score0, flag0, bestM0, found0, alpha0, searchedTT) <- if hasTT
                        then do
                            if Chess.Board.isLegalMove board ttM
                            then do
                                case Chess.Board.toGenMove board ttM of
                                    Just gm -> do
                                        let lm = Chess.Board.mkLegalMove gm
                                        let newVBoard = applyLegalMove vBoard lm

                                        -- Calculate check extension for TT move
                                        let givesCheck = isCheck (getBoard newVBoard)
                                        let newCheckState = if givesCheck then InCheck else NotInCheck
                                        let extension = if inCheck then depthOne else depthZero
                                        let nextDepth = (decDepth depth) `plusDepth` extension

                                        -- PV Node logic: If we are PV, TT move is PV.
                                        s <- alphaBeta ctx newVBoard tt (Just ttM) nextDepth (-beta) (-alpha) True nodeKind newCheckState nodes
                                        let score = stepScore s

                                        if score >= beta
                                        then return (score, TTLower, ttM, True, alpha, True) -- Fail high
                                        else do
                                            let newBestScore = max (-infinity) score
                                            let newAlpha = max alpha score
                                            let newFlag = if score > alpha then TTExact else TTUpper
                                            let newBestM = if score > -infinity then ttM else nullMove
                                            return (newBestScore, newFlag, newBestM, True, newAlpha, True)
                                    Nothing -> return (-infinity, TTUpper, nullMove, False, alpha, False)
                            else return (-infinity, TTUpper, nullMove, False, alpha, False)
                        else return (-infinity, TTUpper, nullMove, False, alpha, False)

                    if score0 >= beta
                    then storeAndReturn score0 bestM0 TTLower
                    else do
                        let filterTT ms = if searchedTT then filter (\lm -> getMove (getGenMove lm) /= ttM) ms else ms
                        let sortingTT = if searchedTT then Nothing else Just ttM

                        -- Stage 1: Captures (Good)
                        let captures = Chess.Board.captureMovesValidated vBoard
                        let (goodCaps, badCaps) = partitionSEE vBoard captures
                        let sortedGood = orderGenMoves vBoard (filterTT goodCaps) sortingTT

                        (score1, flag1, bestM1, found1, alpha1) <- searchStage sortedGood (0 :: Int) inCheck staticEval alpha0 beta depth flag0 score0 bestM0 found0

                        if score1 >= beta
                        then storeAndReturn score1 bestM1 TTLower
                        else do
                            -- Stage 2: Promotions (Quiet only)
                            let promotions = filter (not . isCapture) (Chess.Board.legalPromotionsValidated vBoard)
                            let sortedPromotions = orderGenMoves vBoard (filterTT promotions) sortingTT

                            (score2, flag2, bestM2, found2, alpha2) <- searchStage sortedPromotions (0 :: Int) inCheck staticEval alpha1 beta depth flag1 score1 bestM1 found1

                            if score2 >= beta
                            then storeAndReturn score2 bestM2 TTLower
                            else do
                                -- Stage 3: Quiets
                                let quiets = Chess.Board.legalQuietsValidated vBoard
                                killers <- getKillers ctx depth
                                counterMove <- getCounterMove ctx lastMove
                                sortedQuiets <- orderQuiets ctx (filterTT quiets) killers counterMove sortingTT

                                (score3, flag3, bestM3, found3, alpha3) <- searchStage sortedQuiets (0 :: Int) inCheck staticEval alpha2 beta depth flag2 score2 bestM2 found2

                                if score3 >= beta
                                then storeAndReturn score3 bestM3 flag3
                                else do
                                    -- Stage 4: Captures (Bad)
                                    let sortedBad = orderGenMoves vBoard (filterTT badCaps) sortingTT
                                    (score4, flag4, bestM4, found4, _) <- searchStage sortedBad (0 :: Int) inCheck staticEval alpha3 beta depth flag3 score3 bestM3 found3

                                    if not found4 -- No moves found (Checkmate or Stalemate)
                                    then return $ if inCheck then -mateValue else 0
                                    else storeAndReturn score4 bestM4 flag4

  where
    storeAndReturn s m f = do
        let board = getBoard vBoard
        let hash = GS.zobristHash (state board)
        storeTT tt hash depth s f m
        return s

    searchStage [] _ _ _ a _ _ flag bestScore bestM found = return (bestScore, flag, bestM, found, a)
    searchStage (lm:lms) !index inCheck staticEval a b d flag bestScore bestM _ = do
        let isCap = isCapture lm
        let isProm = isPromotion lm
        let isQuiet = not isCap && not isProm
        let dVal = unDepth d

        let gm = getGenMove lm
        let m = getMove gm

        let newVBoard = applyLegalMove vBoard lm

        -- Calculate check extension based on the NEW board state (after move)
        let givesCheck = isCheck (getBoard newVBoard)
        let newCheckState = if givesCheck then InCheck else NotInCheck

        -- Move-based Pruning (Quiet only)
        -- Delayed to allow checking givesCheck
        let pruneQuiet = if isQuiet && not inCheck && not givesCheck
                         then
                             let lmpCount = 3 + dVal * dVal
                                 doLMP = dVal < 8 && dVal >= 3 && index > lmpCount

                                 fpMargin = 100 * dVal
                                 -- Guard: Disable futility pruning for the first 11 quiet moves (index > 10)
                                 doFutility = index > 10 && dVal < 7 && dVal >= 2 && abs bestScore < mateValue && abs a < mateValue && staticEval + fpMargin <= a
                                              && popCount (Base.occupiedTotal (pieces (getBoard vBoard))) > 5 -- Endgame protection
                             in doLMP || doFutility
                         else False

        if pruneQuiet
        then searchStage lms (index + 1) inCheck staticEval a b d flag bestScore bestM True -- Skip move
        else do
            let extension = if inCheck then depthOne else depthZero
            let nextDepth = (decDepth d) `plusDepth` extension

            -- PVS
            score <- if bestScore == -infinity -- First move
                     then do
                         s <- alphaBeta ctx newVBoard tt (Just m) nextDepth (-b) (-a) True nodeKind newCheckState nodes
                         return (stepScore s)
                     else do
                         -- Late Move Reduction
                         -- If quiet, depth >= 3, not checking, etc.
                         let lmr = if d >= mkDepth 3 && not isCap && not isProm && index >= 2 && not inCheck && not givesCheck
                                      && popCount (Base.occupiedTotal (pieces (getBoard vBoard))) > 5 -- Endgame protection
                                   then
                                       let dIdx = min 63 (unDepth d)
                                           mIdx = min 63 index
                                       in mkDepth (lmrTable U.! (dIdx * 64 + mIdx))
                                   else depthZero

                         let dLMR = nextDepth `minusDepth` lmr

                         -- NonPV Search (Null Window)
                         s <- alphaBeta ctx newVBoard tt (Just m) dLMR (-a - 1) (-a) True NonPV newCheckState nodes
                         let scoreLMR = stepScore s
                         if scoreLMR > a && scoreLMR < b
                         then do
                             -- Re-search with full window (PV node potentially)
                             s2 <- alphaBeta ctx newVBoard tt (Just m) nextDepth (-b) (-a) True nodeKind newCheckState nodes
                             return (stepScore s2)
                         else return scoreLMR

            let newBestScore = max bestScore score
            let newFlag = if score >= b then TTLower else if score > a then TTExact else flag
            let newBestM = if score > bestScore then m else bestM
            let newAlpha = max a score

            if score >= b
            then do
                 if not isCap && not isProm
                 then do
                     updateKillers ctx d m
                     updateHistory ctx d m
                     updateCounterMove ctx lastMove m
                 else return ()
                 return (score, TTLower, m, True, newAlpha) -- Fail high
            else searchStage lms (index + 1) inCheck staticEval newAlpha b d newFlag newBestScore newBestM True

    getMove (GenQuiet f t _) = Move f t Nothing
    getMove (GenCapture f t _ _) = Move f t Nothing
    getMove (GenEnPassant f t) = Move f t Nothing
    getMove (GenCastling f t) = Move f t Nothing
    getMove (GenPromotion f t p) = Move f t (Just p)
    getMove (GenPromotionCapture f t p _) = Move f t (Just p)
