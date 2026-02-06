{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

module Chess.Engine.Search.AlphaBeta where

import Data.Maybe (fromMaybe, isJust)
import Data.List (foldl')
import Data.Bits (popCount, (.&.))
import Data.IORef (IORef, newIORef, readIORef, modifyIORef')
import Control.Concurrent (forkIO, newEmptyMVar, putMVar, takeMVar, getNumCapabilities, newMVar, modifyMVar)
import qualified Data.Vector.Unboxed.Mutable as UM
import qualified Data.Vector.Unboxed as U

import Chess.Types
import Chess.Board (Board(..), applyMove, isCheck, uci, GenMove(..)
                   , pattern GenQuiet, pattern GenCapture, pattern GenEnPassant, pattern GenCastling, pattern GenPromotion, pattern GenPromotionCapture
                   , ValidatedBoard, trustBoard, getBoard, getGenMove
                   , applyLegalMove, isCapture, isPromotion, toGenMove, isLegalMove, mkLegalMove)
import qualified Chess.Board
import qualified Chess.Board.Base as Base
import qualified Chess.Board.GameState as GS
import Chess.Engine.Evaluation (Evaluate(..), evaluatePos)
import Chess.Board.Phase (Position(..))
import Chess.Engine.TT (TT, probeTT, storeTT, TTFlag(..))
import Chess.Engine.Search.Types
import Chess.Engine.Search.Pruning (lmrTable)
import Chess.Engine.Search.Ordering
import Chess.Engine.Search.Quiescence (quiescence)

-- | Search for the best move.
searchPhase :: forall p. Evaluate p => Position p -> TT -> SearchLimits -> IORef Bool -> IO Move
searchPhase (Position vBoard) tt limits stopFlag = do
    let board = getBoard vBoard
    let maxDepth = case limitDepth limits of
            Just d -> mkDepth d
            Nothing -> mkDepth 100
    nodes <- newIORef 0

    -- Initialize Search Resources
    killers <- UM.replicate 256 nullMove
    historyVec <- UM.replicate 4096 0
    counterMove <- UM.replicate 4096 nullMove
    let resources = SearchResources killers historyVec counterMove 128

    -- Initial Context
    let ctx = SearchContext
          { scResources = resources
          , scNodeKind = Root
          , scCheckState = NotInCheck
          , scPhase = MainSearch
          , scPly = 0
          , scNullMoveState = NullMoveAllowed
          } :: SearchContext p

    let loop depth bestM
          | depth > maxDepth = return bestM
          | otherwise = do
              -- Check stop flag before starting new iteration
              stop <- readIORef stopFlag
              if stop then return bestM
              else do
                  (move, score) <- alphaBetaRoot ctx vBoard tt depth nodes stopFlag

                  -- Check if stopped during search
                  stopAfter <- readIORef stopFlag
                  if stopAfter
                  then return bestM
                  else do
                      n <- readIORef nodes
                      let scoreStr = if abs score > 10000
                                     then "mate " ++ show ((if score > 0 then mateValue - score + 1 else -mateValue - score) `div` 2)
                                     else "cp " ++ show score
                      putStrLn $ "info depth " ++ show depth ++ " score " ++ scoreStr ++ " nodes " ++ show n ++ " pv " ++ uci move

                      loop (incDepth depth) move

    -- Iterative Deepening
    loop depthOne nullMove

-- | Root Search
alphaBetaRoot :: forall p. Evaluate p => SearchContext p -> ValidatedBoard -> TT -> Depth -> IORef Int -> IORef Bool -> IO (Move, Int)
alphaBetaRoot ctx vBoard tt depth nodes stopFlag = do
    let moves = Chess.Board.legalMovesValidated vBoard
    let board = getBoard vBoard
    let hash = GS.zobristHash (state board)
    ttEntry <- probeTT tt hash
    let ttMove = case ttEntry of Just (m, _, _, _) -> Just m; Nothing -> Nothing

    let sortedMoves = orderGenMoves vBoard moves ttMove

    case sortedMoves of
        [] -> return (nullMove, 0)
        (lm:lms) -> do
            let gm = getGenMove lm
            let m = getMove gm
            let newVBoard = applyLegalMove vBoard lm
            let givesCheck = isCheck (getBoard newVBoard)
            let newCheckState = if givesCheck then InCheck else NotInCheck

            let nextCtx = ctx
                  { scNodeKind = PV
                  , scCheckState = newCheckState
                  , scPly = scPly ctx + 1
                  , scNullMoveState = NullMoveAllowed
                  } :: SearchContext p

            s <- alphaBeta nextCtx newVBoard tt (Just m) (decDepth depth) (-infinity) infinity nodes stopFlag
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
                                            -- Check stop flag inside worker
                                            stop <- readIORef stopFlag
                                            if stop
                                            then do
                                                n <- readIORef localNodes
                                                return (bestRes, n)
                                            else do
                                                let gmWorker = getGenMove lmWorker
                                                let mWorker = getMove gmWorker
                                                let newVBWorker = applyLegalMove vBoard lmWorker
                                                let givesCheckWorker = isCheck (getBoard newVBWorker)
                                                let newCSWorker = if givesCheckWorker then InCheck else NotInCheck

                                                let workerCtx = ctx
                                                      { scNodeKind = PV
                                                      , scCheckState = newCSWorker
                                                      , scPly = scPly ctx + 1
                                                      , scNullMoveState = NullMoveAllowed
                                                      } :: SearchContext p

                                                s <- alphaBeta workerCtx newVBWorker tt (Just mWorker) (decDepth depth) (-infinity) (-bestScore) localNodes stopFlag
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
        stop <- readIORef stopFlag
        if stop then return (bestM, bestScore)
        else do
            let gm = getGenMove lm
            let m = getMove gm
            let newVBoard = applyLegalMove vBoard lm
            let givesCheck = isCheck (getBoard newVBoard)
            let newCheckState = if givesCheck then InCheck else NotInCheck

            let nextCtx = ctx
                  { scNodeKind = NonPV
                  , scCheckState = newCheckState
                  , scPly = scPly ctx + 1
                  , scNullMoveState = NullMoveAllowed
                  } :: SearchContext p

            let newAlpha = max alpha bestScore
            s <- alphaBeta nextCtx newVBoard tt (Just m) (decDepth depth) (-beta) (-newAlpha) nodes stopFlag
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

-- | Alpha-Beta Search
alphaBeta :: forall p. Evaluate p => SearchContext p -> ValidatedBoard -> TT -> Maybe Move -> Depth -> Int -> Int -> IORef Int -> IORef Bool -> IO Int
alphaBeta ctx vBoard tt lastMove depth alpha beta nodes stopFlag = do
    n <- modifyIORef' nodes (+1) >> readIORef nodes
    if (n .&. 2047 == 0)
    then do
        stop <- readIORef stopFlag
        if stop then return 0 -- Abort value
        else alphaBetaBody ctx vBoard tt lastMove depth alpha beta nodes stopFlag
    else alphaBetaBody ctx vBoard tt lastMove depth alpha beta nodes stopFlag

-- | Alpha-Beta Search Body
alphaBetaBody :: forall p. Evaluate p => SearchContext p -> ValidatedBoard -> TT -> Maybe Move -> Depth -> Int -> Int -> IORef Int -> IORef Bool -> IO Int
alphaBetaBody ctx vBoard tt lastMove depth alpha beta nodes stopFlag = do
    let board = getBoard vBoard
    let hash = GS.zobristHash (state board)

    let isRep = hash `elem` history board
    if isRep && not (isZeroDepth depth)
    then return 0
    else do
        let nodeKind = scNodeKind ctx
        let checkState = scCheckState ctx
        let inCheck = case checkState of InCheck -> True; NotInCheck -> False

        ttEntry <- probeTT tt hash
        let (ttMove, ttScore, ttDepth, ttFlag) = case ttEntry of
                Just (m, s, d, f) -> (Just m, s, d, f)
                Nothing -> (Nothing, 0, mkDepth (-1), TTExact)

        let ttHit = isJust ttEntry && ttDepth >= depth
        let ttCutoff = if ttHit
                       then case ttFlag of
                           TTExact -> True
                           TTLower -> ttScore >= beta
                           TTUpper -> ttScore <= alpha
                           TTEval  -> False
                       else False

        if ttCutoff && abs ttScore < 15000
        then return ttScore
        else do
            -- Static Evaluation using Evaluate Typeclass
            let staticEval = evaluatePos (Position vBoard :: Position p)

            if isZeroDepth depth
            then do
                 let qsCtx = ctx { scPhase = Quiescence } :: SearchContext p
                 quiescence qsCtx vBoard tt alpha beta nodes depth
            else do
                let r = if depth > mkDepth 6 then mkDepth 3 else mkDepth 2
                let canNull = scNullMoveState ctx == NullMoveAllowed
                let doNmp = canNull && not inCheck && depth >= r && beta < mateValue
                            && staticEval >= beta
                            && popCount (Base.occupiedTotal (pieces board)) > 5

                nmpResult <- if doNmp
                             then do
                                 let nullB = Chess.Board.applyMove board NullMove
                                 let nullVB = trustBoard nullB
                                 let d' = depth `minusDepth` depthOne `minusDepth` r

                                 let nmpCtx = ctx
                                       { scNullMoveState = NullMoveSkipped
                                       , scPly = scPly ctx + 1
                                       , scNodeKind = NonPV
                                       , scCheckState = NotInCheck
                                       } :: SearchContext p

                                 score <- alphaBeta nmpCtx nullVB tt Nothing d' (-beta) (-beta + 1) nodes stopFlag
                                 return (if stepScore score >= beta then Just beta else Nothing)
                             else return Nothing

                case nmpResult of
                    Just cutoff -> return cutoff
                    Nothing -> do
                        let hasTT = isJust ttMove
                        let ttM = fromMaybe nullMove ttMove

                        (score0, flag0, bestM0, found0, alpha0, searchedTT) <- if hasTT
                            then do
                                if Chess.Board.isLegalMove board ttM
                                then do
                                    case Chess.Board.toGenMove board ttM of
                                        Just gm -> do
                                            let lm = Chess.Board.mkLegalMove gm
                                            let newVBoard = applyLegalMove vBoard lm
                                            let givesCheck = isCheck (getBoard newVBoard)
                                            let newCheckState = if givesCheck then InCheck else NotInCheck
                                            let extension = if inCheck then depthOne else depthZero
                                            let nextDepth = (decDepth depth) `plusDepth` extension

                                            let nextCtx = ctx
                                                  { scNodeKind = nodeKind
                                                  , scCheckState = newCheckState
                                                  , scPly = scPly ctx + 1
                                                  , scNullMoveState = NullMoveAllowed
                                                  } :: SearchContext p

                                            s <- alphaBeta nextCtx newVBoard tt (Just ttM) nextDepth (-beta) (-alpha) nodes stopFlag
                                            let score = stepScore s

                                            if score >= beta
                                            then return (score, TTLower, ttM, True, alpha, True)
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

                            let captures = Chess.Board.captureMovesValidated vBoard
                            let (goodCaps, badCaps) = partitionSEE vBoard captures
                            let sortedGood = orderGenMoves vBoard (filterTT goodCaps) sortingTT

                            (score1, flag1, bestM1, found1, alpha1) <- searchStage sortedGood (0 :: Int) inCheck staticEval alpha0 beta depth flag0 score0 bestM0 found0

                            if score1 >= beta
                            then storeAndReturn score1 bestM1 TTLower
                            else do
                                let promotions = filter (not . isCapture) (Chess.Board.legalPromotionsValidated vBoard)
                                let sortedPromotions = orderGenMoves vBoard (filterTT promotions) sortingTT

                                (score2, flag2, bestM2, found2, alpha2) <- searchStage sortedPromotions (0 :: Int) inCheck staticEval alpha1 beta depth flag1 score1 bestM1 found1

                                if score2 >= beta
                                then storeAndReturn score2 bestM2 TTLower
                                else do
                                    let quiets = Chess.Board.legalQuietsValidated vBoard
                                    killers <- getKillers ctx depth
                                    counterMove <- getCounterMove ctx lastMove
                                    sortedQuiets <- orderQuiets ctx (filterTT quiets) killers counterMove sortingTT

                                    (score3, flag3, bestM3, found3, alpha3) <- searchStage sortedQuiets (0 :: Int) inCheck staticEval alpha2 beta depth flag2 score2 bestM2 found2

                                    if score3 >= beta
                                    then storeAndReturn score3 bestM3 flag3
                                    else do
                                        let sortedBad = orderGenMoves vBoard (filterTT badCaps) sortingTT
                                        (score4, flag4, bestM4, found4, _) <- searchStage sortedBad (0 :: Int) inCheck staticEval alpha3 beta depth flag3 score3 bestM3 found3

                                        if not found4
                                        then return $ if inCheck then -mateValue else 0
                                        else storeAndReturn score4 bestM4 flag4

  where
    storeAndReturn s m f = do
        stop <- readIORef stopFlag
        if stop
        then return s
        else do
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
        let givesCheck = isCheck (getBoard newVBoard)
        let newCheckState = if givesCheck then InCheck else NotInCheck

        let pruneQuiet = case (scNodeKind ctx, scCheckState ctx) of
                (NonPV, NotInCheck) | isQuiet && not givesCheck ->
                             let lmpCount = 3 + dVal * dVal
                                 doLMP = dVal < 8 && dVal >= 3 && index > lmpCount
                                 fpMargin = 100 * dVal
                                 doFutility = index > 10 && dVal < 7 && dVal >= 2 && abs bestScore < mateValue && abs a < mateValue && staticEval + fpMargin <= a
                                              && popCount (Base.occupiedTotal (pieces (getBoard vBoard))) > 5
                             in doLMP || doFutility
                _ -> False

        if pruneQuiet
        then searchStage lms (index + 1) inCheck staticEval a b d flag bestScore bestM True
        else do
            let extension = if inCheck then depthOne else depthZero
            let nextDepth = (decDepth d) `plusDepth` extension

            score <- if bestScore == -infinity
                     then do
                         let nextCtx = ctx
                               { scNodeKind = scNodeKind ctx
                               , scCheckState = newCheckState
                               , scPly = scPly ctx + 1
                               , scNullMoveState = NullMoveAllowed
                               } :: SearchContext p
                         s <- alphaBeta nextCtx newVBoard tt (Just m) nextDepth (-b) (-a) nodes stopFlag
                         return (stepScore s)
                     else do
                         let lmr = if d >= mkDepth 3 && not isCap && not isProm && index >= 2 && not inCheck && not givesCheck
                                      && popCount (Base.occupiedTotal (pieces (getBoard vBoard))) > 5
                                   then
                                       let dIdx = min 63 (unDepth d)
                                           mIdx = min 63 index
                                       in mkDepth (lmrTable U.! (dIdx * 64 + mIdx))
                                   else depthZero

                         let dLMR = nextDepth `minusDepth` lmr

                         let lmrCtx = ctx
                               { scNodeKind = NonPV
                               , scCheckState = newCheckState
                               , scPly = scPly ctx + 1
                               , scNullMoveState = NullMoveAllowed
                               } :: SearchContext p

                         s <- alphaBeta lmrCtx newVBoard tt (Just m) dLMR (-a - 1) (-a) nodes stopFlag
                         let scoreLMR = stepScore s
                         if scoreLMR > a && scoreLMR < b
                         then do
                             let researchCtx = ctx
                                   { scNodeKind = scNodeKind ctx
                                   , scCheckState = newCheckState
                                   , scPly = scPly ctx + 1
                                   , scNullMoveState = NullMoveAllowed
                                   } :: SearchContext p
                             s2 <- alphaBeta researchCtx newVBoard tt (Just m) nextDepth (-b) (-a) nodes stopFlag
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
                 return (score, TTLower, m, True, newAlpha)
            else searchStage lms (index + 1) inCheck staticEval newAlpha b d newFlag newBestScore newBestM True

    getMove (GenQuiet f t _) = Move f t Nothing
    getMove (GenCapture f t _ _) = Move f t Nothing
    getMove (GenEnPassant f t) = Move f t Nothing
    getMove (GenCastling f t) = Move f t Nothing
    getMove (GenPromotion f t p) = Move f t (Just p)
    getMove (GenPromotionCapture f t p _) = Move f t (Just p)
