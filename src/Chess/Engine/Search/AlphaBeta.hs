{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Chess.Engine.Search.AlphaBeta where

import Data.Maybe (fromMaybe, isJust)
import Data.Bits ((.&.))
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef, modifyIORef', writeIORef)
import qualified Data.List as List
import Control.Concurrent (newMVar, modifyMVar, getNumCapabilities)
import Control.Concurrent.Async (mapConcurrently)
import qualified Data.Vector.Unboxed.Mutable as UM
import qualified Data.Vector.Unboxed as U

import Chess.Types
import Chess.Bitboard (moreThan5)
import Chess.Board (Board(..), applyMove, uci, genMoveToMove
                   , ValidatedBoard, SomeValidatedBoard(..), trustBoard, getBoard, getGenMove, MoveGenerator(..)
                   , applyLegalMoveValidated, isCapture, isPromotion, toGenMove, mkLegalMove)
import qualified Chess.Board.Base as Base
import qualified Chess.Board.GameState as GS
import qualified Chess.Board.MoveGen as MoveGen
import qualified Chess.Board.MoveGen.KingSafety as KingSafety
import Chess.Engine.Evaluation (Evaluate(..), evaluatePos)
import Chess.Board.Phase (Position(..))
import Chess.Engine.TT (TT, cloneTT, probeTT, storeTT, TTFlag(..), probeTTFast, unpackDataFast)
import Chess.Engine.Search.Types
import Chess.Engine.Search.Pruning (lmrTable)
import Chess.Engine.Search.Ordering
import qualified Chess.Engine.Search.Ordering as Ordering
import Chess.Engine.Search.Quiescence (quiescence)

-- | Search for the best move.
searchPhase :: forall p s. (Evaluate p, MoveGenerator s) => Position p s -> TT -> SearchLimits -> IORef Bool -> IO Move
searchPhase (Position vBoard) tt limits stopFlag = do
    let board = getBoard vBoard
    let age = fromIntegral $ unFullmoveNumber $ GS.fullmoveNumber (state board)
    let maxDepth = case limitDepth limits of
            Just d -> mkDepth d
            Nothing -> mkDepth 100
    nodes <- newIORef 0

    -- Get initial legal moves to have a fallback
    let moves = legalMovesValidated vBoard
    let initialBestMove = case moves of
            [] -> nullMove
            (lm:_) -> genMoveToMove (getGenMove lm)

    -- Initialize Search Resources
    resources <- newSearchResources 128

    -- Check Status Value (for context)
    let checkStatus = case trustBoard board of InCheckBoard _ -> InCheck; NotInCheckBoard _ -> NotInCheck

    -- Initial Context
    let ctx = SearchContext
          { scResources = resources
          , scNodeKind = Root
          , scCheckState = checkStatus
          , scPhase = MainSearch
          , scPly = 0
          , scNullMoveState = NullMoveAllowed
          , scAge = age
          } :: SearchContext p

    let searchLoop !depth !bestM = do
          if depth > maxDepth
          then return bestM
          else do
              stop <- readIORef stopFlag
              if stop then return bestM
              else do
                  (move, score) <- alphaBetaRoot ctx vBoard tt depth nodes stopFlag limits

                  stopAfter <- readIORef stopFlag
                  if stopAfter
                  then return bestM
                  else do
                      n <- readIORef nodes
                      putStrLn $ "info depth " ++ show depth ++ " score " ++ formatScore score ++ " nodes " ++ show n ++ " pv " ++ uci move

                      let stopMate = case limitMate limits of
                              Just m -> (abs score > 10000) && ((mateValue - abs score + 1) `div` 2 <= m)
                              Nothing -> False

                      if stopMate
                      then return move
                      else searchLoop (incDepth depth) move

    -- Iterative Deepening
    if initialBestMove == nullMove
    then return nullMove
    else searchLoop depthOne initialBestMove

-- | Root Search
alphaBetaRoot :: forall p s. (Evaluate p, MoveGenerator s) => SearchContext p -> ValidatedBoard s -> TT -> Depth -> IORef Int -> IORef Bool -> SearchLimits -> IO (Move, Int)
alphaBetaRoot ctx vBoard tt depth nodes stopFlag limits = do
    let moves = legalMovesValidated vBoard
    let board = getBoard vBoard
    let hash = GS.zobristHash (state board)
    (ttKey, ttData) <- probeTTFast tt hash
    let ttHit = ttKey == hash
    let (!ttMove, _, _, _) = if ttHit then unpackDataFast ttData else (nullMove, 0, mkDepth (-1), TTExact)

    let sortedMoves = Ordering.orderGenMoves vBoard moves (if ttHit then Just ttMove else Nothing)

    -- Helper to find first legal move
    let inCheck = case scCheckState ctx of InCheck -> True; NotInCheck -> False

    -- Hoist board fields
    let !bb = pieces board
    let !st = state board

    let !dcBitboard = KingSafety.discoveryCandidates bb (GS.turn st)

    -- We know moves are legal, so we just process the first one.
    let processFirstMove [] = return Nothing
        processFirstMove (lm:lms) = do
            let gm = getGenMove lm
            let m = genMoveToMove gm
            let givesCheck = KingSafety.givesCheckOptimized bb st dcBitboard gm

            do
                (s, _) <- case applyLegalMoveValidated vBoard lm givesCheck of
                    InCheckBoard newVBoard -> do
                        let nextCtx = ctx { scNodeKind = PV, scCheckState = InCheck, scPly = scPly ctx + 1, scNullMoveState = NullMoveAllowed } :: SearchContext p
                        s' <- alphaBeta nextCtx newVBoard tt (Just m) (decDepth depth) (-infinity) infinity nodes stopFlag limits
                        return (s', InCheck)
                    NotInCheckBoard newVBoard -> do
                        let nextCtx = ctx { scNodeKind = PV, scCheckState = NotInCheck, scPly = scPly ctx + 1, scNullMoveState = NullMoveAllowed } :: SearchContext p
                        s' <- alphaBeta nextCtx newVBoard tt (Just m) (decDepth depth) (-infinity) infinity nodes stopFlag limits
                        return (s', NotInCheck)
                return $ Just (lm, stepScore s, lms)

    firstResult <- processFirstMove sortedMoves

    let go [] bestM bestScore _ _ = return (bestM, bestScore)
        go (lm:lms) !bestM !bestScore !alpha !beta = do
            stop <- readIORef stopFlag
            if stop then return (bestM, bestScore)
            else do
                let gm = getGenMove lm
                let m = genMoveToMove gm
                let givesCheck = KingSafety.givesCheckOptimized bb st dcBitboard gm

                do
                    score <- case applyLegalMoveValidated vBoard lm givesCheck of
                        InCheckBoard newVBoard -> do
                            let nextCtx = ctx { scNodeKind = NonPV, scCheckState = InCheck, scPly = scPly ctx + 1, scNullMoveState = NullMoveAllowed } :: SearchContext p
                            let newAlpha = max alpha bestScore
                            s <- alphaBeta nextCtx newVBoard tt (Just m) (decDepth depth) (-beta) (-newAlpha) nodes stopFlag limits
                            return (stepScore s)
                        NotInCheckBoard newVBoard -> do
                            let nextCtx = ctx { scNodeKind = NonPV, scCheckState = NotInCheck, scPly = scPly ctx + 1, scNullMoveState = NullMoveAllowed } :: SearchContext p
                            let newAlpha = max alpha bestScore
                            s <- alphaBeta nextCtx newVBoard tt (Just m) (decDepth depth) (-beta) (-newAlpha) nodes stopFlag limits
                            return (stepScore s)

                    if score > bestScore
                    then go lms m score alpha beta
                    else go lms bestM bestScore alpha beta

    case firstResult of
        Nothing -> do
             -- No moves found (stalemate/mate checked elsewhere usually, but here return null)
             let score = if inCheck then -mateValue else 0
             return (nullMove, score)

        Just (lm, bestScore, lms) -> do
            let bestMove = genMoveToMove (getGenMove lm)

            if null lms then return (bestMove, bestScore)
            else do
                caps <- getNumCapabilities
                if caps <= 1
                then go lms bestMove bestScore (-infinity) infinity
                else do
                    -- Parallel Search: Chunk remaining moves and distribute to workers
                    -- lms is already sorted by ordering
                    queue <- newMVar lms

                    let worker _ = do
                            localNodes <- newIORef 0
                            localResources <- cloneSearchResources (scResources ctx)
                            localTT <- cloneTT tt
                            let workerBaseCtx = ctx { scResources = localResources } :: SearchContext p

                            -- Precalculate Discovery Candidates for optimization
                            -- This is per-worker but computed once per search call effectively.
                            let !_dcBitboard = KingSafety.discoveryCandidates (pieces board) (GS.turn (state board))

                            let loop bestRes = do
                                    mbMove <- modifyMVar queue $ \ms -> case ms of
                                        [] -> return ([], Nothing)
                                        (m:rest) -> return (rest, Just m)

                                    case mbMove of
                                        Nothing -> do
                                            n <- readIORef localNodes
                                            return (bestRes, n)
                                        Just lmWorker -> do
                                            stop <- readIORef stopFlag
                                            if stop
                                            then do
                                                n <- readIORef localNodes
                                                return (bestRes, n)
                                            else do
                                                let gmWorker = getGenMove lmWorker
                                                let mWorker = genMoveToMove gmWorker
                                                let givesCheckWorker = KingSafety.givesCheckOptimized (pieces board) (state board) dcBitboard gmWorker

                                                do
                                                    searchScore <- case applyLegalMoveValidated vBoard lmWorker givesCheckWorker of
                                                        InCheckBoard newVBWorker -> do
                                                            let workerCtx = workerBaseCtx { scNodeKind = PV, scCheckState = InCheck, scPly = scPly ctx + 1, scNullMoveState = NullMoveAllowed } :: SearchContext p
                                                            s <- alphaBeta workerCtx newVBWorker localTT (Just mWorker) (decDepth depth) (-infinity) (-bestScore) localNodes stopFlag limits
                                                            return (stepScore s)
                                                        NotInCheckBoard newVBWorker -> do
                                                            let workerCtx = workerBaseCtx { scNodeKind = PV, scCheckState = NotInCheck, scPly = scPly ctx + 1, scNullMoveState = NullMoveAllowed } :: SearchContext p
                                                            s <- alphaBeta workerCtx newVBWorker localTT (Just mWorker) (decDepth depth) (-infinity) (-bestScore) localNodes stopFlag limits
                                                            return (stepScore s)

                                                    let newBestRes = case bestRes of
                                                            Nothing -> Just (mWorker, searchScore)
                                                            Just (_, bs) -> if searchScore > bs then Just (mWorker, searchScore) else bestRes

                                                    loop newBestRes

                            loop Nothing

                    results <- mapConcurrently worker [1..caps]

                    let (finalM, finalS) = List.foldl' merge (bestMove, bestScore) (map fst results)
                    let totalNodes = sum (map snd results)
                    modifyIORef' nodes (+ totalNodes)
                    return (finalM, finalS)

  where
    merge (bm, bs) Nothing = (bm, bs)
    merge (bm, bs) (Just (m, s)) = if s > bs then (m, s) else (bm, bs)


formatScore :: Int -> String
formatScore score
    | abs score > 10000 = "mate " ++ show ((if score > 0 then mateValue - score + 1 else -mateValue - score) `div` 2)
    | otherwise = "cp " ++ show score

newSearchResources :: Int -> IO SearchResources
newSearchResources maxDepth = do
    killers <- UM.replicate (maxDepth * 2) nullMove
    historyVec <- UM.replicate 4096 0
    counterMove <- UM.replicate 4096 nullMove
    return $ SearchResources killers historyVec counterMove maxDepth

cloneSearchResources :: SearchResources -> IO SearchResources
cloneSearchResources (SearchResources killers historyVec counterMove maxDepth) = do
    killers' <- UM.clone killers
    historyVec' <- UM.clone historyVec
    counterMove' <- UM.clone counterMove
    return $ SearchResources killers' historyVec' counterMove' maxDepth

-- | Alpha-Beta Search
alphaBeta :: forall p s. (Evaluate p, MoveGenerator s) => SearchContext p -> ValidatedBoard s -> TT -> Maybe Move -> Depth -> Int -> Int -> IORef Int -> IORef Bool -> SearchLimits -> IO Int
alphaBeta ctx vBoard tt lastMove depth alpha beta nodes stopFlag limits = do
    n <- atomicModifyIORef' nodes $ \i ->
        let j = i + 1
        in (j, j)
    if (n .&. 2047 == 0)
    then do
        -- Check Stop Flag
        stop <- readIORef stopFlag
        if stop then return 0
        else do
            -- Check Node Limit
            case limitNodes limits of
                Just ln | n >= ln -> do
                    writeIORef stopFlag True
                    return 0
                _ -> alphaBetaBody ctx vBoard tt lastMove depth alpha beta nodes stopFlag limits
    else alphaBetaBody ctx vBoard tt lastMove depth alpha beta nodes stopFlag limits

-- | Alpha-Beta Search Body
alphaBetaBody :: forall p s. (Evaluate p, MoveGenerator s) => SearchContext p -> ValidatedBoard s -> TT -> Maybe Move -> Depth -> Int -> Int -> IORef Int -> IORef Bool -> SearchLimits -> IO Int
alphaBetaBody ctx vBoard tt lastMove depth alpha beta nodes stopFlag limits = do
    let board = getBoard vBoard
    let hash = GS.zobristHash (state board)

    let isRep = hash `elem` history board
    if isRep && not (isZeroDepth depth)
    then return 0
    else do
        let nodeKind = scNodeKind ctx
        let checkState = scCheckState ctx
        let inCheck = case checkState of InCheck -> True; NotInCheck -> False

        -- Mate Distance Pruning
        let mateScore = mateValue - scPly ctx
        let alpha' = max alpha (-mateScore)
        let beta'  = min beta (mateScore - 1)
        if alpha' >= beta'
        then return alpha'
        else do


                    (ttKey, ttData) <- probeTTFast tt hash
                    let entryHit = ttKey == hash
                    let (!ttMove, !ttScore, !ttDepth, !ttFlag) = if entryHit then unpackDataFast ttData else (nullMove, 0, mkDepth (-1), TTExact)

                    let ttHit = entryHit && ttDepth >= depth
                    let ttCutoff = if ttHit
                                   then case ttFlag of
                                       TTExact -> True
                                       TTLower -> ttScore >= beta'
                                       TTUpper -> ttScore <= alpha'
                                       TTEval  -> False
                                   else False

                    if ttCutoff && abs ttScore < 15000
                    then return ttScore
                    else do
                        -- Static Evaluation using Evaluate Typeclass
                        let staticEval = evaluatePos (Position vBoard :: Position p s)

                        if isZeroDepth depth
                        then do
                             let qsCtx = ctx { scPhase = Quiescence } :: SearchContext p
                             quiescence qsCtx vBoard tt alpha' beta' nodes depth
                        else do
                            let depthVal = unDepth depth

                            -- Razoring
                            let doRazoring = depthVal <= 2 && not inCheck && nodeKind == NonPV
                            razoringResult <- if doRazoring
                                              then do
                                                  let razoringMargin = 300 * depthVal
                                                  if staticEval + razoringMargin <= alpha'
                                                  then do
                                                      let qsCtx = ctx { scPhase = Quiescence } :: SearchContext p
                                                      qsScore <- quiescence qsCtx vBoard tt alpha' beta' nodes depthZero
                                                      return $ if qsScore <= alpha' then Just qsScore else Nothing
                                                  else return Nothing
                                              else return Nothing

                            case razoringResult of
                              Just score -> return score
                              Nothing -> do

                                -- Reverse Futility Pruning (Static NMP)
                                let doRFP = depthVal <= 6 && not inCheck && nodeKind == NonPV && abs beta' < mateValue
                                let rfpResult = if doRFP
                                                then do
                                                    let futilityMargin = 120 * depthVal
                                                    if staticEval - futilityMargin >= beta'
                                                    then Just staticEval
                                                    else Nothing
                                                else Nothing

                                case rfpResult of
                                  Just score -> return score
                                  Nothing -> do

                                    let r = if depth > mkDepth 6 then mkDepth 3 else mkDepth 2
                                    let canNull = scNullMoveState ctx == NullMoveAllowed
                                    let doNmp = canNull && not inCheck && depth >= r && beta' < mateValue
                                                && staticEval >= beta'
                                                && moreThan5 (Base.occupiedTotal (pieces board))

                                    nmpResult <- if doNmp
                                                 then do
                                                     let nullB = Chess.Board.applyMove board NullMove
                                                     -- trustBoard calculates check status for null move position
                                                     -- Null move switches turn. If we were not in check, opponent might be in check?
                                                     -- No, if we make null move, we pass. Opponent to move.
                                                     -- isCheck checks if side to move is in check.
                                                     -- We verified 'not inCheck' before NMP.
                                                     -- So opponent is not capturing our king.
                                                     -- After null move, it is opponent's turn.
                                                     -- Is opponent in check? Unlikely unless we were giving check (which we are not, it's our turn).
                                                     -- Wait, if it is our turn and we are not in check.
                                                     -- Null move -> Opponent's turn.
                                                     -- Opponent is effectively in same position but it's their turn.
                                                     -- If we were not attacking their king, they are not in check.
                                                     -- So we can assume NotInCheck?
                                                     -- Safest is to use trustBoard.
                                                     let nullVB = trustBoard nullB
                                                     let d' = depth `minusDepth` depthOne `minusDepth` r

                                                     let nmpCtx = ctx { scNullMoveState = NullMoveSkipped, scPly = scPly ctx + 1, scNodeKind = NonPV, scCheckState = NotInCheck } :: SearchContext p

                                                     score <- case nullVB of
                                                         InCheckBoard vb -> alphaBeta nmpCtx vb tt Nothing d' (-beta') (-beta' + 1) nodes stopFlag limits
                                                         NotInCheckBoard vb -> alphaBeta nmpCtx vb tt Nothing d' (-beta') (-beta' + 1) nodes stopFlag limits
                                                     return (if stepScore score >= beta' then Just beta' else Nothing)
                                                 else return Nothing

                                    case nmpResult of
                                        Just cutoff -> return cutoff
                                        Nothing -> do
                                            let hasTT = not (isNullMove ttMove)
                                            let ttM = ttMove

                                            -- Precalculate Discovery Candidates for optimized givesCheck
                                            -- We only do this if we are likely to search moves.
                                            let dcBitboard = KingSafety.discoveryCandidates (pieces board) (GS.turn (state board))

                                            (score0, flag0, bestM0, found0, alpha0, searchedTT) <- if hasTT
                                                then do
                                                    if MoveGen.isLegalMove (pieces board) currentState ttM
                                                    then do
                                                        case Chess.Board.toGenMove board ttM of
                                                            Just gm -> do
                                                                let lm = Chess.Board.mkLegalMove gm
                                                                let givesCheck = KingSafety.givesCheckOptimized (pieces board) (state board) dcBitboard gm

                                                                score <- case applyLegalMoveValidated vBoard lm givesCheck of
                                                                    InCheckBoard newVBoard -> do
                                                                        let extension = if inCheck then depthOne else depthZero
                                                                        let nextDepth = (decDepth depth) `plusDepth` extension
                                                                        let nextCtx = ctx { scNodeKind = nodeKind, scCheckState = InCheck, scPly = scPly ctx + 1, scNullMoveState = NullMoveAllowed } :: SearchContext p
                                                                        s <- alphaBeta nextCtx newVBoard tt (Just ttM) nextDepth (-beta') (-alpha') nodes stopFlag limits
                                                                        return (stepScore s)
                                                                    NotInCheckBoard newVBoard -> do
                                                                        let extension = if inCheck then depthOne else depthZero
                                                                        let nextDepth = (decDepth depth) `plusDepth` extension
                                                                        let nextCtx = ctx { scNodeKind = nodeKind, scCheckState = NotInCheck, scPly = scPly ctx + 1, scNullMoveState = NullMoveAllowed } :: SearchContext p
                                                                        s <- alphaBeta nextCtx newVBoard tt (Just ttM) nextDepth (-beta') (-alpha') nodes stopFlag limits
                                                                        return (stepScore s)

                                                                if score >= beta'
                                                                then return (score, TTLower, ttM, True, alpha', True)
                                                                else do
                                                                    let newBestScore = max (-infinity) score
                                                                    let newAlpha = max alpha' score
                                                                    let newFlag = if score > alpha' then TTExact else TTUpper
                                                                    let newBestM = if score > -infinity then ttM else nullMove
                                                                    return (newBestScore, newFlag, newBestM, True, newAlpha, True)
                                                            Nothing -> return (-infinity, TTUpper, nullMove, False, alpha', False)
                                                    else return (-infinity, TTUpper, nullMove, False, alpha', False)
                                                else return (-infinity, TTUpper, nullMove, False, alpha', False)

                                            if score0 >= beta'
                                            then storeAndReturn score0 bestM0 TTLower
                                            else do
                                                let filterTT ms = if searchedTT then filter (\lm -> genMoveToMove (getGenMove lm) /= ttM) ms else ms
                                                let sortingTT = if searchedTT then Nothing else Just ttM

                                                let captures = captureMovesValidated vBoard
                                                let (goodCaps, badCaps) = partitionSEE vBoard captures
                                                let sortedGood = Ordering.pickAndSort (filterTT goodCaps) sortingTT

                                                (score1, flag1, bestM1, found1, alpha1, playedQuiets1, nextIndex1) <- searchStage dcBitboard sortedGood (0 :: Int) inCheck staticEval alpha0 beta' depth flag0 score0 bestM0 found0 []

                                                if score1 >= beta'
                                                then storeAndReturn score1 bestM1 TTLower
                                                else do
                                                    let promotions = filter (not . isCapture) (legalPromotionsValidated vBoard)
                                                    let sortedPromotions = Ordering.pickAndSort (filterTT promotions) sortingTT

                                                    (score2, flag2, bestM2, found2, alpha2, playedQuiets2, nextIndex2) <- searchStage dcBitboard sortedPromotions nextIndex1 inCheck staticEval alpha1 beta' depth flag1 score1 bestM1 found1 playedQuiets1

                                                    if score2 >= beta'
                                                    then storeAndReturn score2 bestM2 TTLower
                                                    else do
                                                        let quiets = legalQuietsValidated vBoard
                                                        killers <- getKillers ctx depth
                                                        counterMove <- getCounterMove ctx lastMove
                                                        sortedQuiets <- orderQuiets ctx (filterTT quiets) killers counterMove sortingTT

                                                        (score3, flag3, bestM3, found3, alpha3, playedQuiets3, nextIndex3) <- searchStage dcBitboard sortedQuiets nextIndex2 inCheck staticEval alpha2 beta' depth flag2 score2 bestM2 found2 playedQuiets2

                                                        if score3 >= beta'
                                                        then storeAndReturn score3 bestM3 flag3
                                                        else do
                                                            let sortedBad = Ordering.pickAndSort (filterTT badCaps) sortingTT
                                                            (score4, flag4, bestM4, found4, _, _, _) <- searchStage dcBitboard sortedBad nextIndex3 inCheck staticEval alpha3 beta' depth flag3 score3 bestM3 found3 playedQuiets3

                                                            if not found4
                                                            then return $ if inCheck then -mateValue else 0
                                                            else storeAndReturn score4 bestM4 flag4

  where
    -- Hoist fields for searchStage
    !currentBoard = getBoard vBoard
    !currentPieces = pieces currentBoard
    !currentState = state currentBoard

    storeAndReturn s m f = do
        stop <- readIORef stopFlag
        if stop
        then return s
        else do
            let hash = GS.zobristHash currentState
            storeTT tt (scAge ctx) hash depth s f m
            return s

    searchStage _ [] !index _ _ a _ _ flag bestScore bestM found playedQuiets = return (bestScore, flag, bestM, found, a, playedQuiets, index)
    searchStage dcBitboard (lm:lms) !index inCheck staticEval !a !b !d !flag !bestScore !bestM !_found !playedQuiets = do
        let isCap = isCapture lm
        let isProm = isPromotion lm
        let isQuiet = not isCap && not isProm
        let dVal = unDepth d

        let gm = getGenMove lm
        let m = genMoveToMove gm

        -- Optimized givesCheck
        let givesCheck = KingSafety.givesCheckOptimized currentPieces currentState dcBitboard gm

        let pruneQuiet = case (scNodeKind ctx, scCheckState ctx) of
                (NonPV, NotInCheck) | isQuiet && not givesCheck -> -- optimized prune logic knowing NotInCheck
                             let lmpCount = 3 + dVal * dVal
                                 doLMP = dVal < 8 && dVal >= 3 && index > lmpCount
                                 fpMargin = 120 * dVal
                                 doFutility = index > 6 && dVal < 7 && dVal >= 2 && abs bestScore < mateValue && abs a < mateValue && staticEval + fpMargin <= a
                                              && moreThan5 (Base.occupiedTotal currentPieces)
                             in doLMP || doFutility
                _ -> False

        if pruneQuiet
        then searchStage dcBitboard lms (index + 1) inCheck staticEval a b d flag bestScore bestM True playedQuiets
        else do
                let nextPlayedQuiets = if isQuiet then m : playedQuiets else playedQuiets
                let extension = if inCheck then depthOne else depthZero
                let nextDepth = (decDepth d) `plusDepth` extension

                let !nextVB = applyLegalMoveValidated vBoard lm givesCheck

                score <- if bestScore == -infinity
                         then do
                             case nextVB of
                                 InCheckBoard newVBoard -> do
                                     let nextCtx = ctx { scNodeKind = scNodeKind ctx, scCheckState = InCheck, scPly = scPly ctx + 1, scNullMoveState = NullMoveAllowed } :: SearchContext p
                                     s <- alphaBeta nextCtx newVBoard tt (Just m) nextDepth (-b) (-a) nodes stopFlag limits
                                     return (stepScore s)
                                 NotInCheckBoard newVBoard -> do
                                     let nextCtx = ctx { scNodeKind = scNodeKind ctx, scCheckState = NotInCheck, scPly = scPly ctx + 1, scNullMoveState = NullMoveAllowed } :: SearchContext p
                                     s <- alphaBeta nextCtx newVBoard tt (Just m) nextDepth (-b) (-a) nodes stopFlag limits
                                     return (stepScore s)
                     else do
                         -- givesCheck is already computed.
                         let lmr = if d >= mkDepth 3 && not isCap && not isProm && index >= 2 && not inCheck && not givesCheck
                                      && moreThan5 (Base.occupiedTotal currentPieces)
                                   then
                                       let dIdx = min 63 (unDepth d)
                                           mIdx = min 63 index
                                           baseLmr = lmrTable U.! (dIdx * 64 + mIdx)
                                           -- Less reduction for PV nodes
                                           pvAdj = if scNodeKind ctx == PV then 1 else 0
                                           finalLmr = max 0 (baseLmr - pvAdj)
                                       in mkDepth finalLmr
                                   else depthZero

                         let dLMR = nextDepth `minusDepth` lmr

                         scoreLMR <- case nextVB of
                             InCheckBoard newVBoard -> do
                                 let lmrCtx = ctx { scNodeKind = NonPV, scCheckState = InCheck, scPly = scPly ctx + 1, scNullMoveState = NullMoveAllowed } :: SearchContext p
                                 s <- alphaBeta lmrCtx newVBoard tt (Just m) dLMR (-a - 1) (-a) nodes stopFlag limits
                                 return (stepScore s)
                             NotInCheckBoard newVBoard -> do
                                 let lmrCtx = ctx { scNodeKind = NonPV, scCheckState = NotInCheck, scPly = scPly ctx + 1, scNullMoveState = NullMoveAllowed } :: SearchContext p
                                 s <- alphaBeta lmrCtx newVBoard tt (Just m) dLMR (-a - 1) (-a) nodes stopFlag limits
                                 return (stepScore s)

                         if scoreLMR > a && scoreLMR < b
                         then do
                             case nextVB of
                                 InCheckBoard newVBoard -> do
                                     let researchCtx = ctx { scNodeKind = scNodeKind ctx, scCheckState = InCheck, scPly = scPly ctx + 1, scNullMoveState = NullMoveAllowed } :: SearchContext p
                                     s2 <- alphaBeta researchCtx newVBoard tt (Just m) nextDepth (-b) (-a) nodes stopFlag limits
                                     return (stepScore s2)
                                 NotInCheckBoard newVBoard -> do
                                     let researchCtx = ctx { scNodeKind = scNodeKind ctx, scCheckState = NotInCheck, scPly = scPly ctx + 1, scNullMoveState = NullMoveAllowed } :: SearchContext p
                                     s2 <- alphaBeta researchCtx newVBoard tt (Just m) nextDepth (-b) (-a) nodes stopFlag limits
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
                         updateHistoryFull ctx d m playedQuiets
                         updateCounterMove ctx lastMove m
                     else return ()
                     return (score, TTLower, m, True, newAlpha, nextPlayedQuiets, index + 1)
                else searchStage dcBitboard lms (index + 1) inCheck staticEval newAlpha b d newFlag newBestScore newBestM True nextPlayedQuiets
