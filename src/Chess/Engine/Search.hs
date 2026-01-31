{-# LANGUAGE BangPatterns #-}
module Chess.Engine.Search (search) where

import Data.Maybe (fromMaybe, isJust)
import Data.List (sortOn, foldl', partition)
import Data.IORef (IORef, newIORef, readIORef, modifyIORef')
import Control.Concurrent (forkIO, newEmptyMVar, putMVar, takeMVar, getNumCapabilities, newMVar, modifyMVar)
import qualified Data.Vector.Mutable as VM
import qualified Data.Vector.Unboxed.Mutable as UM
import qualified Data.Vector.Unboxed as U

import Chess.Types
import Chess.Board (Board(..), applyMove, isCheck, uci, GenMove(..)
                   , ValidatedBoard, LegalMove, trustBoard, getBoard, getGenMove
                   , legalMovesValidated, captureMovesValidated, legalQuietsValidated, legalPromotionsValidated
                   , applyLegalMove, isCapture, isPromotion, toGenMove, isLegalMove, mkLegalMove)
import qualified Chess.Board.GameState as GS
import Chess.Engine.Evaluation (evaluate)
import Chess.Engine.TT (TT, probeTT, storeTT, TTFlag(..))
import Chess.Engine.SEE (see)

-- | Search Constants
infinity :: Int
infinity = 30000

mateValue :: Int
mateValue = 20000

-- | LMR Table
-- indexed by (depth * 64 + moveIndex)
lmrTable :: U.Vector Int
lmrTable = U.generate (64 * 64) gen
  where
    gen i =
        let d = i `div` 64
            idx = i `mod` 64
        in if d < 3 || idx < 2
           then 0
           else floor $ 0.75 + log (fromIntegral d) * log (fromIntegral idx) / 2.25

-- | Search Context
data SearchContext = SearchContext
    { ctxKillers :: !(VM.IOVector Move) -- 2 killers per ply * maxDepth
    , ctxHistory :: !(UM.IOVector Int)  -- 64*64 = 4096
    , ctxCounterMove :: !(VM.IOVector Move) -- 64*64 = 4096
    , ctxMaxDepth :: !Int
    }

-- | Search for the best move.
search :: Board -> TT -> Int -> IO Move
search board tt maxDepthInt = do
    let maxDepth = mkDepth maxDepthInt
    nodes <- newIORef 0

    -- Initialize Search Context
    -- Killers: 2 per ply. Let's assume max depth 128. Size = 128 * 2 = 256.
    killers <- VM.replicate 256 nullMove
    -- History: 64 * 64 = 4096.
    historyVec <- UM.replicate 4096 0
    -- Counter Moves: 64 * 64 = 4096.
    counterMove <- VM.replicate 4096 nullMove
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
    let moves = legalMovesValidated vBoard
    let board = getBoard vBoard
    -- Probe TT for root move ordering (optional but good)
    let hash = GS.zobristHash (state board)
    ttEntry <- probeTT tt hash
    let ttMove = case ttEntry of Just (m, _, _, _) -> Just m; Nothing -> Nothing

    let sortedMoves = orderGenMoves vBoard moves ttMove

    case sortedMoves of
        [] -> return (nullMove, 0) -- Should not happen if game not over
        (lm:lms) -> do
            let gm = getGenMove lm
            score <- alphaBeta ctx (applyLegalMove vBoard lm) tt (Just (getMove gm)) (decDepth depth) (-infinity) infinity True nodes
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
                                            s <- alphaBeta ctx (applyLegalMove vBoard lmWorker) tt (Just (getMove gmWorker)) (decDepth depth) (-infinity) (-bestScore) True localNodes
                                            let searchScore = -s

                                            let newBestRes = case bestRes of
                                                    Nothing -> Just (getMove gmWorker, searchScore)
                                                    Just (_, bs) -> if searchScore > bs then Just (getMove gmWorker, searchScore) else bestRes

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
        -- PVS: Search with null window first if we found a good move
        let newAlpha = max alpha bestScore
        s <- alphaBeta ctx (applyLegalMove vBoard lm) tt (Just (getMove gm)) (decDepth depth) (-beta) (-newAlpha) True nodes
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
alphaBeta :: SearchContext -> ValidatedBoard -> TT -> Maybe Move -> Depth -> Int -> Int -> Bool -> IORef Int -> IO Int
alphaBeta ctx vBoard tt lastMove depth alpha beta canNull nodes = do
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
        -- Check Extension
        let extension = if inCheck then depthOne else depthZero
        -- Depth for recursive calls. Effectively depth-1+extension.
        let nextDepth = (decDepth depth) `plusDepth` extension

        -- Checkmate/Stalemate detection if no moves (handled later) or depth <= 0
        if isZeroDepth depth
        then quiescence vBoard tt alpha beta nodes
        else do
            -- 2. Futility Pruning
            let staticEval = evaluate vBoard
            let fpMargin = 100 * (unDepth depth)
            let doFutility = depth < mkDepth 7 && not inCheck && abs beta < mateValue && staticEval + fpMargin <= alpha

            if doFutility
            then return alpha
            else do
                -- 3. Null Move Pruning
                -- R=2 if depth > 6 else R=2? Usually R=2.
                let r = if depth > mkDepth 6 then mkDepth 3 else mkDepth 2
                let doNmp = canNull && not inCheck && depth >= r && beta < mateValue

                nmpResult <- if doNmp
                             then do
                                 let nullB = Chess.Board.applyMove board NullMove
                                 let nullVB = trustBoard nullB -- Assuming null move is safe if inCheck is false
                                 -- depth - 1 - r
                                 let d' = depth `minusDepth` depthOne `minusDepth` r
                                 score <- alphaBeta ctx nullVB tt Nothing d' (-beta) (-beta + 1) False nodes
                                 return (if -score >= beta then Just beta else Nothing)
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

                                            s <- alphaBeta ctx newVBoard tt (Just ttM) nextDepth (-beta) (-alpha) True nodes
                                            let score = -s

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
                            let captures = captureMovesValidated vBoard
                            let (goodCaps, badCaps) = partitionSEE vBoard captures
                            let sortedGood = orderGenMoves vBoard (filterTT goodCaps) sortingTT

                            (score1, flag1, bestM1, found1, alpha1) <- searchStage sortedGood (0 :: Int) inCheck alpha0 beta depth flag0 score0 bestM0 found0

                            if score1 >= beta
                            then storeAndReturn score1 bestM1 TTLower
                            else do
                                -- Stage 2: Promotions (Quiet only)
                                let promotions = filter (not . isCapture) (legalPromotionsValidated vBoard)
                                let sortedPromotions = orderGenMoves vBoard (filterTT promotions) sortingTT

                                (score2, flag2, bestM2, found2, alpha2) <- searchStage sortedPromotions (0 :: Int) inCheck alpha1 beta depth flag1 score1 bestM1 found1

                                if score2 >= beta
                                then storeAndReturn score2 bestM2 TTLower
                                else do
                                    -- Stage 3: Quiets
                                    let quiets = legalQuietsValidated vBoard
                                    killers <- getKillers ctx depth
                                    counterMove <- getCounterMove ctx lastMove
                                    sortedQuiets <- orderQuiets ctx (filterTT quiets) killers counterMove sortingTT

                                    (score3, flag3, bestM3, found3, alpha3) <- searchStage sortedQuiets (0 :: Int) inCheck alpha2 beta depth flag2 score2 bestM2 found2

                                    if score3 >= beta
                                    then storeAndReturn score3 bestM3 flag3
                                    else do
                                        -- Stage 4: Captures (Bad)
                                        let sortedBad = orderGenMoves vBoard (filterTT badCaps) sortingTT
                                        (score4, flag4, bestM4, found4, _) <- searchStage sortedBad (0 :: Int) inCheck alpha3 beta depth flag3 score3 bestM3 found3

                                        if not found4 -- No moves found (Checkmate or Stalemate)
                                        then return $ if inCheck then -mateValue + (100 - unDepth depth) else 0
                                        else storeAndReturn score4 bestM4 flag4

  where
    storeAndReturn s m f = do
        let board = getBoard vBoard
        let hash = GS.zobristHash (state board)
        storeTT tt hash depth s f m
        return s

    searchStage [] _ _ a _ _ flag bestScore bestM found = return (bestScore, flag, bestM, found, a)
    searchStage (lm:lms) !index inCheck a b d flag bestScore bestM _ = do
        let isCap = Chess.Board.isCapture lm
        let isProm = Chess.Board.isPromotion lm
        let isQuiet = not isCap && not isProm
        let dVal = unDepth d

        -- Late Move Pruning
        let lmpCount = 3 + dVal * dVal
        if not inCheck && dVal < 8 && isQuiet && index > lmpCount
        then return (bestScore, flag, bestM, True, a)
        else do
            let gm = getGenMove lm
            let m = getMove gm

            let newVBoard = applyLegalMove vBoard lm

            -- Calculate check extension based on the NEW board state (after move)
            let givesCheck = isCheck (getBoard newVBoard)
            let extension = if givesCheck then depthOne else depthZero
            let nextDepth = (decDepth d) `plusDepth` extension

            -- PVS
            score <- if bestScore == -infinity -- First move
                     then do
                         s <- alphaBeta ctx newVBoard tt (Just m) nextDepth (-b) (-a) True nodes
                         return (-s)
                     else do
                         -- Late Move Reduction?
                         -- If quiet, depth > 2, not checking, etc.
                         -- Do not reduce moves that give check!
                         let lmr = if d >= mkDepth 3 && not isCap && not isProm && index >= 2 && not inCheck && not givesCheck
                                   then
                                       let dIdx = min 63 (unDepth d)
                                           mIdx = min 63 index
                                       in mkDepth (lmrTable U.! (dIdx * 64 + mIdx))
                                   else depthZero

                         let dLMR = nextDepth `minusDepth` lmr

                         s <- alphaBeta ctx newVBoard tt (Just m) dLMR (-a - 1) (-a) True nodes -- Null window
                         if s > a && s < b
                         then do
                             -- Re-search with full window
                             s2 <- alphaBeta ctx newVBoard tt (Just m) nextDepth (-b) (-a) True nodes
                             return (-s2)
                         else return (-s)

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
            else searchStage lms (index + 1) inCheck newAlpha b d newFlag newBestScore newBestM True

    getMove (GenQuiet f t _) = Move f t Nothing
    getMove (GenCapture f t _ _) = Move f t Nothing
    getMove (GenEnPassant f t) = Move f t Nothing
    getMove (GenCastling f t) = Move f t Nothing
    getMove (GenPromotion f t p) = Move f t (Just p)
    getMove (GenPromotionCapture f t p _) = Move f t (Just p)

updateKillers :: SearchContext -> Depth -> Move -> IO ()
updateKillers ctx depth m = do
    let ply = ctxMaxDepth ctx - unDepth depth
    if ply >= 0 && ply < ctxMaxDepth ctx then do
        let k1Idx = ply * 2
        let k2Idx = ply * 2 + 1
        k1 <- VM.unsafeRead (ctxKillers ctx) k1Idx
        if m /= k1 then do
            VM.unsafeWrite (ctxKillers ctx) k2Idx k1
            VM.unsafeWrite (ctxKillers ctx) k1Idx m
        else return ()
    else return ()

getKillers :: SearchContext -> Depth -> IO [Move]
getKillers ctx depth = do
    let ply = ctxMaxDepth ctx - unDepth depth
    if ply >= 0 && ply < ctxMaxDepth ctx then do
        let k1Idx = ply * 2
        let k2Idx = ply * 2 + 1
        k1 <- VM.unsafeRead (ctxKillers ctx) k1Idx
        k2 <- VM.unsafeRead (ctxKillers ctx) k2Idx
        return $ filter (/= nullMove) [k1, k2]
    else return []

updateHistory :: SearchContext -> Depth -> Move -> IO ()
updateHistory ctx depth (Move f t _) = do
    let idx = (unSquare f) * 64 + (unSquare t)
    let d = unDepth depth
    let bonus = d * d
    v <- UM.unsafeRead (ctxHistory ctx) idx
    UM.unsafeWrite (ctxHistory ctx) idx (v + bonus)
updateHistory _ _ _ = return ()

updateCounterMove :: SearchContext -> Maybe Move -> Move -> IO ()
updateCounterMove _ Nothing _ = return ()
updateCounterMove ctx (Just prevM) m = do
    if isNullMove prevM then return () else do
        let idx = moveToIndex prevM
        if idx >= 0 && idx < 4096 then
            VM.unsafeWrite (ctxCounterMove ctx) idx m
        else return ()

getCounterMove :: SearchContext -> Maybe Move -> IO (Maybe Move)
getCounterMove _ Nothing = return Nothing
getCounterMove ctx (Just prevM) = do
    if isNullMove prevM then return Nothing else do
        let idx = moveToIndex prevM
        if idx >= 0 && idx < 4096 then do
            m <- VM.unsafeRead (ctxCounterMove ctx) idx
            if isNullMove m then return Nothing else return (Just m)
        else return Nothing

moveToIndex :: Move -> Int
moveToIndex (Move f t _) = (unSquare f) * 64 + (unSquare t)
moveToIndex _ = -1

orderQuiets :: SearchContext -> [LegalMove] -> [Move] -> Maybe Move -> Maybe Move -> IO [LegalMove]
orderQuiets ctx quiets killers counterMove ttM = do
    let (kMoves, others) = partitionKillers quiets killers
    let (cmMoves, others2) = case counterMove of
            Nothing -> ([], others)
            Just cm -> partitionCounterMove others cm

    scoredOthers <- mapM (\lm -> do s <- scoreHistory ctx lm; return (lm, s)) others2
    let sortedOthers = map fst $ sortOn (negate . snd) scoredOthers

    let combined = kMoves ++ cmMoves ++ sortedOthers

    let filtered = case ttM of
            Nothing -> combined
            Just tm -> filter (\lm -> getMove (getGenMove lm) /= tm) combined
    return filtered
  where
    getMove (GenQuiet f t _) = Move f t Nothing
    getMove (GenCastling f t) = Move f t Nothing
    getMove _ = nullMove -- Should not happen for quiets

partitionCounterMove :: [LegalMove] -> Move -> ([LegalMove], [LegalMove])
partitionCounterMove lms cm = foldr part ([], []) lms
  where
    part lm (c, o) = if getMove (getGenMove lm) == cm then (lm:c, o) else (c, lm:o)
    getMove (GenQuiet f t _) = Move f t Nothing
    getMove (GenCastling f t) = Move f t Nothing
    getMove _ = nullMove

partitionKillers :: [LegalMove] -> [Move] -> ([LegalMove], [LegalMove])
partitionKillers lms ks = foldr part ([], []) lms
  where
    part lm (k, o) = if getMove (getGenMove lm) `elem` ks then (lm:k, o) else (k, lm:o)
    getMove (GenQuiet f t _) = Move f t Nothing
    getMove (GenCastling f t) = Move f t Nothing
    getMove _ = nullMove

scoreHistory :: SearchContext -> LegalMove -> IO Int
scoreHistory ctx lm = do
    let gm = getGenMove lm
    case gm of
        GenQuiet f t _ -> do
             let idx = (unSquare f) * 64 + (unSquare t)
             UM.unsafeRead (ctxHistory ctx) idx
        GenCastling _ _ -> return 0
        _ -> return 0

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
            Just (_, s, _, TTEval) -> Just s
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
        let caps = captureMovesValidated vBoard
        let proms = legalPromotionsValidated vBoard
        let allMoves = caps ++ proms
        let sortedMoves = orderGenMoves vBoard allMoves Nothing

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
orderGenMoves :: ValidatedBoard -> [LegalMove] -> Maybe Move -> [LegalMove]
orderGenMoves vBoard moves ttM =
    let board = pieces (getBoard vBoard)
        (ttMoves, rest) = case ttM of
            Nothing -> ([], moves)
            Just tm -> foldr (\lm (t, o) -> if getMove (getGenMove lm) == tm then (lm:t, o) else (t, lm:o)) ([], []) moves

        (capProms, capsAll, proms, quiets) = partitionMoves rest
        (goodCaps, badCaps) = partition (\lm -> see board (getMove (getGenMove lm)) >= 0) capsAll

        sortDesc = sortOn (negate . scoreMove . getGenMove)
    in ttMoves ++ sortDesc capProms ++ sortDesc goodCaps ++ sortDesc proms ++ quiets ++ sortDesc badCaps
  where
    getMove (GenQuiet f t _) = Move f t Nothing
    getMove (GenCapture f t _ _) = Move f t Nothing
    getMove (GenEnPassant f t) = Move f t Nothing
    getMove (GenCastling f t) = Move f t Nothing
    getMove (GenPromotion f t p) = Move f t (Just p)
    getMove (GenPromotionCapture f t p _) = Move f t (Just p)

scoreMove :: GenMove -> Int
scoreMove (GenCapture _ _ pt capPt) = 1000 + (pieceValue capPt * 10) - (pieceValue pt)
scoreMove (GenPromotionCapture _ _ promPt capPt) = 1000 + (pieceValue capPt * 10) - (pieceValue Pawn) + (pieceValue promPt)
scoreMove (GenPromotion _ _ promPt) = 900 + (pieceValue promPt)
scoreMove (GenEnPassant _ _) = 1000 + (pieceValue Pawn * 10) - (pieceValue Pawn)
scoreMove _ = 0

pieceValue :: PieceType -> Int
pieceValue Pawn = 1
pieceValue Knight = 3
pieceValue Bishop = 3
pieceValue Rook = 5
pieceValue Queen = 9
pieceValue King = 100

partitionSEE :: ValidatedBoard -> [LegalMove] -> ([LegalMove], [LegalMove])
partitionSEE vb moves = partition isGood moves
  where
    b = pieces (getBoard vb)
    isGood lm = case getGenMove lm of
        GenPromotionCapture {} -> True
        GenEnPassant {} -> True
        GenCapture {} -> see b (getMove (getGenMove lm)) >= 0
        _ -> True
    getMove (GenQuiet f t _) = Move f t Nothing
    getMove (GenCapture f t _ _) = Move f t Nothing
    getMove (GenEnPassant f t) = Move f t Nothing
    getMove (GenCastling f t) = Move f t Nothing
    getMove (GenPromotion f t p) = Move f t (Just p)
    getMove (GenPromotionCapture f t p _) = Move f t (Just p)

partitionMoves :: [LegalMove] -> ([LegalMove], [LegalMove], [LegalMove], [LegalMove])
partitionMoves moves = foldr part ([], [], [], []) moves
  where
    part lm (cp, c, p, q) = case getGenMove lm of
        GenPromotionCapture {} -> (lm:cp, c, p, q)
        GenCapture {} -> (cp, lm:c, p, q)
        GenEnPassant {} -> (cp, lm:c, p, q)
        GenPromotion {} -> (cp, c, lm:p, q)
        GenQuiet {} -> (cp, c, p, lm:q)
        GenCastling {} -> (cp, c, p, lm:q)
