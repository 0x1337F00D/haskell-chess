{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Chess.Engine.Search.Ordering where


import qualified Data.Vector.Unboxed.Mutable as UM

import Chess.Types
import Chess.Board (Board(..), ValidatedBoard, LegalMove, GenMove, genMoveToMove, pattern GenQuiet, pattern GenCapture, pattern GenEnPassant, pattern GenCastling, pattern GenPromotion, pattern GenPromotionCapture, pattern GenCastling960, pattern GenDrop, getBoard, getGenMove)
import qualified Chess.Board.GameState as GS
import Chess.Engine.SEE (seeGen)
import Chess.Engine.Search.Types (SearchContext(..), SearchResources(..))

-- | Move Ordering
-- Optimized to use a single sort pass with a comprehensive scoring function.
orderGenMoves :: ValidatedBoard s -> [LegalMove] -> Maybe Move -> [LegalMove]
orderGenMoves vBoard moves ttM = finish (foldr step (Nothing, [], [], [], [], []) moves)
  where
    (Board b gs _) = getBoard vBoard
    turn = GS.turn gs

    isTTMove gm = case ttM of
        Nothing -> False
        Just tm -> genMoveToMove gm == tm

    step lm (!ttHit, !promoCaps, !goodCaps, !proms, !quiets, !badCaps) =
        let gm = getGenMove lm
        in if isTTMove gm
           then (Just lm, promoCaps, goodCaps, proms, quiets, badCaps)
           else case gm of
                GenPromotionCapture {} ->
                    (ttHit, insertScoredMove (scoreMove gm) lm promoCaps, goodCaps, proms, quiets, badCaps)
                GenCapture _ _ pt capPt ->
                    if pieceValue capPt >= pieceValue pt || seeGen b turn gm >= 0
                    then (ttHit, promoCaps, insertScoredMove (scoreMove gm) lm goodCaps, proms, quiets, badCaps)
                    else (ttHit, promoCaps, goodCaps, proms, quiets, insertScoredMove (scoreMove gm) lm badCaps)
                GenEnPassant {} ->
                    (ttHit, promoCaps, insertScoredMove (scoreMove gm) lm goodCaps, proms, quiets, badCaps)
                GenPromotion {} ->
                    (ttHit, promoCaps, goodCaps, insertScoredMove (scoreMove gm) lm proms, quiets, badCaps)
                _ ->
                    (ttHit, promoCaps, goodCaps, proms, lm : quiets, badCaps)

    finish (ttHit, promoCaps, goodCaps, proms, quiets, badCaps) =
        let ordered = promoCaps ++ goodCaps ++ proms ++ quiets ++ badCaps
        in case ttHit of
            Nothing -> ordered
            Just ttMove -> ttMove : ordered

-- | Specialized move ordering for Quiescence Search.
-- Avoids concatenating lists and re-partitioning.
-- Returns moves with known check status (True if known to give check, Nothing if unknown).
orderQSMoves :: ValidatedBoard s -> [LegalMove] -> [LegalMove] -> [LegalMove] -> [(LegalMove, Maybe Bool)]
orderQSMoves vBoard caps proms quietChecks =
    let (Board b gs _) = getBoard vBoard
        turn = GS.turn gs

        (promoCaps, goodCaps, badCaps) = foldr processCap ([], [], []) caps

        processCap lm (pc, gc, bc) =
            let gm = getGenMove lm
            in case gm of
                GenPromotionCapture {} -> (insertScoredMove (scoreMove gm) lm pc, gc, bc)
                GenCapture _ _ pt capPt ->
                    if pieceValue capPt >= pieceValue pt || seeGen b turn gm >= 0
                    then (pc, insertScoredMove (scoreMove gm) lm gc, bc)
                    else (pc, gc, insertScoredMove (scoreMove gm) lm bc)
                GenEnPassant {} -> (pc, insertScoredMove (scoreMove gm) lm gc, bc)
                _ -> (pc, gc, bc)

        sortedProms = sortByMoveScore proms

        tagUnknown ms = map (\m -> (m, Nothing)) ms
        tagCheck ms = map (\m -> (m, Just True)) ms

    in tagUnknown promoCaps
        ++ tagUnknown goodCaps
        ++ tagUnknown sortedProms
        ++ tagCheck quietChecks
        ++ tagCheck badCaps

insertScoredMove :: Int -> LegalMove -> [LegalMove] -> [LegalMove]
insertScoredMove !score !move = go
  where
    go [] = [move]
    go (x:xs)
        | score >= scoreMove (getGenMove x) = move : x : xs
        | otherwise = x : go xs

sortByMoveScore :: [LegalMove] -> [LegalMove]
sortByMoveScore = foldr (\lm acc -> insertScoredMove (scoreMove (getGenMove lm)) lm acc) []

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

partitionSEE :: ValidatedBoard s -> [LegalMove] -> ([LegalMove], [LegalMove])
partitionSEE vb moves = foldr step ([], []) moves
  where
    (Board b gs _) = getBoard vb
    turn = GS.turn gs

    step lm (good, bad) = case getGenMove lm of
        GenPromotionCapture {} -> (lm:good, bad)
        GenEnPassant {} -> (lm:good, bad)
        GenCapture _ _ pt capPt ->
            if pieceValue capPt >= pieceValue pt || seeGen b turn (getGenMove lm) >= 0
            then (lm:good, bad)
            else (good, lm:bad)
        _ -> (lm:good, bad)

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
        GenCastling960 {} -> (cp, c, p, lm:q)
        GenDrop {} -> (cp, c, p, lm:q)

updateKillers :: forall p. SearchContext p -> Depth -> Move -> IO ()
updateKillers ctx depth m = do
    let res = scResources ctx
    let ply = resMaxDepth res - unDepth depth
    if ply >= 0 && ply < resMaxDepth res then do
        let k1Idx = ply * 2
        let k2Idx = ply * 2 + 1
        k1 <- UM.unsafeRead (resKillers res) k1Idx
        if m /= k1 then do
            UM.unsafeWrite (resKillers res) k2Idx k1
            UM.unsafeWrite (resKillers res) k1Idx m
        else return ()
    else return ()

getKillers :: forall p. SearchContext p -> Depth -> IO [Move]
getKillers ctx depth = do
    let res = scResources ctx
    let ply = resMaxDepth res - unDepth depth
    if ply >= 0 && ply < resMaxDepth res then do
        let k1Idx = ply * 2
        let k2Idx = ply * 2 + 1
        k1 <- UM.unsafeRead (resKillers res) k1Idx
        k2 <- UM.unsafeRead (resKillers res) k2Idx
        return $ filter (/= nullMove) [k1, k2]
    else return []

updateHistoryFull :: forall p. SearchContext p -> Depth -> Move -> [Move] -> IO ()
updateHistoryFull ctx depth bestMove playedQuiets = do
    let res = scResources ctx
    let d = unDepth depth
        modifier = min 400 (d * d)

    let updateOne delta (Move f t _) = do
          let idx = (unSquare f) * 64 + (unSquare t)
          v <- UM.unsafeRead (resHistory res) idx
          let scaledV = v - v * modifier `div` 16384
          UM.unsafeWrite (resHistory res) idx (scaledV + delta)
        updateOne _ _ = return ()

    updateOne modifier bestMove
    mapM_ (updateOne (-modifier)) playedQuiets

updateCounterMove :: forall p. SearchContext p -> Maybe Move -> Move -> IO ()
updateCounterMove _ Nothing _ = return ()
updateCounterMove ctx (Just prevM) m = do
    let res = scResources ctx
    if isNullMove prevM then return () else do
        let idx = moveToIndex prevM
        if idx >= 0 && idx < 4096 then
            UM.unsafeWrite (resCounterMove res) idx m
        else return ()

getCounterMove :: forall p. SearchContext p -> Maybe Move -> IO (Maybe Move)
getCounterMove _ Nothing = return Nothing
getCounterMove ctx (Just prevM) = do
    let res = scResources ctx
    if isNullMove prevM then return Nothing else do
        let idx = moveToIndex prevM
        if idx >= 0 && idx < 4096 then do
            m <- UM.unsafeRead (resCounterMove res) idx
            if isNullMove m then return Nothing else return (Just m)
        else return Nothing

moveToIndex :: Move -> Int
moveToIndex (Move f t _) = (unSquare f) * 64 + (unSquare t)
moveToIndex _ = -1

orderQuiets :: forall p. SearchContext p -> [LegalMove] -> [Move] -> Maybe Move -> Maybe Move -> IO [LegalMove]
orderQuiets ctx quiets killers counterMove ttM = do
    let (kMoves, others) = partitionKillers quiets killers
    let (cmMoves, others2) = case counterMove of
            Nothing -> ([], others)
            Just cm -> partitionCounterMove others cm

    scoredOthers <- mapM (\lm -> do s <- scoreHistory ctx lm; return (lm, s)) others2
    let sortedOthers = foldr (\(lm, s) acc -> insertScoredMove s lm acc) [] scoredOthers

    let combined = kMoves ++ cmMoves ++ sortedOthers

    let filtered = case ttM of
            Nothing -> combined
            Just tm -> filter (\lm -> genMoveToMove (getGenMove lm) /= tm) combined
    return filtered

partitionCounterMove :: [LegalMove] -> Move -> ([LegalMove], [LegalMove])
partitionCounterMove lms cm = foldr part ([], []) lms
  where
    part lm (c, o) = if genMoveToMove (getGenMove lm) == cm then (lm:c, o) else (c, lm:o)

partitionKillers :: [LegalMove] -> [Move] -> ([LegalMove], [LegalMove])
partitionKillers lms ks = foldr part ([], []) lms
  where
    k1 = case ks of
        m:_ -> Just m
        [] -> Nothing
    k2 = case ks of
        _:m:_ -> Just m
        _ -> Nothing

    isKiller m = maybe False (== m) k1 || maybe False (== m) k2

    part lm (k, o) =
        let m = genMoveToMove (getGenMove lm)
        in if isKiller m then (lm:k, o) else (k, lm:o)

scoreHistory :: forall p. SearchContext p -> LegalMove -> IO Int
scoreHistory ctx lm = do
    let res = scResources ctx
    let gm = getGenMove lm
    case gm of
        GenQuiet f t _ -> do
             let idx = (unSquare f) * 64 + (unSquare t)
             UM.unsafeRead (resHistory res) idx
        GenCastling _ _ -> return 0
        _ -> return 0

sortMoves :: [LegalMove] -> [LegalMove]
sortMoves moves = map snd $ foldr insertMove [] moves
  where
    insertMove m acc =
        let s = scoreMove (getGenMove m)
        in insert s m acc

    insert s m [] = [(s, m)]
    insert s m ((s', m'):xs)
        | s >= s'   = (s, m) : (s', m') : xs
        | otherwise = (s', m') : insert s m xs

-- | Sorts a list of moves by score, optionally picking a TT move to be first.
-- Does NOT re-partition or re-calculate SEE.
{-# INLINE pickAndSort #-}
pickAndSort :: [LegalMove] -> Maybe Move -> [LegalMove]
pickAndSort moves Nothing = sortMoves moves
pickAndSort moves (Just ttM) =
    let (pre, post) = break (\lm -> genMoveToMove (getGenMove lm) == ttM) moves
    in case post of
        [] -> sortMoves pre
        (tt:rest) -> tt : sortMoves (pre ++ rest)
