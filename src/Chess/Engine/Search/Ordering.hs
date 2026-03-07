{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Chess.Engine.Search.Ordering where

import Control.Monad (forM_)

import Data.List (sortOn, partition)
import Data.Ord (Down(..))
import qualified Data.Vector.Unboxed.Mutable as UM

import Chess.Types
import Chess.Board (Board(..), ValidatedBoard, LegalMove, GenMove, genMoveToMove, pattern GenQuiet, pattern GenCapture, pattern GenEnPassant, pattern GenCastling, pattern GenPromotion, pattern GenPromotionCapture, pattern GenCastling960, pattern GenDrop, getBoard, pieces, getGenMove)
import qualified Chess.Board.GameState as GS
import Chess.Engine.SEE (see, seeGen)
import Chess.Engine.Search.Types (SearchContext(..), SearchResources(..))

-- | Move Ordering
-- Optimized to use a single sort pass with a comprehensive scoring function.
orderGenMoves :: ValidatedBoard s -> [LegalMove] -> Maybe Move -> [LegalMove]
orderGenMoves vBoard moves ttM = sortOn (Down . orderScore) moves
  where
    (Board b gs _) = getBoard vBoard
    turn = GS.turn gs

    orderScore lm =
        let gm = getGenMove lm
        in if isTTMove gm
           then 2000000
           else scoreGenMove gm

    isTTMove gm = case ttM of
        Nothing -> False
        Just tm -> genMoveToMove gm == tm

    scoreGenMove gm = case gm of
        GenPromotionCapture {} -> 100000 + scoreMove gm
        GenCapture _ _ pt capPt ->
            if pieceValue capPt >= pieceValue pt || seeGen b turn gm >= 0
            then 50000 + scoreMove gm
            else -10000 + scoreMove gm
        GenEnPassant {} -> 50000 + scoreMove gm
        GenPromotion {} -> 30000 + scoreMove gm
        _ -> 0

-- | Specialized move ordering for Quiescence Search.
-- Avoids concatenating lists and re-partitioning.
-- Returns moves with known check status (True if known to give check, Nothing if unknown).
orderQSMoves :: ValidatedBoard s -> [LegalMove] -> [LegalMove] -> [LegalMove] -> [(LegalMove, Maybe Bool)]
orderQSMoves vBoard caps proms quietChecks =
    let (Board b gs _) = getBoard vBoard
        turn = GS.turn gs

        -- Process caps: Split into PromoCaps, GoodCaps, BadCaps
        (promoCaps, goodCaps, badCaps) = foldr processCap ([], [], []) caps

        processCap lm (pc, gc, bc) = case getGenMove lm of
            GenPromotionCapture {} -> (lm:pc, gc, bc)
            GenCapture _ _ pt capPt ->
                if pieceValue capPt >= pieceValue pt || seeGen b turn (getGenMove lm) >= 0
                then (pc, lm:gc, bc)
                else (pc, gc, lm:bc)
            GenEnPassant {} -> (pc, lm:gc, bc) -- En Passant is generally good
            _ -> (pc, gc, bc) -- Should not happen for caps list

        sortDesc = sortOn (negate . scoreMove . getGenMove)

        tagUnknown ms = map (\m -> (m, Nothing)) ms
        tagCheck ms = map (\m -> (m, Just True)) ms

        -- Order: PromoCaps > GoodCaps > Proms > QuietChecks > BadCaps
    in tagUnknown (sortDesc promoCaps) ++ tagUnknown (sortDesc goodCaps) ++ tagUnknown (sortDesc proms) ++ tagCheck quietChecks ++ tagCheck (sortDesc badCaps)

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
partitionSEE vb moves = partition isGood moves
  where
    (Board b gs _) = getBoard vb
    turn = GS.turn gs
    isGood lm = case getGenMove lm of
        GenPromotionCapture {} -> True
        GenEnPassant {} -> True
        GenCapture _ _ pt capPt -> pieceValue capPt >= pieceValue pt || seeGen b turn (getGenMove lm) >= 0
        _ -> True

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

updateHistory :: forall p. SearchContext p -> Depth -> Move -> IO ()
updateHistory ctx depth (Move f t _) = do
    let res = scResources ctx
    let idx = (unSquare f) * 64 + (unSquare t)
    let d = unDepth depth
    let bonus = min 400 (d * d)
    v <- UM.unsafeRead (resHistory res) idx
    let scaledV = v - v * bonus `div` 16384
    UM.unsafeWrite (resHistory res) idx (scaledV + bonus)
updateHistory _ _ _ = return ()

penaltyHistory :: forall p. SearchContext p -> Depth -> Move -> IO ()
penaltyHistory ctx depth (Move f t _) = do
    let res = scResources ctx
    let idx = (unSquare f) * 64 + (unSquare t)
    let d = unDepth depth
    let penalty = min 400 (d * d)
    v <- UM.unsafeRead (resHistory res) idx
    let scaledV = v - v * penalty `div` 16384
    UM.unsafeWrite (resHistory res) idx (scaledV - penalty)
penaltyHistory _ _ _ = return ()

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
    let sortedOthers = map fst $ sortOn (negate . snd) scoredOthers

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
    part lm (k, o) = if genMoveToMove (getGenMove lm) `elem` ks then (lm:k, o) else (k, lm:o)

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
sortMoves [] = []
sortMoves [x] = [x]
sortMoves (x:xs) =
    let (best, rest) = extractMax x (scoreMove (getGenMove x)) [] xs
    in best : sortMoves rest
  where
    extractMax curr currScore acc [] = (curr, acc)
    extractMax curr currScore acc (y:ys) =
        let yScore = scoreMove (getGenMove y)
        in if yScore > currScore
           then extractMax y yScore (curr:acc) ys
           else extractMax curr currScore (y:acc) ys

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
