{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Chess.Engine.Search.Ordering where

import Data.List (sortOn, partition)
import qualified Data.Vector.Unboxed.Mutable as UM

import Chess.Types
import Chess.Board (Board(..), ValidatedBoard, LegalMove, GenMove(..), pattern GenQuiet, pattern GenCapture, pattern GenEnPassant, pattern GenCastling, pattern GenPromotion, pattern GenPromotionCapture, getBoard, pieces, getGenMove)
import qualified Chess.Board.GameState as GS
import Chess.Engine.SEE (see, seeGen)
import Chess.Engine.Search.Types (SearchContext(..), SearchResources(..))

-- | Move Ordering
orderGenMoves :: ValidatedBoard -> [LegalMove] -> Maybe Move -> [LegalMove]
orderGenMoves vBoard moves ttM =
    let (Board b gs _) = getBoard vBoard
        turn = GS.turn gs

        (ttMoves, rest) = case ttM of
            Nothing -> ([], moves)
            Just tm -> foldr (\lm (t, o) -> if getMove (getGenMove lm) == tm then (lm:t, o) else (t, lm:o)) ([], []) moves

        (capProms, capsAll, proms, quiets) = partitionMoves rest
        (goodCaps, badCaps) = partition (\lm -> seeGen b turn (getGenMove lm) >= 0) capsAll

        sortDesc = sortOn (negate . scoreMove . getGenMove)
    in ttMoves ++ sortDesc capProms ++ sortDesc goodCaps ++ sortDesc proms ++ quiets ++ sortDesc badCaps
  where
    getMove (GenQuiet f t _) = Move f t Nothing
    getMove (GenCapture f t _ _) = Move f t Nothing
    getMove (GenEnPassant f t) = Move f t Nothing
    getMove (GenCastling f t) = Move f t Nothing
    getMove (GenPromotion f t p) = Move f t (Just p)
    getMove (GenPromotionCapture f t p _) = Move f t (Just p)

-- | Specialized move ordering for Quiescence Search.
-- Avoids concatenating lists and re-partitioning.
orderQSMoves :: ValidatedBoard -> [LegalMove] -> [LegalMove] -> [LegalMove] -> [LegalMove]
orderQSMoves vBoard caps proms quietChecks =
    let (Board b gs _) = getBoard vBoard
        turn = GS.turn gs

        -- Process caps: Split into PromoCaps, GoodCaps, BadCaps
        (promoCaps, goodCaps, badCaps) = foldr processCap ([], [], []) caps

        processCap lm (pc, gc, bc) = case getGenMove lm of
            GenPromotionCapture {} -> (lm:pc, gc, bc)
            GenCapture _ _ _ _ ->
                if seeGen b turn (getGenMove lm) >= 0
                then (pc, lm:gc, bc)
                else (pc, gc, lm:bc)
            GenEnPassant {} -> (pc, lm:gc, bc) -- En Passant is generally good
            _ -> (pc, gc, bc) -- Should not happen for caps list

        sortDesc = sortOn (negate . scoreMove . getGenMove)

        -- Order: PromoCaps > GoodCaps > Proms > QuietChecks > BadCaps
    in sortDesc promoCaps ++ sortDesc goodCaps ++ sortDesc proms ++ quietChecks ++ sortDesc badCaps

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
    (Board b gs _) = getBoard vb
    turn = GS.turn gs
    isGood lm = case getGenMove lm of
        GenPromotionCapture {} -> True
        GenEnPassant {} -> True
        GenCapture {} -> seeGen b turn (getGenMove lm) >= 0
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
    let bonus = d * d
    v <- UM.unsafeRead (resHistory res) idx
    UM.unsafeWrite (resHistory res) idx (v + bonus)
updateHistory _ _ _ = return ()

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
