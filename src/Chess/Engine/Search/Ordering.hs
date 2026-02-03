{-# LANGUAGE BangPatterns #-}
module Chess.Engine.Search.Ordering where

import Data.List (sortOn, partition)
import qualified Data.Vector.Unboxed.Mutable as UM

import Chess.Types
import Chess.Board (ValidatedBoard, LegalMove, GenMove(..), getBoard, pieces, getGenMove)
import Chess.Engine.SEE (see)
import Chess.Engine.Search.Types (SearchContext(..))

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

updateKillers :: SearchContext -> Depth -> Move -> IO ()
updateKillers ctx depth m = do
    let ply = ctxMaxDepth ctx - unDepth depth
    if ply >= 0 && ply < ctxMaxDepth ctx then do
        let k1Idx = ply * 2
        let k2Idx = ply * 2 + 1
        k1 <- UM.unsafeRead (ctxKillers ctx) k1Idx
        if m /= k1 then do
            UM.unsafeWrite (ctxKillers ctx) k2Idx k1
            UM.unsafeWrite (ctxKillers ctx) k1Idx m
        else return ()
    else return ()

getKillers :: SearchContext -> Depth -> IO [Move]
getKillers ctx depth = do
    let ply = ctxMaxDepth ctx - unDepth depth
    if ply >= 0 && ply < ctxMaxDepth ctx then do
        let k1Idx = ply * 2
        let k2Idx = ply * 2 + 1
        k1 <- UM.unsafeRead (ctxKillers ctx) k1Idx
        k2 <- UM.unsafeRead (ctxKillers ctx) k2Idx
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
            UM.unsafeWrite (ctxCounterMove ctx) idx m
        else return ()

getCounterMove :: SearchContext -> Maybe Move -> IO (Maybe Move)
getCounterMove _ Nothing = return Nothing
getCounterMove ctx (Just prevM) = do
    if isNullMove prevM then return Nothing else do
        let idx = moveToIndex prevM
        if idx >= 0 && idx < 4096 then do
            m <- UM.unsafeRead (ctxCounterMove ctx) idx
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
