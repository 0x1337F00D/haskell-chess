{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

module Chess.Engine.Search.Quiescence where

import Data.IORef (IORef, modifyIORef')
import Chess.Types (Depth(..), unDepth, decDepth, depthZero, CheckStatus(..))
import Chess.Board (ValidatedBoard, SomeValidatedBoard(..), getBoard, state, pieces, applyLegalMoveValidated, captureMovesValidated, legalPromotionsValidated, legalQuietsValidated, legalMovesValidated, getGenMove, MoveGenerator(..))
import Chess.Engine.Evaluation (Evaluate(..), evaluatePos)
import Chess.Board.Phase (Position(..))
import Chess.Engine.TT (TT, probeTTFast, storeTT, TTFlag(..), unpackData)
import Chess.Engine.Search.Types (mateValue, SearchContext(..))
import Chess.Engine.Search.Ordering (orderGenMoves, orderQSMoves, partitionSEE)
import Chess.Types (nullMove)
import qualified Chess.Board.GameState as GS
import qualified Chess.Board.MoveGen.KingSafety as KingSafety

-- | Quiescence Search.
quiescence :: forall p s. (Evaluate p, MoveGenerator s) => SearchContext p -> ValidatedBoard s -> TT -> Int -> Int -> IORef Int -> Depth -> IO Int
quiescence ctx vBoard tt alpha beta nodes depth = do
    modifyIORef' nodes (+1)
    let board = getBoard vBoard

    -- Check for cached evaluation in TT
    let hash = GS.zobristHash (state board)
    ttEntryData <- probeTTFast tt hash

    -- Use CheckState from context
    let inCheck = case scCheckState ctx of
            InCheck -> True
            NotInCheck -> False

    if inCheck
    then do
        -- If in check, we must search all evasions (legal moves)
        let evasions = legalMovesValidated vBoard
        if null evasions
        then return (-mateValue)
        else do
            let sortedMoves = orderGenMoves vBoard evasions Nothing

            -- Precalculate DC for Evasions? No, Evasions are few.
            -- But standard loop does calls to givesCheck for next recursion.
            -- So yes, calculate it.
            let dcBitboard = KingSafety.discoveryCandidates (pieces board) (GS.turn (state board))

            go dcBitboard (map (\m -> (m, Nothing)) sortedMoves) alpha
    else do
        -- Not in check: Standard QSearch
        -- Use cached eval if available
        let staticEval = if ttEntryData /= 0
                         then let (_, s, _, f, _) = unpackData ttEntryData
                              in if f == TTEval then Just s else Nothing
                         else Nothing

        standPat <- case staticEval of
            Just s -> return s
            Nothing -> do
                let s = evaluatePos (Position vBoard :: Position p s)
                -- Store static eval in TT
                storeTT tt (scAge ctx) hash depthZero s TTEval nullMove
                return s

        if standPat >= beta
        then return beta
        else do
            -- Precalculate Discovery Candidates
            let dcBitboard = KingSafety.discoveryCandidates (pieces board) (GS.turn (state board))

            let givesCheckLocal lm =
                    KingSafety.givesCheckOptimized (pieces board) (state board) dcBitboard (getGenMove lm)

            let a = max alpha standPat
            let caps = captureMovesValidated vBoard
            let (goodCaps, badCaps) = partitionSEE vBoard caps
            let proms = legalPromotionsValidated vBoard

            -- Quiet Checks (Extension +1 ply equivalent logic)
            -- Only generate if depth > -1
            quietChecks <- if unDepth depth > -1
                           then do
                               let quiets = legalQuietsValidated vBoard
                               return $ filter givesCheckLocal quiets
                           else return []

            -- Also search bad captures if they give check (tactical sacrifices)
            let checkingBadCaps = filter givesCheckLocal badCaps
            let qsMoves = goodCaps ++ checkingBadCaps

            let sortedMoves = orderQSMoves vBoard qsMoves proms quietChecks

            go dcBitboard sortedMoves a
  where
    go _ [] a = return a
    go dcBitboard ((lm, mbCheck):lms) a = do
        let givesCheck = case mbCheck of
                Just c -> c
                Nothing ->
                    let b = getBoard vBoard
                    in KingSafety.givesCheckOptimized (pieces b) (state b) dcBitboard (getGenMove lm)

        do
            score <- case applyLegalMoveValidated vBoard lm givesCheck of
                InCheckBoard newVBoard -> do
                    let newCtx = ctx { scCheckState = InCheck, scPly = scPly ctx + 1 } :: SearchContext p
                    s <- quiescence newCtx newVBoard tt (-beta) (-a) nodes (decDepth depth)
                    return (-s)
                NotInCheckBoard newVBoard -> do
                    let newCtx = ctx { scCheckState = NotInCheck, scPly = scPly ctx + 1 } :: SearchContext p
                    s <- quiescence newCtx newVBoard tt (-beta) (-a) nodes (decDepth depth)
                    return (-s)

            if score >= beta
            then return beta
            else go dcBitboard lms (max a score)
