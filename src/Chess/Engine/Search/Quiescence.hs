{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

module Chess.Engine.Search.Quiescence where

import Data.IORef (IORef, modifyIORef')
import Chess.Types (Depth(..), unDepth, decDepth, depthZero, CheckStatus(..))
import Chess.Board (ValidatedBoard, SomeValidatedBoard(..), getBoard, state, pieces, applyLegalMove, isCheck, captureMovesValidated, legalPromotionsValidated, legalQuietsValidated, legalMovesValidated, getGenMove, MoveGenerator(..))
import qualified Chess.Board
import qualified Chess.Board.MoveGen as MoveGen
import qualified Chess.Board.GameState as GS
import Chess.Engine.Evaluation (Evaluate(..), evaluatePos)
import Chess.Board.Phase (Position(..))
import Chess.Engine.TT (TT, probeTT, storeTT, TTFlag(..))
import Chess.Engine.Search.Types (mateValue, SearchContext(..))
import Chess.Engine.Search.Ordering (orderGenMoves, orderQSMoves)
import Chess.Types (Move, nullMove)

-- | Quiescence Search.
quiescence :: forall p s. (Evaluate p, MoveGenerator s) => SearchContext p -> ValidatedBoard s -> TT -> Int -> Int -> IORef Int -> Depth -> IO Int
quiescence ctx vBoard tt alpha beta nodes depth = do
    modifyIORef' nodes (+1)
    let board = getBoard vBoard

    -- Check for cached evaluation in TT
    let hash = GS.zobristHash (state board)
    ttEntry <- probeTT tt hash

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
            -- Search evasions. No stand-pat logic.
            go sortedMoves alpha
    else do
        -- Not in check: Standard QSearch
        -- Use cached eval if available
        let staticEval = case ttEntry of
                Just (_, s, _, TTEval) -> Just s
                _ -> Nothing

        standPat <- case staticEval of
            Just s -> return s
            Nothing -> do
                let s = evaluatePos (Position vBoard :: Position p s)
                -- Store static eval in TT
                storeTT tt hash depthZero s TTEval nullMove
                return s

        if standPat >= beta
        then return beta
        else do
            let a = max alpha standPat
            let caps = pseudoCapturesValidated vBoard
            let proms = pseudoPromotionsValidated vBoard

            -- Quiet Checks (Extension +1 ply equivalent logic)
            -- Only generate if depth > -1
            quietChecks <- if unDepth depth > -1
                           then do
                               let quiets = pseudoQuietsValidated vBoard
                               -- We must filter pseudo-quiets to ensure they actually give check.
                               -- givesCheck checks if the move is a check.
                               -- Note: We do NOT check full legality here for performance, relying on the loop check.
                               return $ filter (givesCheckLocal vBoard) quiets
                           else return []

            let sortedMoves = orderQSMoves vBoard caps proms quietChecks

            go sortedMoves a
  where
    givesCheckLocal vb lm =
        let b = getBoard vb
        in MoveGen.givesCheck (pieces b) (state b) (getGenMove lm)

    go [] a = return a
    go (lm:lms) a = do
        let gm = getGenMove lm
        let b = getBoard vBoard
        if not (MoveGen.isLegal (pieces b) (state b) gm)
        then go lms a
        else do
            score <- case applyLegalMove vBoard lm of
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
            else go lms (max a score)
