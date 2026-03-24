{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Chess.Core.Rules.Standard where

import Chess.Core.Rules.Class
import Chess.Core.Rules.Common
import Chess.Core.Board.Internal
import Chess.Core.Game.Internal

import qualified Chess.Types as T
import qualified Chess.Board.GameState as GS
import qualified Chess.Board.MoveGen as MG
import qualified Chess.Board.Validation as Val
import qualified Chess.Board.Fen as Fen
import qualified Chess.Board as FastBoard
import Chess.Board (legalGenMovesVector, legalGenMovesSafeVector, countLegalGenMoves, countLegalGenMovesSafe, applyGenMoveFast) -- Import fast board ops
import Control.Parallel.Strategies (parMap, rseq)
import qualified Data.Vector.Unboxed as U

-- | Create the initial game state for Standard chess.
initialGame :: Game 'Standard 'Active
initialGame =
  let b = initialBoard
      ag = ActiveGame
           { internalBoard = toBaseBoard b
           , gameState = GS.initialGameState
           , variantState = ()
           , checkStatus = SSafe
           } :: ActiveGame 'Standard 'White 'Safe
  in InProgressGame ag

-- | Create a game from FEN string (Standard variant).
gameFromFEN :: String -> Maybe (Game 'Standard 'Active)
gameFromFEN s = do
  (baseBoard, gs) <- Fen.parseFen s

  let c = case GS.turn gs of
            T.White -> White
            T.Black -> Black

      checked = Val.isCheck baseBoard gs
      hasMoves = Val.hasLegalMoves baseBoard gs

  if hasMoves
    then case c of
      White -> if checked
               then return $ InProgressGame (ActiveGame baseBoard gs () SChecked :: ActiveGame 'Standard 'White 'Checked)
               else return $ InProgressGame (ActiveGame baseBoard gs () SSafe    :: ActiveGame 'Standard 'White 'Safe)
      Black -> if checked
               then return $ InProgressGame (ActiveGame baseBoard gs () SChecked :: ActiveGame 'Standard 'Black 'Checked)
               else return $ InProgressGame (ActiveGame baseBoard gs () SSafe    :: ActiveGame 'Standard 'Black 'Safe)
    else Nothing

instance ChessVariant 'Standard where
  generateMoves (ag :: ActiveGame 'Standard c s) =
    let baseBoard = internalBoard ag
        gs = toGameState ag

        -- Optimization: dispatch based on check status
        baseMoves = case checkStatus ag of
             SChecked ->
                 -- If in check, castling is illegal. Construct pseudo-legal moves excluding castling.
                 let pseudos = concat
                        [ MG.pawnMovesList baseBoard gs
                        , MG.pieceMovesList baseBoard gs T.Knight
                        , MG.pieceMovesList baseBoard gs T.Bishop
                        , MG.pieceMovesList baseBoard gs T.Rook
                        , MG.pieceMovesList baseBoard gs T.Queen
                        , MG.pieceMovesList baseBoard gs T.King
                        ]
                 in filter (MG.isLegal baseBoard gs) pseudos
             _ -> MG.legalGenMovesList baseBoard gs

    in map toCoreMove baseMoves

  countMoves (ag :: ActiveGame 'Standard c s) =
    let baseBoard = internalBoard ag
        gs = toGameState ag
    in case checkStatus ag of
         SChecked ->
             -- If in check, count explicitly. It relies on the length of pseudos which is unboxed in counting builder
             length (generateMoves ag)
         _ -> MG.countLegalGenMovesSafe baseBoard gs

  applyMove = genericApplyMove
  executeMove = genericExecuteMove
  perftExecuteMove = genericPerftExecuteMove

  -- Optimization: Use fast MoveGen directly for perft
  perftVariant depth ag =
      let b = internalBoard ag
          gs = toGameState ag
          board = FastBoard.Board b gs []
          isCh = case checkStatus ag of
                   SChecked -> True
                   SSafe -> False
                   SUnchecked -> Val.isCheck b gs
      in fastPerft depth isCh board

fastPerft :: Int -> Bool -> FastBoard.Board -> Int
fastPerft 0 _ _ = 1
fastPerft 1 isCh b =
    if isCh then countLegalGenMoves b else countLegalGenMovesSafe b
fastPerft d isCh b =
    let moves = if isCh then legalGenMovesVector b else legalGenMovesSafeVector b
        evalMove m =
            let nextIsCheck = MG.givesCheck (FastBoard.pieces b) (FastBoard.state b) m
            in fastPerft (d - 1) nextIsCheck (applyGenMoveFast b m)
    in if d >= 3
       then sum $ parMap rseq evalMove (U.toList moves)
       else U.foldl' (\acc m -> acc + evalMove m) 0 moves
