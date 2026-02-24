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
import Chess.Core.Move.Internal

import qualified Chess.Types as T
import qualified Chess.Board.Base as Base
import qualified Chess.Board.GameState as GS
import qualified Chess.Board.MoveGen as MG
import qualified Chess.Board.Validation as Val
import qualified Chess.Bitboard as BB
import qualified Chess.Board.Fen as Fen
import qualified Chess.Board as FastBoard
import Chess.Board (legalGenMoves, applyGenMoveFast, pseudoLegalGenMoves, isKingSafe, pattern GenCastling) -- Import fast board ops
import Data.Bits (testBit, countTrailingZeros, (.|.))
import Control.Parallel.Strategies (parMap, rseq)

-- | Create the initial game state for Standard chess.
initialGame :: Game 'Standard 'Active
initialGame =
  let b = initialBoard
      base = toBaseBoard b
      -- Set standard initial state (White, all castling rights)
      baseWithState = base { Base.statePacked = GS.initialStatePacked, Base.stateZobrist = 0 }
      ag = ActiveGame
           { internalBoard = baseWithState
           , variantState = ()
           , checkStatus = SSafe
           } :: ActiveGame 'Standard 'White 'Safe
  in InProgressGame ag

-- | Create a game from FEN string (Standard variant).
gameFromFEN :: String -> Maybe (Game 'Standard 'Active)
gameFromFEN s = do
  baseBoard <- Fen.parseFen s

  let c = case GS.getTurn (Base.statePacked baseBoard) of
            T.White -> White
            T.Black -> Black

      checked = Val.isCheck baseBoard
      hasMoves = Val.hasLegalMoves baseBoard

  if hasMoves
    then case c of
      White -> if checked
               then return $ InProgressGame (ActiveGame baseBoard () SChecked :: ActiveGame 'Standard 'White 'Checked)
               else return $ InProgressGame (ActiveGame baseBoard () SSafe    :: ActiveGame 'Standard 'White 'Safe)
      Black -> if checked
               then return $ InProgressGame (ActiveGame baseBoard () SChecked :: ActiveGame 'Standard 'Black 'Checked)
               else return $ InProgressGame (ActiveGame baseBoard () SSafe    :: ActiveGame 'Standard 'Black 'Safe)
    else Nothing

instance ChessVariant 'Standard where
  generateMoves (ag :: ActiveGame 'Standard c s) =
    let baseBoard = internalBoard ag

        -- Optimization: dispatch based on check status
        baseMoves = case checkStatus ag of
             SChecked ->
                 -- If in check, castling is illegal. Construct pseudo-legal moves excluding castling.
                 let pseudos = concat
                        [ MG.pawnMovesList baseBoard
                        , MG.pieceMovesList baseBoard T.Knight
                        , MG.pieceMovesList baseBoard T.Bishop
                        , MG.pieceMovesList baseBoard T.Rook
                        , MG.pieceMovesList baseBoard T.Queen
                        , MG.pieceMovesList baseBoard T.King
                        ]
                 in filter (MG.isLegal baseBoard) pseudos
             _ -> MG.legalGenMovesList baseBoard

    in map toCoreMove baseMoves

  applyMove = genericApplyMove
  executeMove = genericExecuteMove

  -- Optimization: Use fast MoveGen directly for perft
  perftVariant depth ag =
      let b = internalBoard ag
          board = FastBoard.Board b []
      in fastPerft depth board

fastPerft :: Int -> FastBoard.Board -> Int
fastPerft 0 _ = 1
fastPerft 1 b = length (legalGenMoves b)
fastPerft d b =
    let moves = pseudoLegalGenMoves b
        c = GS.getTurn (Base.statePacked (FastBoard.pieces b))
        evalMove m =
            case m of
                GenCastling _ _ ->
                    if MG.isLegal (FastBoard.pieces b) m
                    then fastPerft (d - 1) (applyGenMoveFast b m)
                    else 0
                _ ->
                    let b' = applyGenMoveFast b m
                    in if isKingSafe b' c
                       then fastPerft (d - 1) b'
                       else 0
    in if d >= 3
       then sum $ parMap rseq evalMove moves
       else sum $ map evalMove moves
