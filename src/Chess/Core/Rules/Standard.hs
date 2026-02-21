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
                 -- Actually, MG.generateEvasions returns all legal moves when in check.
                 U.toList $ MG.generateEvasions baseBoard gs
             _ -> U.toList $ MG.legalGenMovesNotInCheck baseBoard gs

    in map toCoreMove baseMoves

  applyMove = genericApplyMove
  executeMove = genericExecuteMove

  -- Optimization: Use fast MoveGen directly for perft
  perftVariant depth ag =
      let b = internalBoard ag
          gs = toGameState ag
          board = FastBoard.Board b gs []
          inCheck = case checkStatus ag of
            SChecked -> True
            _ -> False
      in fastPerft depth board inCheck

fastPerft :: Int -> FastBoard.Board -> Bool -> Int
fastPerft 0 _ _ = 1
fastPerft 1 b inCheck =
    if inCheck
    then U.length (MG.generateEvasions (FastBoard.pieces b) (FastBoard.state b))
    else U.length (MG.legalGenMovesNotInCheck (FastBoard.pieces b) (FastBoard.state b))
fastPerft d b inCheck =
    let pieces = FastBoard.pieces b
        st = FastBoard.state b
        moves = if inCheck
                then U.toList (MG.generateEvasions pieces st) -- Already legal
                else
                    let pinned = MG.pinnedBits pieces st
                        pseudos = U.toList (MG.pseudoLegalMoves pieces st)
                    in filter (MG.isLegalSafe pieces st pinned) pseudos
    in sum $ map (\m ->
        let b' = applyGenMoveFast b m
            givesCheck = MG.givesCheck pieces st m
        in fastPerft (d - 1) b' givesCheck
       ) moves
