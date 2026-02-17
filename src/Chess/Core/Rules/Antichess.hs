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

module Chess.Core.Rules.Antichess where

import Chess.Core.Rules.Class
import Chess.Core.Rules.Common
import Chess.Core.Board.Internal
import Chess.Core.Game.Internal
import Chess.Core.Move.Internal

import qualified Chess.Types as T
import qualified Chess.Board.Base as Base
import qualified Chess.Board.GameState as GS
import qualified Chess.Board.MoveGen as MG
import Data.Bits (popCount)

-- | Initial Game State for Antichess (Standard setup but no castling)
antichessInitialGame :: Game 'Antichess 'Active
antichessInitialGame =
  let b = initialBoard
      ag = ActiveGame
           { internalBoard = toBaseBoard b
           , gameState = GS.setCastlingRights GS.initialGameState 0
           , variantState = ()
           , checkStatus = SSafe
           } :: ActiveGame 'Antichess 'White 'Safe
  in InProgressGame ag

instance ChessVariant 'Antichess where
  generateMoves (ag :: ActiveGame 'Antichess c s) =
    let baseBoard = internalBoard ag
        gs = toGameState ag

        -- Generate all pseudo-legal moves.
        -- In Antichess, King safety is ignored, so pseudo-legal moves are effectively legal.
        pseudos = MG.pseudoLegalMovesList baseBoard gs

        -- 1. Filter out Castling moves (Standard MG might generate them if rights exist)
        isCastling (MG.GenCastling _ _) = True
        isCastling _ = False

        pseudosNoCastling = filter (not . isCastling) pseudos

        -- 2. Add King Promotion moves
        -- Standard MG only generates Q, R, B, N promotions.
        -- We duplicate Queen promotions as King promotions.
        addKingPromos [] = []
        addKingPromos (gm : rest) =
            case gm of
              MG.GenPromotion f t T.Queen ->
                  gm : MG.GenPromotion f t T.King : addKingPromos rest
              MG.GenPromotionCapture f t T.Queen c ->
                  gm : MG.GenPromotionCapture f t T.King c : addKingPromos rest
              _ -> gm : addKingPromos rest

        pseudosEnhanced = addKingPromos pseudosNoCastling

        -- 3. Filter Captures (Compulsory)
        isCapture :: MG.GenMove -> Bool
        isCapture gm = case gm of
            MG.GenCapture {} -> True
            MG.GenPromotionCapture {} -> True
            MG.GenEnPassant {} -> True
            _ -> False

        captures = filter isCapture pseudosEnhanced

        validMoves = if not (null captures) then captures else pseudosEnhanced

    in map toCoreMove validMoves

  applyMove = genericApplyMove

  executeMove (m :: Move c) (ag :: ActiveGame 'Antichess c s) =
    case applyMove m ag of
      Transition nextAg ->
        let
            c = colorVal @c
            oppC = colorVal @(Opposite c)
            baseBoard = internalBoard nextAg

            -- 1. I win if I have no pieces left
            myPiecesBB = Base.occupiedBy baseBoard (toColor c)
            iWin = popCount myPiecesBB == 0

            -- 2. Opponent wins if they are stalemated (no legal moves)
            -- We can't use Val.hasLegalMoves because it assumes Standard rules (checks).
            -- We need to replicate Antichess move generation logic for opponent.

            oppPseudos = MG.pseudoLegalMovesList baseBoard (toGameState nextAg)
            oppHasMoves = not (null oppPseudos)

            opponentStalemated = not oppHasMoves

        in if iWin
           then Checkmate (Winner c)
           else if opponentStalemated
                then Checkmate (Winner oppC)
                else Continue (setStatus SSafe nextAg)
