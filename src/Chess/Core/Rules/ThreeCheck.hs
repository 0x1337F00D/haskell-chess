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

module Chess.Core.Rules.ThreeCheck where

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

instance ChessVariant 'ThreeCheck where
  generateMoves (ag :: ActiveGame 'ThreeCheck c s) =
    let baseBoard = internalBoard ag
        gs = toGameState ag
        baseMoves = MG.legalGenMoves baseBoard gs
    in map toCoreMove baseMoves

  applyMove (m :: Move c) (ag :: ActiveGame 'ThreeCheck c s) =
    let
        c = colorVal @c
        oppC = colorVal @(Opposite c)
        internalB = internalBoard ag
        internalB' = applyMoveBase m internalB
        (from, to) = case m of
                       StandardMove f t _ -> (f, t)
                       PromotionMove f t _ -> (f, t)
                       CastlingMove f t -> (f, t)
                       EnPassantMove f t -> (f, t)
                       DropMove _ t -> (t, t)
                       Castling960Move _ _ -> error "Castling960Move invalid in ThreeCheck"

        newCR = updateCastlingRights (castlingRights ag) from to

        newEP = case m of
                  StandardMove f t pt -> if pt == Pawn && isDoublePush f t then Just (getFile f) else Nothing
                  _ -> Nothing

        newHMC = halfMoveClock ag + 1
        newFMN = fullMoveNumber ag + (if c == Black then 1 else 0)

        baseBoard = internalB'
        nextTurnGS = GS.GameState
          { GS.turn = toColor oppC
          , GS.castlingRights = toCastlingRights newCR
          , GS.epSquare = case newEP of
                            Nothing -> Nothing
                            Just f -> Just (toSquare (Square f (epRank oppC)))
          , GS.halfmoveClock = newHMC
          , GS.fullmoveNumber = newFMN
          , GS.zobristHash = 0
          }

        isChecked = Val.isCheck baseBoard nextTurnGS

        (wChecks, bChecks) = variantState ag
        (wChecks', bChecks') = if isChecked
                               then if c == White then (wChecks + 1, bChecks) else (wChecks, bChecks + 1)
                               else (wChecks, bChecks)

        newVariantState = (wChecks', bChecks')

    in if isChecked
       then Transition (ActiveGame internalB' newCR newEP newHMC newFMN newVariantState SChecked)
       else Transition (ActiveGame internalB' newCR newEP newHMC newFMN newVariantState SSafe)

  executeMove (m :: Move c) (ag :: ActiveGame 'ThreeCheck c s) =
    case applyMove m ag of
      Transition nextAg ->
         let c = colorVal @c
             (wChecks, bChecks) = variantState nextAg
             winByCheck = (if c == White then wChecks else bChecks) >= 3
         in if winByCheck
            then Checkmate (Winner c)
            else
               let checked = case checkStatus nextAg of SChecked -> True; _ -> False
                   (hasMoves, nextAgChecked) = if checked
                      then (not (null (generateMoves (setStatus SChecked nextAg))), Right (setStatus SChecked nextAg))
                      else (not (null (generateMoves (setStatus SSafe nextAg))), Left (setStatus SSafe nextAg))
               in if checked
                  then if hasMoves
                       then case nextAgChecked of Right finalAg -> Continue finalAg; Left _ -> error "Impossible"
                       else Checkmate (Winner (colorVal @c))
                  else if hasMoves
                       then case nextAgChecked of Left finalAg -> Continue finalAg; Right _ -> error "Impossible"
                       else Stalemate
