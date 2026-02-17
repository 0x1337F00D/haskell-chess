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
        baseMoves = MG.legalGenMovesList baseBoard gs
    in map toCoreMove baseMoves

  applyMove (m :: Move c) (ag :: ActiveGame 'ThreeCheck c s) =
    let
        c = colorVal @c
        oppC = colorVal @(Opposite c)
        internalB = internalBoard ag
        internalB' = applyMoveBase m internalB
        (from, to) = case m of
                       QuietMove f t _ -> (f, t)
                       CaptureMove f t _ _ -> (f, t)
                       PromotionMove f t _ -> (f, t)
                       PromotionCaptureMove f t _ _ -> (f, t)
                       CastlingMove f t -> (f, t)
                       EnPassantMove f t -> (f, t)
                       DropMove _ t -> (t, t)
                       Castling960Move _ _ -> error "Castling960Move invalid in ThreeCheck"

        gs = gameState ag
        gsUpdated = updateCastlingRights gs m

        isPawn = case m of
                   QuietMove _ _ pt -> pt == Pawn
                   CaptureMove _ _ pt _ -> pt == Pawn
                   EnPassantMove _ _ -> True
                   PromotionMove _ _ _ -> True
                   PromotionCaptureMove _ _ _ _ -> True
                   DropMove pt _ -> pt == Pawn
                   _ -> False

        isCapture = case m of
                      CaptureMove {} -> True
                      PromotionCaptureMove {} -> True
                      EnPassantMove _ _ -> True
                      _ -> False

        newEP = case m of
                  QuietMove f t _ ->
                    if isPawn && isDoublePush f t
                    then toSquare (Square (getFile f) (epRank (colorVal @(Opposite c))))
                    else T.NoSquare
                  _ -> T.NoSquare

        newHMC = if isPawn || isCapture then 0 else GS.halfmoveClock gs + 1
        newFMN = GS.fullmoveNumber gs + (if c == Black then 1 else 0)

        baseBoard = internalB'
        nextTurnGS = GS.setZobristHash 0 $
                     GS.setFullmoveNumber newFMN $
                     GS.setHalfmoveClock newHMC $
                     GS.setEpSquare newEP $
                     GS.setTurn (toColor oppC) gsUpdated

        isChecked = Val.isCheck baseBoard nextTurnGS

        (wChecks, bChecks) = variantState ag
        (wChecks', bChecks') = if isChecked
                               then if c == White then (wChecks + 1, bChecks) else (wChecks, bChecks + 1)
                               else (wChecks, bChecks)

        newVariantState = (wChecks', bChecks')

    in if isChecked
       then Transition (ActiveGame internalB' nextTurnGS newVariantState SChecked)
       else Transition (ActiveGame internalB' nextTurnGS newVariantState SSafe)

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
