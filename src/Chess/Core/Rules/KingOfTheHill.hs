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

module Chess.Core.Rules.KingOfTheHill where

import Chess.Core.Rules.Class
import Chess.Core.Rules.Common
import Chess.Core.Board.Internal
import Chess.Core.Game.Internal
import Chess.Core.Move.Internal

import qualified Chess.Board.MoveGen as MG

instance VariantMoveGen 'KingOfTheHill where
  generateMoves (ag :: ActiveGame 'KingOfTheHill c s) =
    let baseBoard = internalBoard ag
        gs = toGameState ag
        baseMoves = MG.legalGenMovesList baseBoard gs
    in map toCoreMove baseMoves

instance VariantMoveApply 'KingOfTheHill where
  applyMove = genericApplyMove

instance VariantMoveExecute 'KingOfTheHill where
  executeMove (m :: Move c) (ag :: ActiveGame 'KingOfTheHill c s) =
    case applyMove m ag of
      Transition _nextAg ->
         let
            cColor = colorVal @c

            -- Extract destination for win check
            to = case m of
                       QuietMove _ t _ -> t
                       CaptureMove _ t _ _ -> t
                       PromotionMove _ t _ -> t
                       PromotionCaptureMove _ t _ _ -> t
                       CastlingMove _ t -> t
                       EnPassantMove _ t -> t
                       DropMove _ t -> t
                       Castling960Move _ _ -> error "Castling960Move invalid in KingOfTheHill"

            isKing = case m of
                       QuietMove _ _ pt -> pt == King
                       CaptureMove _ _ pt _ -> pt == King
                       CastlingMove _ _ -> True
                       _ -> False

            centerSquares = [ Square FileE Rank4, Square FileD Rank4
                            , Square FileE Rank5, Square FileD Rank5 ]
            kingInCenter = isKing && to `elem` centerSquares

         in if kingInCenter
            then Checkmate (Winner cColor)
            else genericExecuteMove m ag

  perftExecuteMove (m :: Move c) (ag :: ActiveGame 'KingOfTheHill c s) =
    case applyMove m ag of
      Transition nextAg ->
         let
            cColor = colorVal @c
            to = case m of
                       QuietMove _ t _ -> t
                       CaptureMove _ t _ _ -> t
                       PromotionMove _ t _ -> t
                       PromotionCaptureMove _ t _ _ -> t
                       CastlingMove _ t -> t
                       EnPassantMove _ t -> t
                       DropMove _ t -> t
                       Castling960Move _ _ -> error "Castling960Move invalid in KingOfTheHill"

            isKing = case m of
                       QuietMove _ _ pt -> pt == King
                       CaptureMove _ _ pt _ -> pt == King
                       CastlingMove _ _ -> True
                       _ -> False

            centerSquares = [ Square FileE Rank4, Square FileD Rank4
                            , Square FileE Rank5, Square FileD Rank5 ]
            kingInCenter = isKing && to `elem` centerSquares

         in if kingInCenter
            then Checkmate (Winner cColor)
            else Continue (nextAg { checkStatus = SUnchecked })

instance VariantPerft 'KingOfTheHill

instance ChessVariant 'KingOfTheHill
