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

import qualified Chess.Types as T
import qualified Chess.Board.Base as Base
import qualified Chess.Board.GameState as GS
import qualified Chess.Board.MoveGen as MG
import qualified Chess.Board.Validation as Val

instance ChessVariant 'KingOfTheHill where
  generateMoves (ag :: ActiveGame 'KingOfTheHill c s) =
    let baseBoard = internalBoard ag
        gs = toGameState ag
        baseMoves = MG.legalGenMoves baseBoard gs
    in map toCoreMove baseMoves

  applyMove (m :: Move c) (ag :: ActiveGame 'KingOfTheHill c s) =
    let
        c = colorVal @c
        internalB = internalBoard ag
        internalB' = applyMoveBase m internalB
        (from, to) = case m of
                       StandardMove f t -> (f, t)
                       PromotionMove f t _ -> (f, t)
                       CastlingMove f t -> (f, t)
                       EnPassantMove f t -> (f, t)
                       DropMove _ t -> (t, t)
                       Castling960Move _ _ -> error "Castling960Move invalid in KingOfTheHill"

        newCR = updateCastlingRights (castlingRights ag) from to
        movedPiece = Base.pieceAt internalB' (toSquare to)
        isPawn = case movedPiece of
                   Just p -> T.pieceType p == T.Pawn
                   _ -> False

        newEP = case m of
                  StandardMove f t -> if isPawn && isDoublePush f t then Just (getFile f) else Nothing
                  _ -> Nothing

        newHMC = halfMoveClock ag + 1
        newFMN = fullMoveNumber ag + (if c == Black then 1 else 0)

        baseBoard = internalB'
        nextTurnGS = GS.GameState
          { GS.turn = toColor (colorVal @(Opposite c))
          , GS.castlingRights = toCastlingRights newCR
          , GS.epSquare = case newEP of
                            Nothing -> Nothing
                            Just f -> Just (toSquare (Square f (epRank (colorVal @(Opposite c)))))
          , GS.halfmoveClock = newHMC
          , GS.fullmoveNumber = newFMN
          }

        isChecked = Val.isCheck baseBoard nextTurnGS

    in if isChecked
       then Transition (ActiveGame internalB' newCR newEP newHMC newFMN () SChecked :: ActiveGame 'KingOfTheHill (Opposite c) 'Checked)
       else Transition (ActiveGame internalB' newCR newEP newHMC newFMN () SSafe    :: ActiveGame 'KingOfTheHill (Opposite c) 'Safe)

  executeMove (m :: Move c) ag =
    case applyMove m ag of
      Transition nextGame ->
        let
            baseBoard = internalBoard nextGame
            gs = toGameState nextGame
            c = colorVal @c

            (from, to) = case m of
                           StandardMove f t -> (f, t)
                           PromotionMove f t _ -> (f, t)
                           CastlingMove f t -> (f, t)
                           EnPassantMove f t -> (f, t)
                           DropMove _ t -> (t, t)
                           Castling960Move _ _ -> error "Castling960Move invalid in KingOfTheHill"

            -- We need movedPiece type for kingInCenter check
            -- But nextGame has already moved pieces.
            -- Wait, to check if *moved* piece was King, we can look at `to` square in `baseBoard`.
            movedPiece = Base.pieceAt baseBoard (toSquare to)
            isKing = case movedPiece of
                       Just p -> T.pieceType p == T.King
                       _ -> False

            kingInCenter = isKing && to `elem` centerSquares
            centerSquares = [ Square FileE Rank4, Square FileD Rank4
                            , Square FileE Rank5, Square FileD Rank5 ]

            isChecked = case checkStatus nextGame of SChecked -> True; SSafe -> False
            hasMoves = Val.hasLegalMoves baseBoard gs

        in if kingInCenter
           then Checkmate (Winner c)
           else case (isChecked, hasMoves) of
             (True, False) -> Checkmate (Winner c)
             (False, False) -> Stalemate
             (True, True) -> Continue nextGame
             (False, True) -> Continue nextGame
