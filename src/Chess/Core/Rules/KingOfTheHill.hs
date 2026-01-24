{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Chess.Core.Rules.KingOfTheHill where

import Chess.Core.Board.Internal
import Chess.Core.Game.Internal
import Chess.Core.Move.Internal
import Chess.Core.Rules.Class
import Chess.Core.Rules.Common

import qualified Chess.Types as T
import qualified Chess.Board.Base as Base
import qualified Chess.Board.GameState as GS
import qualified Chess.Board.MoveGen as MG
import qualified Chess.Board.Validation as Val

instance ChessVariant 'KingOfTheHill where
  generateMoves (ag :: ActiveGame 'KingOfTheHill c s) =
    let baseBoard = internalBoard ag
        gs = toGameState ag
        baseMoves = MG.legalMoves baseBoard gs
    in map (toCoreMove baseBoard) baseMoves

  executeMove (m :: Move c) (ag :: ActiveGame 'KingOfTheHill c s) =
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

        newCR = updateCastlingRights (castlingRights ag) from to
        movedPiece = Base.pieceAt internalB' (toSquare to)
        isPawn = case movedPiece of
                   Just p -> T.pieceType p == T.Pawn
                   _ -> False

        isKing = case movedPiece of
                   Just p -> T.pieceType p == T.King
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
        hasMoves = Val.hasLegalMoves baseBoard nextTurnGS

        kingInCenter = isKing && to `elem` centerSquares
        centerSquares = [ Square FileE Rank4, Square FileD Rank4
                        , Square FileE Rank5, Square FileD Rank5 ]

    in if kingInCenter
       then Checkmate (Winner c)
       else case (isChecked, hasMoves) of
         (True, False) -> Checkmate (Winner c)
         (False, False) -> Stalemate
         (True, True) -> Continue (ActiveGame internalB' newCR newEP newHMC newFMN () :: ActiveGame 'KingOfTheHill (Opposite c) 'Checked)
         (False, True) -> Continue (ActiveGame internalB' newCR newEP newHMC newFMN () :: ActiveGame 'KingOfTheHill (Opposite c) 'Safe)
