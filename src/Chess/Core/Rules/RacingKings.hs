{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Chess.Core.Rules.RacingKings where

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

instance ChessVariant 'RacingKings where
  generateMoves (ag :: ActiveGame 'RacingKings c s) =
    let baseBoard = internalBoard ag
        gs = toGameState ag
        baseMoves = MG.legalMoves baseBoard gs
        coreMoves = map (toCoreMove baseBoard) baseMoves
        c = colorVal @c
        oppC = opposite c

        noGiveCheck m =
            let baseNext = applyMoveBase m baseBoard
            in not (Val.isCheck baseNext (dummyGameState oppC))
          where
            dummyGameState col = GS.initialGameState { GS.turn = toColor col }

    in filter noGiveCheck coreMoves

  executeMove (m :: Move c) (ag :: ActiveGame 'RacingKings c s) =
    let c = colorVal @c
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
        newEP = case m of
                  StandardMove f t -> if isPawn && isDoublePush f t then Just (getFile f) else Nothing
                  _ -> Nothing
        newHMC = halfMoveClock ag + 1
        newFMN = fullMoveNumber ag + (if c == Black then 1 else 0)

        nextGameCandidate :: ActiveGame 'RacingKings (Opposite c) 'Safe
        nextGameCandidate = ActiveGame internalB' newCR newEP newHMC newFMN ()

        nextMoves = generateMoves nextGameCandidate
        realHasMoves = not (null nextMoves)

        wKingSq = MG.kingSquare internalB' T.White
        bKingSq = MG.kingSquare internalB' T.Black
        wInGoal = case wKingSq of Just sq -> T.squareRank sq == 7; _ -> False
        bInGoal = case bKingSq of Just sq -> T.squareRank sq == 7; _ -> False

        result =
             if c == White
             then if wInGoal
                  then if realHasMoves
                       then Continue nextGameCandidate
                       else Checkmate (Winner White)
                  else
                       if realHasMoves then Continue nextGameCandidate else Stalemate
             else
                  if bInGoal && wInGoal then Checkmate Draw else
                  if wInGoal then Checkmate (Winner White) else
                  if bInGoal then Checkmate (Winner Black) else
                  if realHasMoves then Continue nextGameCandidate else Stalemate

    in result
