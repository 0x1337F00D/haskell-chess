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
import Data.Bits (testBit, countTrailingZeros, (.|.))

-- | Create the initial game state for Standard chess.
initialGame :: Game 'Standard 'Active
initialGame =
  let b = initialBoard
      cr = CastlingRights (castlingWhiteKingSide .|. castlingWhiteQueenSide .|. castlingBlackKingSide .|. castlingBlackQueenSide)
      ag = ActiveGame
           { internalBoard = toBaseBoard b
           , castlingRights = cr
           , enPassantTarget = Nothing
           , halfMoveClock = 0
           , fullMoveNumber = 1
           , variantState = ()
           } :: ActiveGame 'Standard 'White 'Safe
  in InProgressGame ag

-- | Create a game from FEN string (Standard variant).
gameFromFEN :: String -> Maybe (Game 'Standard 'Active)
gameFromFEN s = do
  (baseBoard, gs) <- Fen.parseFen s

  let c = case GS.turn gs of
            T.White -> White
            T.Black -> Black

      -- Map bitboard bits to CastlingRights bits
      crVal = (if testBit (GS.castlingRights gs) (countTrailingZeros BB.BB_H1) then castlingWhiteKingSide else 0) .|.
              (if testBit (GS.castlingRights gs) (countTrailingZeros BB.BB_A1) then castlingWhiteQueenSide else 0) .|.
              (if testBit (GS.castlingRights gs) (countTrailingZeros BB.BB_H8) then castlingBlackKingSide else 0) .|.
              (if testBit (GS.castlingRights gs) (countTrailingZeros BB.BB_A8) then castlingBlackQueenSide else 0)

      cr = CastlingRights crVal

      ep = case GS.epSquare gs of
             Nothing -> Nothing
             Just sq -> Just (getFile (fromBaseSquare sq))

      hmc = GS.halfmoveClock gs
      fmn = GS.fullmoveNumber gs

      checked = Val.isCheck baseBoard gs
      hasMoves = Val.hasLegalMoves baseBoard gs

  if hasMoves
    then case c of
      White -> if checked
               then return $ InProgressGame (ActiveGame baseBoard cr ep hmc fmn () :: ActiveGame 'Standard 'White 'Checked)
               else return $ InProgressGame (ActiveGame baseBoard cr ep hmc fmn () :: ActiveGame 'Standard 'White 'Safe)
      Black -> if checked
               then return $ InProgressGame (ActiveGame baseBoard cr ep hmc fmn () :: ActiveGame 'Standard 'Black 'Checked)
               else return $ InProgressGame (ActiveGame baseBoard cr ep hmc fmn () :: ActiveGame 'Standard 'Black 'Safe)
    else Nothing

instance ChessVariant 'Standard where
  generateMoves (ag :: ActiveGame 'Standard c s) =
    let baseBoard = internalBoard ag
        gs = toGameState ag
        baseMoves = MG.legalGenMoves baseBoard gs
    in map toCoreMove baseMoves

  executeMove (m :: Move c) (ag :: ActiveGame 'Standard c s) =
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

    in case (isChecked, hasMoves) of
         (True, False) -> Checkmate (Winner c)
         (False, False) -> Stalemate
         (True, True) -> Continue (ActiveGame internalB' newCR newEP newHMC newFMN () :: ActiveGame 'Standard (Opposite c) 'Checked)
         (False, True) -> Continue (ActiveGame internalB' newCR newEP newHMC newFMN () :: ActiveGame 'Standard (Opposite c) 'Safe)
