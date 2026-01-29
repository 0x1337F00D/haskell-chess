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

import Chess.Types (Depth)

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
               then return $ InProgressGame (ActiveGame baseBoard cr ep hmc fmn () SChecked :: ActiveGame 'Standard 'White 'Checked)
               else return $ InProgressGame (ActiveGame baseBoard cr ep hmc fmn () SSafe    :: ActiveGame 'Standard 'White 'Safe)
      Black -> if checked
               then return $ InProgressGame (ActiveGame baseBoard cr ep hmc fmn () SChecked :: ActiveGame 'Standard 'Black 'Checked)
               else return $ InProgressGame (ActiveGame baseBoard cr ep hmc fmn () SSafe    :: ActiveGame 'Standard 'Black 'Safe)
    else Nothing

instance ChessVariant 'Standard where
  generateMoves (ag :: ActiveGame 'Standard c s) =
    let baseBoard = internalBoard ag
        gs = toGameState ag

        -- Optimization: dispatch based on check status
        baseMoves = case checkStatus ag of
             SChecked ->
                 -- If in check, castling is illegal. Construct pseudo-legal moves excluding castling.
                 let pseudos = concat
                        [ MG.pawnMoves baseBoard gs
                        , MG.pieceMoves baseBoard gs T.Knight
                        , MG.pieceMoves baseBoard gs T.Bishop
                        , MG.pieceMoves baseBoard gs T.Rook
                        , MG.pieceMoves baseBoard gs T.Queen
                        , MG.pieceMoves baseBoard gs T.King
                        ]
                 in filter (MG.isLegal baseBoard gs) pseudos
             _ -> MG.legalGenMoves baseBoard gs

    in map toCoreMove baseMoves

  applyMove = genericApplyMove
  executeMove = genericExecuteMove
  perftVariant d ag = go d (internalBoard ag) (toGameState ag)
    where
      go 0 _ _ = 1
      go 1 b gs = length (MG.legalGenMoves b gs)
      go n b gs = sum $ map (\gm ->
             let (b', gs') = MG.applyGenMove b gs gm
             in go (n-1) b' gs'
           ) (MG.legalGenMoves b gs)
