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

module Chess.Core.Rules.Horde where

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
import Data.Bits (popCount, testBit, setBit, (.|.))
import Data.Word (Word64)
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM

-- | Initial Game State for Horde
hordeInitialGame :: Game 'Horde 'Active
hordeInitialGame =
  let
      -- White Pawns: Ranks 1, 2, 3, 4 + B5, C5, F5, G5
      wPawns =
          foldl setBit (0 :: BB.Bitboard) (
              [ i | i <- [0..31] ] ++ -- Ranks 1-4 (0-7, 8-15, 16-23, 24-31)
              [ 33, 34, 37, 38 ]      -- B5, C5, F5, G5 (Rank 5 is 32-39)
          )

      -- Black Pieces: Standard Setup
      bPawns = 0x00FF000000000000 -- Rank 7 (Indices 48-55)
      bKnights = (1 `setBit` 57) .|. (1 `setBit` 62) -- B8, G8
      bBishops = (1 `setBit` 58) .|. (1 `setBit` 61) -- C8, F8
      bRooks   = (1 `setBit` 56) .|. (1 `setBit` 63) -- A8, H8
      bQueens  = (1 `setBit` 59)                     -- D8
      bKings   = (1 `setBit` 60)                     -- E8

      wOcc = wPawns
      bOcc = bPawns .|. bKnights .|. bBishops .|. bRooks .|. bQueens .|. bKings

      baseBoard = Base.computeScores $ Base.Board
          { Base.whitePawns   = wPawns
          , Base.blackPawns   = bPawns
          , Base.whiteKnights = 0
          , Base.blackKnights = bKnights
          , Base.whiteBishops = 0
          , Base.blackBishops = bBishops
          , Base.whiteRooks   = 0
          , Base.blackRooks   = bRooks
          , Base.whiteQueens  = 0
          , Base.blackQueens  = bQueens
          , Base.whiteKings   = 0
          , Base.blackKings   = bKings
          , Base.occupiedWhite = wOcc
          , Base.occupiedBlack = bOcc
          , Base.occupiedTotal = wOcc .|. bOcc
          , Base.whiteDiagonal = 0
          , Base.whiteOrthogonal = 0
          , Base.blackDiagonal = bBishops .|. bQueens
          , Base.blackOrthogonal = bRooks .|. bQueens
          , Base.scoreWhite = 0
          , Base.scoreBlack = 0
          , Base.gamePhase = 0
          }

      -- Castling Rights: Black only (King Side + Queen Side)
      -- White has no King, so no castling rights.
      -- Black Rooks at A8 (56) and H8 (63).
      crBB = BB.BB_A8 .|. BB.BB_H8

      gs = GS.initialGameState
           { GS.castlingRights = crBB
           , GS.turn = T.White
           }

      ag = ActiveGame
           { internalBoard = baseBoard
           , gameState = gs
           , variantState = ()
           , checkStatus = SSafe
           } :: ActiveGame 'Horde 'White 'Safe

  in InProgressGame ag

instance ChessVariant 'Horde where
  generateMoves (ag :: ActiveGame 'Horde c s) =
    let baseBoard = internalBoard ag
        gs = toGameState ag
        c = colorVal @c

        baseMoves = MG.legalGenMovesList baseBoard gs

        -- Custom White Pawn Moves (Rank 1 double push)
        whiteExtraMoves = if c == White
            then
               let
                   pawns = Base.whitePawns baseBoard
                   occ = Base.occupiedTotal baseBoard

                   -- Pawns on Rank 1 (indices 0-7)
                   -- Can move to Rank 3 (index + 16) if Rank 2 (index + 8) and Rank 3 are empty.

                   genRank1Double fromIdx =
                       let idx1 = fromIdx + 8
                           idx2 = fromIdx + 16
                       in if fromIdx < 8
                             && testBit pawns fromIdx
                             && not (testBit occ idx1)
                             && not (testBit occ idx2)
                          then [MG.GenQuiet (T.Square fromIdx) (T.Square idx2) T.Pawn]
                          else []

               in concatMap genRank1Double [0..7]
            else []

        allMoves = if c == White
                   then baseMoves ++ whiteExtraMoves
                   else baseMoves -- Black uses standard generation

    in map toCoreMove allMoves

  applyMove (m :: Move c) (ag :: ActiveGame 'Horde c s) =
    let
        c = colorVal @c
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
                       Castling960Move _ _ -> error "Castling960Move invalid in Horde"

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

        newEP = case m of
                  QuietMove f t _ ->
                      if isPawn
                      then
                          if isDoublePush f t
                          then toSquare (Square (getFile f) (epRank (colorVal @(Opposite c))))
                          -- Horde Special: Rank 1 Double Push
                          else if c == White && (fromEnum (getRank f) == 0) && (fromEnum (getRank t) == 2)
                               then toSquare (Square (getFile f) Rank2)
                               else T.NoSquare
                      else T.NoSquare
                  _ -> T.NoSquare

        isCapture = case m of
                      CaptureMove {} -> True
                      PromotionCaptureMove {} -> True
                      EnPassantMove _ _ -> True
                      _ -> False

        newHMC = if isPawn || isCapture then 0 else GS.halfmoveClock gs + 1
        newFMN = GS.fullmoveNumber gs + (if c == Black then 1 else 0)

        newGS = gsUpdated
          { GS.turn = toColor (colorVal @(Opposite c))
          , GS.epSquare = newEP
          , GS.halfmoveClock = newHMC
          , GS.fullmoveNumber = newFMN
          , GS.zobristHash = 0
          }

        nextAg = ActiveGame internalB' newGS () SUnchecked

    in Transition nextAg

  executeMove (m :: Move c) (ag :: ActiveGame 'Horde c s) =
    case applyMove m ag of
      Transition nextAg ->
        let
           oppC = colorVal @(Opposite c)
           baseBoard = internalBoard nextAg

           nextTurnGS = toGameState nextAg

           -- Win Conditions

           -- 1. White Wins if Black is Checkmated
           blackInCheck = if oppC == Black
                          then Val.isCheck baseBoard nextTurnGS
                          else False -- White has no King

           hasMovesHorde =
               if oppC == White
               then
                    let standardHasMoves = Val.hasLegalMoves baseBoard nextTurnGS
                        -- Check if any Rank 1 double push is possible
                        wPawns = Base.whitePawns baseBoard
                        occ = Base.occupiedTotal baseBoard
                        canPushRank1 i =
                            testBit wPawns i &&
                            not (testBit occ (i+8)) &&
                            not (testBit occ (i+16))
                        extraHasMoves = any canPushRank1 [0..7]
                    in standardHasMoves || extraHasMoves
               else Val.hasLegalMoves baseBoard nextTurnGS -- Black

           -- 2. Black Wins if White has no pieces (Pawns + Promoted)
           whitePiecesCount = popCount (Base.occupiedBy baseBoard T.White)
           blackWins = whitePiecesCount == 0

        in if blackWins
           then Checkmate (Winner Black)
           else case (blackInCheck, hasMovesHorde) of
             (True, False) -> Checkmate (Winner White) -- Black Checkmated
             (False, False) -> Stalemate
             (True, True) -> Continue (setStatus SChecked nextAg)
             (False, True) -> Continue (setStatus SSafe nextAg)

getRank :: Square -> Rank
getRank (Square _ r) = r
