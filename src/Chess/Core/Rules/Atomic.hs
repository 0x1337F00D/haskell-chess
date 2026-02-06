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

module Chess.Core.Rules.Atomic where

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
import Data.Bits ((.&.), complement, (.|.))

instance ChessVariant 'Atomic where
  generateMoves (ag :: ActiveGame 'Atomic c s) =
    let baseBoard = internalBoard ag
        gs = toGameState ag
        c = colorVal @c

        pseudos = MG.pseudoLegalMovesList baseBoard gs

        isKingCapture :: MG.GenMove -> Bool
        isKingCapture gm =
           case gm of
             MG.GenCapture _ _ T.King _ -> True
             _ -> False

        chebyshevDist :: T.Square -> T.Square -> Int
        chebyshevDist (T.Square i1) (T.Square i2) =
           let r1 = i1 `div` 8
               c1 = i1 `mod` 8
               r2 = i2 `div` 8
               c2 = i2 `mod` 8
           in max (abs (r1 - r2)) (abs (c1 - c2))

        isSelfExplosion :: MG.GenMove -> Bool
        isSelfExplosion gm =
           let ownKingSq = MG.kingSquare baseBoard (toColor c)
               checkExplosion t = case ownKingSq of
                                    Just k -> chebyshevDist t k <= 1
                                    Nothing -> False
           in case gm of
                MG.GenCapture _ t _ _ -> checkExplosion t
                MG.GenEnPassant _ t -> checkExplosion t
                MG.GenPromotionCapture _ t _ _ -> checkExplosion t
                _ -> False

        atomicMoves = filter (\gm -> not (isKingCapture gm) && not (isSelfExplosion gm)) pseudos
        validMoves = filter (MG.isLegal baseBoard gs) atomicMoves

    in map toCoreMove validMoves

  applyMove (m :: Move c) (ag :: ActiveGame 'Atomic c s) =
    let c = colorVal @c
        internalB = internalBoard ag

        bBasic = applyMoveBase m internalB
        (from, to) = case m of
                       QuietMove f t _ -> (f, t)
                       CaptureMove f t _ _ -> (f, t)
                       PromotionMove f t _ -> (f, t)
                       PromotionCaptureMove f t _ _ -> (f, t)
                       CastlingMove f t -> (f, t)
                       EnPassantMove f t -> (f, t)
                       DropMove _ t -> (t, t)
                       Castling960Move _ _ -> error "Castling960Move invalid in Atomic"

        isCapture = case m of
                      CaptureMove {} -> True
                      PromotionCaptureMove {} -> True
                      EnPassantMove _ _ -> True
                      _ -> False

        isPawn = case m of
                   QuietMove _ _ pt -> pt == Pawn
                   CaptureMove _ _ pt _ -> pt == Pawn
                   EnPassantMove _ _ -> True
                   PromotionMove _ _ _ -> True
                   PromotionCaptureMove _ _ _ _ -> True
                   DropMove pt _ -> pt == Pawn
                   _ -> False

        bFinal = if isCapture
          then
            let center = to
                centerSq = toSquare center
                explosionMask = BB.kingAttacks centerSq
                maskNonPawns = complement explosionMask
                maskCenter = complement (BB.bbFromSquare centerSq)

                -- Pawns: Only remove the capturing pawn (at center). Surrounding pawns are immune.
                wPawns = Base.whitePawns bBasic .&. maskCenter
                bPawns = Base.blackPawns bBasic .&. maskCenter

                -- Non-Pawns: Remove capturing piece (center) AND exploded pieces (surroundings).
                maskAll = maskNonPawns .&. maskCenter

                wKnights = Base.whiteKnights bBasic .&. maskAll
                wBishops = Base.whiteBishops bBasic .&. maskAll
                wRooks   = Base.whiteRooks   bBasic .&. maskAll
                wQueens  = Base.whiteQueens  bBasic .&. maskAll
                wKings   = Base.whiteKings   bBasic .&. maskAll

                bKnights = Base.blackKnights bBasic .&. maskAll
                bBishops = Base.blackBishops bBasic .&. maskAll
                bRooks   = Base.blackRooks   bBasic .&. maskAll
                bQueens  = Base.blackQueens  bBasic .&. maskAll
                bKings   = Base.blackKings   bBasic .&. maskAll

                b2 = bBasic
                   { Base.whitePawns   = wPawns
                   , Base.blackPawns   = bPawns
                   , Base.whiteKnights = wKnights
                   , Base.blackKnights = bKnights
                   , Base.whiteBishops = wBishops
                   , Base.blackBishops = bBishops
                   , Base.whiteRooks   = wRooks
                   , Base.blackRooks   = bRooks
                   , Base.whiteQueens  = wQueens
                   , Base.blackQueens  = bQueens
                   , Base.whiteKings   = wKings
                   , Base.blackKings   = bKings
                   }
            in Base.updateOccupancy b2
          else bBasic

        gs = gameState ag
        gsUpdated = updateCastlingRights gs m

        newEP = case m of
                  QuietMove f t _ ->
                    if isPawn && isDoublePush f t
                    then toSquare (Square (getFile f) (epRank (colorVal @(Opposite c))))
                    else T.NoSquare
                  _ -> T.NoSquare

        newHMC = if isPawn || isCapture then 0 else GS.halfmoveClock gs + 1
        newFMN = GS.fullmoveNumber gs + (if c == Black then 1 else 0)

        newGS = gsUpdated
          { GS.turn = toColor (colorVal @(Opposite c))
          , GS.epSquare = newEP
          , GS.halfmoveClock = newHMC
          , GS.fullmoveNumber = newFMN
          , GS.zobristHash = 0
          }

        nextAg = ActiveGame bFinal newGS () SUnchecked

    in Transition nextAg

  executeMove (m :: Move c) (ag :: ActiveGame 'Atomic c s) =
    case applyMove m ag of
      Transition nextAg ->
         let
            oppC = colorVal @(Opposite c)
            baseBoard = internalBoard nextAg
            enemyKingSq = MG.kingSquare baseBoard (toColor oppC)
            enemyKingExploded = enemyKingSq == Nothing
         in if enemyKingExploded
            then Checkmate (Winner (colorVal @c))
            else
                let checked = Val.isCheck baseBoard (toGameState nextAg)
                    (hasMoves, nextAgChecked) = if checked
                        then
                           let agChecked = setStatus SChecked nextAg
                           in (not (null (generateMoves agChecked)), Right agChecked)
                        else
                           let agSafe = setStatus SSafe nextAg
                           in (not (null (generateMoves agSafe)), Left agSafe)
                in if checked
                   then if hasMoves
                        then case nextAgChecked of Right finalAg -> Continue finalAg; Left _ -> error "Impossible"
                        else Checkmate (Winner (colorVal @c))
                   else if hasMoves
                        then case nextAgChecked of Left finalAg -> Continue finalAg; Right _ -> error "Impossible"
                        else Stalemate
