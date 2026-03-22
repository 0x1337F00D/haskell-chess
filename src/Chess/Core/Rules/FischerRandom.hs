{-# LANGUAGE BangPatterns #-}
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

module Chess.Core.Rules.FischerRandom where

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
import Data.Bits (testBit, (.&.))

-- | Create a game from FEN string (Fischer Random variant).
fischerRandomGameFromFEN :: String -> Maybe (Game 'FischerRandom 'Active)
fischerRandomGameFromFEN s = do
  (baseBoard, gs) <- Fen.parseFen s

  let c = case GS.turn gs of
            T.White -> White
            T.Black -> Black

      -- Extract Rook Files from GameState CastlingRights (Bitboard)
      whiteRooks = GS.castlingRights gs .&. BB.bbRank1
      blackRooks = GS.castlingRights gs .&. BB.bbRank8

      frState = FischerRandomState whiteRooks blackRooks

      checked = Val.isCheck baseBoard gs

      -- Check for moves using 960 generator
      -- We need to construct a temp ActiveGame to call generateMoves
      hasMoves = case c of
        White -> not (null (generateMoves (ActiveGame baseBoard gs frState SSafe :: ActiveGame 'FischerRandom 'White 'Safe)))
        Black -> not (null (generateMoves (ActiveGame baseBoard gs frState SSafe :: ActiveGame 'FischerRandom 'Black 'Safe)))

  if hasMoves
    then case c of
      White -> if checked
               then return $ InProgressGame (ActiveGame baseBoard gs frState SChecked :: ActiveGame 'FischerRandom 'White 'Checked)
               else return $ InProgressGame (ActiveGame baseBoard gs frState SSafe    :: ActiveGame 'FischerRandom 'White 'Safe)
      Black -> if checked
               then return $ InProgressGame (ActiveGame baseBoard gs frState SChecked :: ActiveGame 'FischerRandom 'Black 'Checked)
               else return $ InProgressGame (ActiveGame baseBoard gs frState SSafe    :: ActiveGame 'FischerRandom 'Black 'Safe)
    else Nothing

instance ChessVariant 'FischerRandom where
  generateMoves (ag :: ActiveGame 'FischerRandom c s) =
    let baseBoard = internalBoard ag
        gs = toGameState ag
        c = colorVal @c

        baseMoves = MG.legalGenMovesList baseBoard gs
        standardMoves = map toCoreMove baseMoves
        -- Filter out standard CastlingMoves
        nonCastlingMoves = filter (not . isStandardCastling) standardMoves

        isStandardCastling (CastlingMove _ _) = True
        isStandardCastling _ = False

        -- Generate 960 Castling Moves
        frState = variantState ag
        rooksBB = if c == White then whiteRookFiles frState else blackRookFiles frState

        kingSq = if MG.hasKing baseBoard (toColor c) then MG.kingSquareFast baseBoard (toColor c) else T.Square 0

        currentRights = GS.castlingRights gs

        kFile = T.squareFile kingSq
        kRank = T.squareRank kingSq

        -- Get eligible rooks
        rooks = BB.mapBitboard id rooksBB

        genCastling rSq =
            let rFile = T.squareFile rSq
                rRank = T.squareRank rSq
                isKSide = rFile > kFile

                hasRight = testBit currentRights (T.unSquare rSq)

            in if hasRight && rRank == kRank
               then if isCastlingValid baseBoard (toColor c) kingSq rSq isKSide
                    then [Castling960Move (fromSquare kingSq) (fromSquare rSq)]
                    else []
               else []

        castling960Moves = concatMap genCastling rooks

    in nonCastlingMoves ++ castling960Moves

  applyMove (m :: Move c) (ag :: ActiveGame 'FischerRandom c s) =
    let
        c = colorVal @c
        internalB = internalBoard ag
        internalB' = applyMoveBase m internalB

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
                    if isPawn && isDoublePush f t
                    then toSquare (Square (getFile f) (epRank (colorVal @(Opposite c))))
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

        frState = variantState ag

        nextAg = ActiveGame internalB' newGS frState SUnchecked

    in Transition nextAg

  executeMove = genericExecuteMove
  perftExecuteMove = genericPerftExecuteMove

-- Helper to validate 960 castling
isCastlingValid :: Base.Board -> T.Color -> T.Square -> T.Square -> Bool -> Bool
isCastlingValid b c kSq rSq isKSide =
    let rank = T.squareRank kSq
        kFile = T.squareFile kSq
        rFile = T.squareFile rSq

        -- Target squares
        destKFile = if isKSide then 6 else 2 -- G or C
        destRFile = if isKSide then 5 else 3 -- F or D

        -- Helper to check a range of files for emptiness and/or safety
        loop :: Int -> Int -> (T.Square -> Bool) -> Bool
        loop !i !maxI p
            | i > maxI = True
            | otherwise = p (T.Square (rank * 8 + i)) && loop (i + 1) maxI p

        isEmpty s = not (testBit (Base.occupiedTotal b) (T.unSquare s))

        oppC = Base.oppositeColor c
        isSafe s = not (Base.isAttackedBy b oppC s)

        -- Check conditions
        checkPred s = s == kSq || s == rSq || isEmpty s

        -- Path 1: Between King and Rook (exclusive) must be empty (except K/R)
        start = min kFile rFile
        end = max kFile rFile
        path1Ok = loop (start + 1) (end - 1) checkPred

        -- Path 2: Between King and Dest (inclusive) must be empty (except K/R) and safe
        startK = min kFile destKFile
        endK = max kFile destKFile
        pathKOk = loop startK endK (\s -> checkPred s && isSafe s)

        -- Path 3: Between Rook and Dest (inclusive) must be empty (except K/R)
        startR = min rFile destRFile
        endR = max rFile destRFile
        pathROk = loop startR endR checkPred

    in path1Ok && pathROk && pathKOk

