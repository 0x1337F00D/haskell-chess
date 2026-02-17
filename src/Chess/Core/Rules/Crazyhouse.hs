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

module Chess.Core.Rules.Crazyhouse where

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
import Data.Bits (testBit, countTrailingZeros, (.|.), setBit, clearBit)
import Data.Word (Word8)

-- | Create a game from FEN string (Crazyhouse variant).
crazyhouseGameFromFEN :: String -> Maybe (Game 'Crazyhouse 'Active)
crazyhouseGameFromFEN s = do
  (baseBoard, gs, extra) <- Fen.parseFenRest s

  let c = case GS.turn gs of
            T.White -> White
            T.Black -> Black

      -- Parse pockets
      pocketStr = case filter (\x -> not (null x) && head x == '[') extra of
                    (p:_) -> p
                    [] -> "[]"

      -- Helper to add to pocket
      addToPockets p pt = case pt of
          Pawn   -> p { pocketPawns   = pocketPawns p + 1 }
          Knight -> p { pocketKnights = pocketKnights p + 1 }
          Bishop -> p { pocketBishops = pocketBishops p + 1 }
          Rook   -> p { pocketRooks   = pocketRooks p + 1 }
          Queen  -> p { pocketQueens  = pocketQueens p + 1 }
          King   -> p -- Should not happen

      (wPocket, bPocket) = foldr dist (emptyPockets, emptyPockets) (filter (`elem` "PNBRQKpnbrqk") pocketStr)
        where
          dist char (wm, bm) =
             case T.fromSymbol char of
                Just (T.Piece T.White pt) -> (addToPockets wm (fromPieceType pt), bm)
                Just (T.Piece T.Black pt) -> (wm, addToPockets bm (fromPieceType pt))
                Nothing -> (wm, bm)

      vs = CrazyhouseState wPocket bPocket 0

      checked = Val.isCheck baseBoard gs

      -- Check if any moves available (including drops)
      generateDrops :: forall col. KnownColor col => [Move col]
      generateDrops =
           let col = colorVal @col
               pocket = if col == White then wPocket else bPocket
               emptySqs = [ Square f r | f <- [FileA .. FileH], r <- [Rank1 .. Rank8], Base.pieceAt baseBoard (toSquare (Square f r)) == Nothing ]

               genDrops pt count =
                  if count <= 0 then []
                  else
                     let validSquares = if pt == Pawn
                                        then filter (\(Square _ r) -> r /= Rank1 && r /= Rank8) emptySqs
                                        else emptySqs
                     in map (DropMove pt) validSquares

               drops = concat
                   [ genDrops Pawn (pocketPawns pocket)
                   , genDrops Knight (pocketKnights pocket)
                   , genDrops Bishop (pocketBishops pocket)
                   , genDrops Rook (pocketRooks pocket)
                   , genDrops Queen (pocketQueens pocket)
                   ]

               safe m = not (Val.isCheck (applyMoveBase m baseBoard) gs)
           in filter safe drops

      hasMoves :: forall col. KnownColor col => Bool
      hasMoves = Val.hasLegalMoves baseBoard gs || not (null (generateDrops @col))

  case c of
      White -> if hasMoves @'White
               then if checked
                    then return $ InProgressGame (ActiveGame baseBoard gs vs SChecked :: ActiveGame 'Crazyhouse 'White 'Checked)
                    else return $ InProgressGame (ActiveGame baseBoard gs vs SSafe    :: ActiveGame 'Crazyhouse 'White 'Safe)
               else Nothing
      Black -> if hasMoves @'Black
               then if checked
                    then return $ InProgressGame (ActiveGame baseBoard gs vs SChecked :: ActiveGame 'Crazyhouse 'Black 'Checked)
                    else return $ InProgressGame (ActiveGame baseBoard gs vs SSafe    :: ActiveGame 'Crazyhouse 'Black 'Safe)
               else Nothing

instance ChessVariant 'Crazyhouse where
  generateMoves (ag :: ActiveGame 'Crazyhouse c s) =
    let baseBoard = internalBoard ag
        gs = toGameState ag
        c = colorVal @c

        baseMoves = MG.legalGenMovesList baseBoard gs
        standardMoves = map toCoreMove baseMoves

        (CrazyhouseState wPocket bPocket _) = variantState ag
        pocket = if c == White then wPocket else bPocket

        emptySquares = [ Square f r | f <- [FileA .. FileH], r <- [Rank1 .. Rank8], Base.pieceAt baseBoard (toSquare (Square f r)) == Nothing ]

        genDrops pt count =
          if count <= 0 then []
          else
             let validSquares = if pt == Pawn
                                then filter (\(Square _ r) -> r /= Rank1 && r /= Rank8) emptySquares
                                else emptySquares
             in map (DropMove pt) validSquares

        dropMoves = concat
           [ genDrops Pawn (pocketPawns pocket)
           , genDrops Knight (pocketKnights pocket)
           , genDrops Bishop (pocketBishops pocket)
           , genDrops Rook (pocketRooks pocket)
           , genDrops Queen (pocketQueens pocket)
           ]

        isSafeDrop m =
           let nextBase = applyMoveBase m baseBoard
           in not (Val.isCheck nextBase gs)

        validDropMoves = filter isSafeDrop dropMoves

    in standardMoves ++ validDropMoves

  applyMove (m :: Move c) (ag :: ActiveGame 'Crazyhouse c s) =
    let
        c = colorVal @c
        oppC = colorVal @(Opposite c)
        internalB = internalBoard ag
        internalB' = applyMoveBase m internalB

        (CrazyhouseState wPocket bPocket promoted) = variantState ag

        -- Helper to modify pockets
        updatePockets p pt f = case pt of
          Pawn   -> p { pocketPawns   = f (pocketPawns p) }
          Knight -> p { pocketKnights = f (pocketKnights p) }
          Bishop -> p { pocketBishops = f (pocketBishops p) }
          Rook   -> p { pocketRooks   = f (pocketRooks p) }
          Queen  -> p { pocketQueens  = f (pocketQueens p) }
          King   -> p

        (from, to) = case m of
                       QuietMove f t _ -> (f, t)
                       CaptureMove f t _ _ -> (f, t)
                       PromotionMove f t _ -> (f, t)
                       PromotionCaptureMove f t _ _ -> (f, t)
                       CastlingMove f t -> (f, t)
                       EnPassantMove f t -> (f, t)
                       DropMove _ t -> (t, t)
                       Castling960Move _ _ -> error "Castling960Move invalid in Crazyhouse"

        ((wPocket', bPocket'), promoted') = case m of
           DropMove p _ ->
              let pockets = if c == White
                            then (updatePockets wPocket p (\x -> x - 1), bPocket)
                            else (wPocket, updatePockets bPocket p (\x -> x - 1))
              in (pockets, promoted)
           _ ->
              let capture = case m of
                              CaptureMove _ _ _ cap -> Just (T.Piece (toColor oppC) (toPieceType cap))
                              PromotionCaptureMove _ _ _ cap -> Just (T.Piece (toColor oppC) (toPieceType cap))
                              EnPassantMove _ _ -> Just (T.Piece (toColor oppC) T.Pawn)
                              _ -> Nothing

                  capturedSquare = case m of
                                     EnPassantMove f t -> Just (getEpCapturedSquare f t)
                                     CaptureMove _ t _ _ -> Just t
                                     PromotionCaptureMove _ t _ _ -> Just t
                                     _ -> Nothing

                  pockets' = case capture of
                     Just (T.Piece _ pt) ->
                        let capturedType = fromPieceType pt
                            isPromoted = case capturedSquare of
                                           Just sq -> testBit promoted (T.unSquare (toSquare sq))
                                           Nothing -> False
                            addToPocket = if isPromoted then Pawn else capturedType
                            (wm, bm) = (wPocket, bPocket)
                        in if c == White
                           then (updatePockets wm addToPocket (+1), bm)
                           else (wm, updatePockets bm addToPocket (+1))
                     Nothing -> (wPocket, bPocket)

                  -- Update Promoted Bitboard
                  p1 = case capturedSquare of
                          Just sq -> clearBit promoted (T.unSquare (toSquare sq))
                          Nothing -> promoted

                  isMovingPromoted = testBit p1 (T.unSquare (toSquare from))
                  p2 = if isMovingPromoted
                       then setBit (clearBit p1 (T.unSquare (toSquare from))) (T.unSquare (toSquare to))
                       else p1

                  p3 = case m of
                          PromotionMove _ _ _ -> setBit p2 (T.unSquare (toSquare to))
                          PromotionCaptureMove _ _ _ _ -> setBit p2 (T.unSquare (toSquare to))
                          _ -> p2

              in (pockets', p3)

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

        resetClock = isPawn || isCapture
        newHMC = if resetClock then 0 else GS.halfmoveClock gs + 1
        newFMN = GS.fullmoveNumber gs + (if c == Black then 1 else 0)

<<<<<<< HEAD
        newGS = GS.setZobristHash
          (GS.setFullmoveNumber
            (GS.setHalfmoveClock
              (GS.setEpSquare
                (GS.setTurn gsUpdated (toColor (colorVal @(Opposite c))))
                newEP)
              newHMC)
            newFMN)
          0
=======
        newGS = gsUpdated
          { GS.turn = toColor (colorVal @(Opposite c))
          , GS.epSquare = newEP
          , GS.halfmoveClock = newHMC
          , GS.fullmoveNumber = newFMN
          , GS.zobristHash = 0
          }
>>>>>>> origin/main

        newState = CrazyhouseState wPocket' bPocket' promoted'

        nextAg = ActiveGame internalB' newGS newState SUnchecked

    in Transition nextAg

  executeMove = genericExecuteMove
