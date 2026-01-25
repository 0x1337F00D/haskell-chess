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
import Data.Bits (testBit, countTrailingZeros, (.|.), (.&.), complement, setBit)
import Data.List (find)
import Data.Maybe (mapMaybe)

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

      -- Map to Core.CastlingRights
      -- We need to know King position to distinguish K-side vs Q-side
      wKing = MG.kingSquare baseBoard T.White
      bKing = MG.kingSquare baseBoard T.Black

      -- Helper to determine rights
      getRights kingSq rooksBB =
        let kFile = case kingSq of Just (T.Square i) -> i `mod` 8; Nothing -> 4
            rooks = BB.scanForward rooksBB
            hasKSide = any (\i -> (i `mod` 8) > kFile) rooks
            hasQSide = any (\i -> (i `mod` 8) < kFile) rooks
        in (hasKSide, hasQSide)

      (wK, wQ) = getRights wKing whiteRooks
      (bK, bQ) = getRights bKing blackRooks

      crVal = (if wK then castlingWhiteKingSide else 0) .|.
              (if wQ then castlingWhiteQueenSide else 0) .|.
              (if bK then castlingBlackKingSide else 0) .|.
              (if bQ then castlingBlackQueenSide else 0)

      cr = CastlingRights crVal

      ep = case GS.epSquare gs of
             Nothing -> Nothing
             Just sq -> Just (getFile (fromBaseSquare sq))

      hmc = GS.halfmoveClock gs
      fmn = GS.fullmoveNumber gs

      checked = Val.isCheck baseBoard gs

      -- Check for moves using 960 generator
      -- We need to construct a temp ActiveGame to call generateMoves
      hasMoves = case c of
        White -> not (null (generateMoves (ActiveGame baseBoard cr ep hmc fmn frState SSafe :: ActiveGame 'FischerRandom 'White 'Safe)))
        Black -> not (null (generateMoves (ActiveGame baseBoard cr ep hmc fmn frState SSafe :: ActiveGame 'FischerRandom 'Black 'Safe)))

  if hasMoves
    then case c of
      White -> if checked
               then return $ InProgressGame (ActiveGame baseBoard cr ep hmc fmn frState SChecked :: ActiveGame 'FischerRandom 'White 'Checked)
               else return $ InProgressGame (ActiveGame baseBoard cr ep hmc fmn frState SSafe    :: ActiveGame 'FischerRandom 'White 'Safe)
      Black -> if checked
               then return $ InProgressGame (ActiveGame baseBoard cr ep hmc fmn frState SChecked :: ActiveGame 'FischerRandom 'Black 'Checked)
               else return $ InProgressGame (ActiveGame baseBoard cr ep hmc fmn frState SSafe    :: ActiveGame 'FischerRandom 'Black 'Safe)
    else Nothing

instance ChessVariant 'FischerRandom where
  generateMoves (ag :: ActiveGame 'FischerRandom c s) =
    let baseBoard = internalBoard ag
        gs = toGameState ag
        c = colorVal @c

        baseMoves = MG.legalGenMoves baseBoard gs
        standardMoves = map toCoreMove baseMoves
        -- Filter out standard CastlingMoves
        nonCastlingMoves = filter (not . isStandardCastling) standardMoves

        isStandardCastling (CastlingMove _ _) = True
        isStandardCastling _ = False

        -- Generate 960 Castling Moves
        frState = variantState ag
        rooksBB = if c == White then whiteRookFiles frState else blackRookFiles frState

        kingSq = case MG.kingSquare baseBoard (toColor c) of
                   Just sq -> sq
                   Nothing -> T.Square 0

        cr = castlingRights ag

        -- Check KingSide
        canK = case c of
                 White -> testBit ((\(CastlingRights x) -> x) cr) 0
                 Black -> testBit ((\(CastlingRights x) -> x) cr) 2

        -- Check QueenSide
        canQ = case c of
                 White -> testBit ((\(CastlingRights x) -> x) cr) 1
                 Black -> testBit ((\(CastlingRights x) -> x) cr) 3

        kFile = T.squareFile kingSq
        kRank = T.squareRank kingSq

        -- Get eligible rooks
        rooks = BB.mapBitboard id rooksBB

        genCastling rSq =
            let rFile = T.squareFile rSq
                rRank = T.squareRank rSq
                isKSide = rFile > kFile
                allowed = if isKSide then canK else canQ
            in if allowed && rRank == kRank
               then if isCastlingValid baseBoard (toColor c) kingSq rSq isKSide
                    then [Castling960Move (fromSquare kingSq) (fromSquare rSq)]
                    else []
               else []

        castling960Moves = concatMap genCastling rooks

    in nonCastlingMoves ++ castling960Moves

  executeMove (m :: Move c) (ag :: ActiveGame 'FischerRandom c s) =
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
                       Castling960Move f _ -> (f, f)

        newCR = case m of
                  Castling960Move _ _ ->
                     let mask = if c == White
                                then complement (castlingWhiteKingSide .|. castlingWhiteQueenSide)
                                else complement (castlingBlackKingSide .|. castlingBlackQueenSide)
                         (CastlingRights old) = castlingRights ag
                     in CastlingRights (old .&. mask)
                  _ ->
                     updateCastlingRights960 (castlingRights ag) (variantState ag) internalB from to c

        movedPiece = Base.pieceAt internalB' (toSquare to)

        isPawn = case m of
                   StandardMove _ _ ->
                       case Base.pieceAt internalB' (toSquare to) of
                           Just p -> T.pieceType p == T.Pawn
                           _ -> False
                   _ -> False

        newEP = case m of
                  StandardMove f t -> if isPawn && isDoublePush f t then Just (getFile f) else Nothing
                  _ -> Nothing

        newHMC = halfMoveClock ag + 1
        newFMN = fullMoveNumber ag + (if c == Black then 1 else 0)

        baseBoard = internalB'

        frState = variantState ag

        nextTurnGS = GS.GameState
          { GS.turn = toColor (colorVal @(Opposite c))
          , GS.castlingRights = toCastlingRights newCR
          , GS.epSquare = case newEP of
                            Nothing -> Nothing
                            Just f -> Just (toSquare (Square f (epRank (colorVal @(Opposite c)))))
          , GS.halfmoveClock = newHMC
          , GS.fullmoveNumber = newFMN
          }

        nextAg = ActiveGame internalB' newCR newEP newHMC newFMN frState SSafe :: ActiveGame 'FischerRandom (Opposite c) 'Safe

        isChecked = Val.isCheck baseBoard nextTurnGS

        legalMoves = if isChecked
                     then generateMoves (ActiveGame internalB' newCR newEP newHMC newFMN frState SChecked :: ActiveGame 'FischerRandom (Opposite c) 'Checked)
                     else generateMoves nextAg
        hasMoves = not (null legalMoves)

    in case (isChecked, hasMoves) of
         (True, False) -> Checkmate (Winner c)
         (False, False) -> Stalemate
         (True, True) -> Continue (ActiveGame internalB' newCR newEP newHMC newFMN frState SChecked :: ActiveGame 'FischerRandom (Opposite c) 'Checked)
         (False, True) -> Continue (ActiveGame internalB' newCR newEP newHMC newFMN frState SSafe    :: ActiveGame 'FischerRandom (Opposite c) 'Safe)

-- Helper to validate 960 castling
isCastlingValid :: Base.Board -> T.Color -> T.Square -> T.Square -> Bool -> Bool
isCastlingValid b c kSq rSq isKSide =
    let rank = T.squareRank kSq
        kFile = T.squareFile kSq
        rFile = T.squareFile rSq

        -- Target squares
        destKFile = if isKSide then 6 else 2 -- G or C
        destRFile = if isKSide then 5 else 3 -- F or D

        -- Path 1: Between King and Rook (exclusive) must be empty
        start = min kFile rFile
        end = max kFile rFile
        path1 = [ T.Square (rank * 8 + f) | f <- [start+1 .. end-1] ]

        -- Path 2: Between King and Dest (inclusive) must be empty (except K and R) and safe
        startK = min kFile destKFile
        endK = max kFile destKFile
        pathK = [ T.Square (rank * 8 + f) | f <- [startK .. endK] ]

        -- Path 3: Between Rook and Dest (inclusive) must be empty (except K and R)
        startR = min rFile destRFile
        endR = max rFile destRFile
        pathR = [ T.Square (rank * 8 + f) | f <- [startR .. endR] ]

        allSquares = path1 ++ pathK ++ pathR

        -- Filter out K and R starting squares
        checkSquares = filter (\s -> s /= kSq && s /= rSq) allSquares

        isEmpty s = not (testBit (Base.occupiedTotal b) (T.unSquare s))

        -- King must not be in check (start), pass through check (pathK), or end in check (endK).
        -- MoveGen legalMoves checks start check.
        -- We check pathK.
        safeCheckSquares = pathK

        oppC = Base.oppositeColor c
        isSafe s = not (Base.isAttackedBy b oppC s)

    in all isEmpty checkSquares && all isSafe safeCheckSquares

-- Custom updateCastlingRights for 960
updateCastlingRights960 :: CastlingRights -> FischerRandomState -> Base.Board -> Square -> Square -> Color -> CastlingRights
updateCastlingRights960 (CastlingRights cr) state b from to c =
    let
        whiteRooks = whiteRookFiles state
        blackRooks = blackRookFiles state

        -- Check if a square corresponds to a rook with rights
        isRook s cc = testBit (if cc == White then whiteRooks else blackRooks) (T.unSquare (toSquare s))

        -- King positions (before move)
        wKing = MG.kingSquare b T.White
        bKing = MG.kingSquare b T.Black

        myKing = if c == White then wKing else bKing
        oppKing = if c == White then bKing else wKing

        -- Helper to clear bit
        clearSide kSq rSq maskK maskQ rights =
             case kSq of
               Just k ->
                 let kFile = T.squareFile k
                     rFile = T.squareFile (toSquare rSq)
                 in if rFile > kFile then rights .&. complement maskK
                    else if rFile < kFile then rights .&. complement maskQ
                    else rights
               Nothing -> rights

        -- Update logic
        cr1 = if T.pieceType (T.Piece (toColor c) T.King) == T.King && (case Base.pieceAt b (toSquare from) of Just p -> T.pieceType p == T.King; _ -> False)
              then
                 if c == White
                 then cr .&. complement (castlingWhiteKingSide .|. castlingWhiteQueenSide)
                 else cr .&. complement (castlingBlackKingSide .|. castlingBlackQueenSide)
              else cr

        cr2 = if isRook from c
              then if c == White
                   then clearSide wKing from castlingWhiteKingSide castlingWhiteQueenSide cr1
                   else clearSide bKing from castlingBlackKingSide castlingBlackQueenSide cr1
              else cr1

        oppC = if c == White then Black else White
        cr3 = if isRook to oppC
              then if c == White
                   then clearSide bKing to castlingBlackKingSide castlingBlackQueenSide cr2
                   else clearSide wKing to castlingWhiteKingSide castlingWhiteQueenSide cr2
              else cr2

    in CastlingRights cr3
