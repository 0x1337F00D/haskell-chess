{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Chess.Board.MoveGen.Core where

import Data.Bits
import Data.Maybe (fromMaybe)
import Control.Monad (unless, when)
import qualified Data.Vector.Unboxed as U

import Chess.Types
import Chess.Bitboard
import Chess.Board.Base
import Chess.Board.GameState
import Chess.Internal.Builder
import Chess.Board.MoveGen.Common
import Chess.Board.MoveGen.KingSafety
import Chess.Board.MoveGen.Pawn
import Chess.Board.MoveGen.Piece
import Chess.Board.MoveGen.Castling

-- | Generate all pseudo-legal moves.
pseudoLegalMoves :: Board -> GameState -> U.Vector GenMove
pseudoLegalMoves b gs = runBuilder256 $ do
       fillPawnQuiets     b gs
       fillPawnCaptures   b gs
       fillPawnPromotions b gs
       fillPieceMoves     b gs Knight
       fillPieceMoves     b gs Bishop
       fillPieceMoves     b gs Rook
       fillPieceMoves     b gs Queen
       fillPieceMoves     b gs King
       fillCastlingMoves  b gs

-- | Generate all legal moves.
legalMoves :: Board -> GameState -> [Move]
legalMoves b gs =
    let c = turn gs
        occ = occupiedTotal b
        attackers = if not (hasKing b c) then 0 else
            let k = kingSquareFast b c
            in attackersTo b k occ .&. occupiedBy b (oppositeColor c)
    in if attackers /= 0
       then U.toList $ U.map genMoveToMove $ generateEvasions b gs
       else
           let pinned = pinnedBits b c
               pseudo = pseudoLegalMoves b gs
               step gm acc = if isLegalSafe b gs pinned gm then genMoveToMove gm : acc else acc
           in U.foldr step [] pseudo

-- | Generate all legal moves returning GenMove.
legalGenMoves :: Board -> GameState -> U.Vector GenMove
legalGenMoves b gs =
    let c = turn gs
        occ = occupiedTotal b
        attackers = if not (hasKing b c) then 0 else
            let k = kingSquareFast b c
            in attackersTo b k occ .&. occupiedBy b (oppositeColor c)
    in if attackers /= 0
       then generateEvasions b gs
       else
           let pinned = pinnedBits b c
               pseudo = pseudoLegalMoves b gs
           in U.filter (isLegalSafe b gs pinned) pseudo

-- | Generate all pseudo-legal capture moves.
pseudoLegalCaptures :: Board -> GameState -> U.Vector GenMove
pseudoLegalCaptures b gs = runBuilder256 $ do
       fillPawnCaptures   b gs
       fillPieceCaptures  b gs Knight
       fillPieceCaptures  b gs Bishop
       fillPieceCaptures  b gs Rook
       fillPieceCaptures  b gs Queen
       fillPieceCaptures  b gs King

-- | Generate all legal capture moves.
legalCaptures :: Board -> GameState -> [Move]
legalCaptures b gs =
    let c = turn gs
        occ = occupiedTotal b
        attackers = if not (hasKing b c) then 0 else
            let k = kingSquareFast b c
            in attackersTo b k occ .&. occupiedBy b (oppositeColor c)
    in if attackers /= 0
       then U.toList $ U.map genMoveToMove $ generateEvasionCaptures b gs
       else
           let pinned = pinnedBits b c
               pseudo = pseudoLegalCaptures b gs
               step gm acc = if isLegalSafe b gs pinned gm then genMoveToMove gm : acc else acc
           in U.foldr step [] pseudo

-- | Generate all legal capture moves returning GenMove.
legalGenCaptures :: Board -> GameState -> U.Vector GenMove
legalGenCaptures b gs =
    let c = turn gs
        occ = occupiedTotal b
        attackers = if not (hasKing b c) then 0 else
            let k = kingSquareFast b c
            in attackersTo b k occ .&. occupiedBy b (oppositeColor c)
    in if attackers /= 0
       then generateEvasionCaptures b gs
       else
           let pinned = pinnedBits b c
               pseudo = pseudoLegalCaptures b gs
           in U.filter (isLegalSafe b gs pinned) pseudo

-- | Generate all pseudo-legal quiet moves.
pseudoLegalQuiets :: Board -> GameState -> U.Vector GenMove
pseudoLegalQuiets b gs = runBuilder256 $ do
       fillPawnQuiets     b gs
       fillPieceQuiets    b gs Knight
       fillPieceQuiets    b gs Bishop
       fillPieceQuiets    b gs Rook
       fillPieceQuiets    b gs Queen
       fillPieceQuiets    b gs King
       fillCastlingMoves  b gs

-- | Generate all legal quiet moves returning GenMove.
legalGenQuiets :: Board -> GameState -> U.Vector GenMove
legalGenQuiets b gs =
    let c = turn gs
        occ = occupiedTotal b
        attackers = if not (hasKing b c) then 0 else
            let k = kingSquareFast b c
            in attackersTo b k occ .&. occupiedBy b (oppositeColor c)
    in if attackers /= 0
       then generateEvasionQuiets b gs
       else
           let pinned = pinnedBits b c
               pseudo = pseudoLegalQuiets b gs
           in U.filter (isLegalSafe b gs pinned) pseudo

-- | Generate all pseudo-legal promotion moves.
pseudoLegalPromotions :: Board -> GameState -> U.Vector GenMove
pseudoLegalPromotions b gs = pawnPromotions b gs

-- | Generate all legal promotion moves returning GenMove.
legalGenPromotions :: Board -> GameState -> U.Vector GenMove
legalGenPromotions b gs =
    let c = turn gs
        occ = occupiedTotal b
        attackers = if not (hasKing b c) then 0 else
            let k = kingSquareFast b c
            in attackersTo b k occ .&. occupiedBy b (oppositeColor c)
    in if attackers /= 0
       then generateEvasionPromotions b gs
       else
           let pinned = pinnedBits b c
               pseudo = pseudoLegalPromotions b gs
           in U.filter (isLegalSafe b gs pinned) pseudo

-- | Generate only legal moves when the king is in check.
generateEvasions :: Board -> GameState -> U.Vector GenMove
generateEvasions b gs = runBuilder256 $ do
    let c = turn gs
        kingSq = if hasKing b c then kingSquareFast b c else Square 0

        occ = occupiedTotal b
        enemies = occupiedBy b (oppositeColor c)

        attackers = attackersTo b kingSq occ .&. enemies

    unless (attackers == 0) $ do
        fillKingEvasions b gs (complement 0)
        -- Bolt: isSingleCheck is faster than popCount > 1. attackers is guaranteed > 0 here.
        let isSingleCheck = (attackers .&. (attackers - 1)) == 0
        when isSingleCheck $ do
             let attackerSq = Square (fromMaybe 0 (lsb attackers))
                 r = ray kingSq attackerSq
                 -- Target mask: capture attacker or block ray
                 targetMask = r .|. bbFromSquare attackerSq
                 -- Handle En Passant
                 ep = epSquare gs
                 realTargetMask = case ep of
                        NoSquare -> targetMask
                        Square e ->
                             let captureSq = if c == White then Square (e - 8) else Square (e + 8)
                             in if captureSq == attackerSq
                                then targetMask `setBit` e
                                else targetMask

             fillPawnEvasions b gs realTargetMask
             fillPieceEvasions b gs Knight realTargetMask
             fillPieceEvasions b gs Bishop realTargetMask
             fillPieceEvasions b gs Rook realTargetMask
             fillPieceEvasions b gs Queen realTargetMask

generateEvasionCaptures :: Board -> GameState -> U.Vector GenMove
generateEvasionCaptures b gs = runBuilder256 $ do
    let c = turn gs
        kingSq = if hasKing b c then kingSquareFast b c else Square 0
        occ = occupiedTotal b
        enemies = occupiedBy b (oppositeColor c)
        attackers = attackersTo b kingSq occ .&. enemies

    unless (attackers == 0) $ do
        fillKingEvasions b gs enemies
        -- Bolt: isSingleCheck is faster than popCount > 1. attackers is guaranteed > 0 here.
        let isSingleCheck = (attackers .&. (attackers - 1)) == 0
        when isSingleCheck $ do
            let attackerSq = Square (fromMaybe 0 (lsb attackers))
                targetMask = bbFromSquare attackerSq
                ep = epSquare gs
                realTargetMask = case ep of
                    NoSquare -> targetMask
                    Square e ->
                         let captureSq = if c == White then Square (e - 8) else Square (e + 8)
                         in if captureSq == attackerSq
                            then targetMask `setBit` e
                            else targetMask

            fillPawnEvasions b gs realTargetMask
            fillPieceEvasions b gs Knight realTargetMask
            fillPieceEvasions b gs Bishop realTargetMask
            fillPieceEvasions b gs Rook realTargetMask
            fillPieceEvasions b gs Queen realTargetMask

generateEvasionQuiets :: Board -> GameState -> U.Vector GenMove
generateEvasionQuiets b gs = runBuilder256 $ do
    let c = turn gs
        kingSq = if hasKing b c then kingSquareFast b c else Square 0
        occ = occupiedTotal b
        enemies = occupiedBy b (oppositeColor c)
        attackers = attackersTo b kingSq occ .&. enemies

    unless (attackers == 0) $ do
        fillKingEvasions b gs (complement enemies)
        -- Bolt: isSingleCheck is faster than popCount > 1. attackers is guaranteed > 0 here.
        let isSingleCheck = (attackers .&. (attackers - 1)) == 0
        when isSingleCheck $ do
            let attackerSq = Square (fromMaybe 0 (lsb attackers))
                r = ray kingSq attackerSq
                targetMask = r

            unless (targetMask == 0) $ do
               fillPawnEvasions b gs targetMask
               fillPieceEvasions b gs Knight targetMask
               fillPieceEvasions b gs Bishop targetMask
               fillPieceEvasions b gs Rook targetMask
               fillPieceEvasions b gs Queen targetMask

generateEvasionPromotions :: Board -> GameState -> U.Vector GenMove
generateEvasionPromotions b gs = runBuilder256 $ do
    let c = turn gs
        kingSq = if hasKing b c then kingSquareFast b c else Square 0
        occ = occupiedTotal b
        enemies = occupiedBy b (oppositeColor c)
        attackers = attackersTo b kingSq occ .&. enemies

    unless (attackers == 0) $ do
         -- Bolt: isSingleCheck is faster than popCount > 1. attackers is guaranteed > 0 here.
         let isSingleCheck = (attackers .&. (attackers - 1)) == 0
         when isSingleCheck $ do
             let attackerSq = Square (fromMaybe 0 (lsb attackers))
                 r = ray kingSq attackerSq
                 targetMask = if r == 0 then bbFromSquare attackerSq else r
             fillPawnEvasionPromotions b gs targetMask

-- | Check if there is any legal move.
hasLegalMove :: Board -> GameState -> Bool
hasLegalMove b gs =
    U.any (isLegal b gs) (pieceMoves b gs King) ||
    U.any (isLegal b gs) (pieceMoves b gs Knight) ||
    U.any (isLegal b gs) (pieceMoves b gs Bishop) ||
    U.any (isLegal b gs) (pieceMoves b gs Rook) ||
    U.any (isLegal b gs) (pieceMoves b gs Queen) ||
    U.any (isLegal b gs) (pawnMoves b gs) ||
    U.any (isLegal b gs) (castlingMoves b gs)

-- List Adapters for Core
{-# INLINE pseudoLegalMovesList #-}
pseudoLegalMovesList :: Board -> GameState -> [GenMove]
pseudoLegalMovesList b gs = U.toList (pseudoLegalMoves b gs)

{-# INLINE legalGenMovesList #-}
legalGenMovesList :: Board -> GameState -> [GenMove]
legalGenMovesList b gs =
    let c = turn gs
        occ = occupiedTotal b
        attackers = if not (hasKing b c) then 0 else
            let k = kingSquareFast b c
            in attackersTo b k occ .&. occupiedBy b (oppositeColor c)
    in if attackers /= 0
       then U.toList (generateEvasions b gs)
       else
           let pinned = pinnedBits b c
               pseudo = pseudoLegalMoves b gs
               step gm acc = if isLegalSafe b gs pinned gm then gm : acc else acc
           in U.foldr step [] pseudo

{-# INLINE legalGenCapturesList #-}
legalGenCapturesList :: Board -> GameState -> [GenMove]
legalGenCapturesList b gs =
    let c = turn gs
        occ = occupiedTotal b
        attackers = if not (hasKing b c) then 0 else
            let k = kingSquareFast b c
            in attackersTo b k occ .&. occupiedBy b (oppositeColor c)
    in if attackers /= 0
       then U.toList (generateEvasionCaptures b gs)
       else
           let pinned = pinnedBits b c
               pseudo = pseudoLegalCaptures b gs
               step gm acc = if isLegalSafe b gs pinned gm then gm : acc else acc
           in U.foldr step [] pseudo

{-# INLINE legalGenQuietsList #-}
legalGenQuietsList :: Board -> GameState -> [GenMove]
legalGenQuietsList b gs =
    let c = turn gs
        occ = occupiedTotal b
        attackers = if not (hasKing b c) then 0 else
            let k = kingSquareFast b c
            in attackersTo b k occ .&. occupiedBy b (oppositeColor c)
    in if attackers /= 0
       then U.toList (generateEvasionQuiets b gs)
       else
           let pinned = pinnedBits b c
               pseudo = pseudoLegalQuiets b gs
               step gm acc = if isLegalSafe b gs pinned gm then gm : acc else acc
           in U.foldr step [] pseudo

{-# INLINE legalGenPromotionsList #-}
legalGenPromotionsList :: Board -> GameState -> [GenMove]
legalGenPromotionsList b gs =
    let c = turn gs
        occ = occupiedTotal b
        attackers = if not (hasKing b c) then 0 else
            let k = kingSquareFast b c
            in attackersTo b k occ .&. occupiedBy b (oppositeColor c)
    in if attackers /= 0
       then U.toList (generateEvasionPromotions b gs)
       else
           let pinned = pinnedBits b c
               pseudo = pseudoLegalPromotions b gs
               step gm acc = if isLegalSafe b gs pinned gm then gm : acc else acc
           in U.foldr step [] pseudo

{-# INLINE pawnMovesList #-}
pawnMovesList :: Board -> GameState -> [GenMove]
pawnMovesList b gs = U.toList (pawnMoves b gs)

{-# INLINE pieceMovesList #-}
pieceMovesList :: Board -> GameState -> PieceType -> [GenMove]
pieceMovesList b gs pt = U.toList (pieceMoves b gs pt)

{-# INLINE castlingMovesList #-}
castlingMovesList :: Board -> GameState -> [GenMove]
castlingMovesList b gs = U.toList (castlingMoves b gs)

-- | Generate legal moves assuming the king is not in check (Safe).
-- This skips the expensive attackers check and uses isLegalSafe directly.
{-# INLINE legalGenMovesSafeVector #-}
legalGenMovesSafeVector :: Board -> GameState -> U.Vector GenMove
legalGenMovesSafeVector b gs =
    let c = turn gs
        pinned = pinnedBits b c
        pseudo = pseudoLegalMoves b gs
    in U.filter (isLegalSafe b gs pinned) pseudo

{-# INLINE legalGenCapturesSafeVector #-}
legalGenCapturesSafeVector :: Board -> GameState -> U.Vector GenMove
legalGenCapturesSafeVector b gs =
    let c = turn gs
        pinned = pinnedBits b c
        pseudo = pseudoLegalCaptures b gs
    in U.filter (isLegalSafe b gs pinned) pseudo

{-# INLINE legalGenQuietsSafeVector #-}
legalGenQuietsSafeVector :: Board -> GameState -> U.Vector GenMove
legalGenQuietsSafeVector b gs =
    let c = turn gs
        pinned = pinnedBits b c
        pseudo = pseudoLegalQuiets b gs
    in U.filter (isLegalSafe b gs pinned) pseudo

{-# INLINE legalGenPromotionsSafeVector #-}
legalGenPromotionsSafeVector :: Board -> GameState -> U.Vector GenMove
legalGenPromotionsSafeVector b gs =
    let c = turn gs
        pinned = pinnedBits b c
        pseudo = pseudoLegalPromotions b gs
    in U.filter (isLegalSafe b gs pinned) pseudo
