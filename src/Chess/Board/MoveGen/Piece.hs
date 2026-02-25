{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE BangPatterns #-}

module Chess.Board.MoveGen.Piece where

import Data.Bits
import Control.Monad (when)
import qualified Data.Vector.Unboxed as U

import Chess.Types
import Chess.Bitboard
import Chess.Board.Base
import Chess.Board.GameState
import Chess.Internal.Builder
import Chess.Board.MoveGen.Common
import Chess.Board.MoveGen.KingSafety

pieceMoves :: Board -> GameState -> PieceType -> U.Vector GenMove
pieceMoves b gs pt = runBuilder256 $ fillPieceMoves b gs pt

{-# INLINE fillPieceMoves #-}
fillPieceMoves :: Board -> GameState -> PieceType -> Builder s GenMove ()
fillPieceMoves b gs pt = do
    let c = turn gs
        bb = pieceBitboard b c pt
        occ = occupiedTotal b
        friends = occupiedBy b c
        enemies = occupiedBy b (oppositeColor c)
        oppC = oppositeColor c
        getAttacks from = case pt of
             Knight -> knightAttacks from
             Bishop -> bishopAttacks from occ
             Rook   -> rookAttacks from occ
             Queen  -> bishopAttacks from occ .|. rookAttacks from occ
             King   -> kingAttacks from
             _      -> 0
    forBitboard bb $ \from -> do
            let att = getAttacks from
            let valid = att .&. complement friends
            forBitboard valid $ \to -> do
                    let toI = unSquare to
                    let isCap = testBit enemies toI
                    let gm = if isCap
                             then GenCapture from to pt (findPieceType b oppC to)
                             else GenQuiet from to pt
                    emit gm

pieceCaptures :: Board -> GameState -> PieceType -> U.Vector GenMove
pieceCaptures b gs pt = runBuilder256 $ fillPieceCaptures b gs pt

{-# INLINE fillPieceCaptures #-}
fillPieceCaptures :: Board -> GameState -> PieceType -> Builder s GenMove ()
fillPieceCaptures b gs pt = do
    let c = turn gs
        bb = pieceBitboard b c pt
        occ = occupiedTotal b
        enemies = occupiedBy b (oppositeColor c)
        oppC = oppositeColor c
        getAttacks from = case pt of
             Knight -> knightAttacks from
             Bishop -> bishopAttacks from occ
             Rook   -> rookAttacks from occ
             Queen  -> bishopAttacks from occ .|. rookAttacks from occ
             King   -> kingAttacks from
             _      -> 0
    forBitboard bb $ \from -> do
            let att = getAttacks from
            let valid = att .&. enemies
            forBitboard valid $ \to -> do
                    let gm = GenCapture from to pt (findPieceType b oppC to)
                    emit gm

pieceQuiets :: Board -> GameState -> PieceType -> U.Vector GenMove
pieceQuiets b gs pt = runBuilder256 $ fillPieceQuiets b gs pt

{-# INLINE fillPieceQuiets #-}
fillPieceQuiets :: Board -> GameState -> PieceType -> Builder s GenMove ()
fillPieceQuiets b gs pt = do
    let c = turn gs
        bb = pieceBitboard b c pt
        occ = occupiedTotal b
        getAttacks from = case pt of
             Knight -> knightAttacks from
             Bishop -> bishopAttacks from occ
             Rook   -> rookAttacks from occ
             Queen  -> bishopAttacks from occ .|. rookAttacks from occ
             King   -> kingAttacks from
             _      -> 0
    forBitboard bb $ \from -> do
            let att = getAttacks from
            let valid = att .&. complement occ
            forBitboard valid $ \to -> do
                    emit (GenQuiet from to pt)

{-# INLINE fillKingEvasions #-}
fillKingEvasions :: Board -> GameState -> Bitboard -> Builder s GenMove ()
fillKingEvasions b gs targetMask = do
    let c = turn gs
        bb = pieceBitboard b c King
        friends = occupiedBy b c
    forBitboard bb $ \from -> do
            let att = kingAttacks from
            let valid = att .&. complement friends .&. targetMask
            forBitboard valid $ \to -> do
                    let toI = unSquare to
                    let isCap = testBit (occupiedBy b (oppositeColor c)) toI
                    let gm = if isCap
                             then GenCapture from to King (findPieceType b (oppositeColor c) to)
                             else GenQuiet from to King

                    -- Check legality
                    when (isLegal b gs gm) $ emit gm

{-# INLINE fillPieceEvasions #-}
fillPieceEvasions :: Board -> GameState -> PieceType -> Bitboard -> Builder s GenMove ()
fillPieceEvasions b gs pt targetMask = do
    let c = turn gs
        bb = pieceBitboard b c pt
        occ = occupiedTotal b
        friends = occupiedBy b c
        enemies = occupiedBy b (oppositeColor c)
        oppC = oppositeColor c
        getAttacks from = case pt of
             Knight -> knightAttacks from
             Bishop -> bishopAttacks from occ
             Rook   -> rookAttacks from occ
             Queen  -> bishopAttacks from occ .|. rookAttacks from occ
             _      -> 0
    forBitboard bb $ \from -> do
            let att = getAttacks from
            let valid = att .&. complement friends .&. targetMask
            forBitboard valid $ \to -> do
                    let toI = unSquare to
                    let isCap = testBit enemies toI
                    let gm = if isCap
                             then GenCapture from to pt (findPieceType b oppC to)
                             else GenQuiet from to pt

                    when (isLegal b gs gm) $ emit gm
