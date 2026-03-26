{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE BangPatterns #-}

module Chess.Board.MoveGen.Piece where

import Data.Bits
import Control.Monad (when)
import qualified Data.Vector.Unboxed as U

import Chess.Types
import Chess.Bitboard ((.&~.))
import Chess.Bitboard
import Chess.Board.Base
import Chess.Board.GameState
import Chess.Internal.Builder
import Chess.Board.MoveGen.Common
import Chess.Board.MoveGen.KingSafety

pieceMoves :: Board -> GameState -> PieceType -> U.Vector GenMove
pieceMoves b gs pt = runBuilder256 $ fillPieceMoves b gs pt

{-# INLINE fillPieceMoves #-}
fillPieceMoves :: MonadEmit GenMove m => Board -> GameState -> PieceType -> m ()
fillPieceMoves b gs pt = do
    let c       = turn gs
        bb      = pieceBitboard b c pt
        occ     = occupiedTotal b
        friends = occupiedBy b c
        oppC    = oppositeColor c
        enemies = occupiedBy b oppC

    let {-# INLINE go #-}
        go attcks = forBitboard bb $ \from -> do
            let att   = attcks from
                valid = att .&~. friends
            forBitboard valid $ \to -> do
                let toI = unSquare to
                if testBit enemies toI
                  then emit (GenCapture from to pt (findPieceType b oppC to))
                  else emit (GenQuiet   from to pt)

    case pt of
        Knight -> go knightAttacks
        Bishop -> go (`bishopAttacks` occ)
        Rook   -> go (`rookAttacks` occ)
        Queen  -> go (\from -> bishopAttacks from occ .|. rookAttacks from occ)
        King   -> go kingAttacks
        _      -> pure ()

pieceCaptures :: Board -> GameState -> PieceType -> U.Vector GenMove
pieceCaptures b gs pt = runBuilder256 $ fillPieceCaptures b gs pt

{-# INLINE fillPieceCaptures #-}
fillPieceCaptures :: MonadEmit GenMove m => Board -> GameState -> PieceType -> m ()
fillPieceCaptures b gs pt = do
    let c       = turn gs
        bb      = pieceBitboard b c pt
        occ     = occupiedTotal b
        oppC    = oppositeColor c
        enemies = occupiedBy b oppC

    let {-# INLINE go #-}
        go attcks = forBitboard bb $ \from -> do
            let att   = attcks from
                valid = att .&. enemies
            forBitboard valid $ \to -> do
                emit (GenCapture from to pt (findPieceType b oppC to))

    case pt of
        Knight -> go knightAttacks
        Bishop -> go (`bishopAttacks` occ)
        Rook   -> go (`rookAttacks` occ)
        Queen  -> go (\from -> bishopAttacks from occ .|. rookAttacks from occ)
        King   -> go kingAttacks
        _      -> pure ()

pieceQuiets :: Board -> GameState -> PieceType -> U.Vector GenMove
pieceQuiets b gs pt = runBuilder256 $ fillPieceQuiets b gs pt

{-# INLINE fillPieceQuiets #-}
fillPieceQuiets :: MonadEmit GenMove m => Board -> GameState -> PieceType -> m ()
fillPieceQuiets b gs pt = do
    let c       = turn gs
        bb      = pieceBitboard b c pt
        occ     = occupiedTotal b

    let {-# INLINE go #-}
        go attcks = forBitboard bb $ \from -> do
            let att   = attcks from
                valid = att .&~. occ
            forBitboard valid $ \to -> do
                emit (GenQuiet from to pt)

    case pt of
        Knight -> go knightAttacks
        Bishop -> go (`bishopAttacks` occ)
        Rook   -> go (`rookAttacks` occ)
        Queen  -> go (\from -> bishopAttacks from occ .|. rookAttacks from occ)
        King   -> go kingAttacks
        _      -> pure ()

{-# INLINE fillKingEvasions #-}
fillKingEvasions :: MonadEmit GenMove m => Board -> GameState -> Bitboard -> m ()
fillKingEvasions b gs targetMask = do
    let c       = turn gs
        bb      = pieceBitboard b c King
        friends = occupiedBy b c
        oppC    = oppositeColor c
        enemies = occupiedBy b oppC

    forBitboard bb $ \from -> do
            let att = kingAttacks from
            let valid = att .&~. friends .&. targetMask
            forBitboard valid $ \to -> do
                    let toI = unSquare to
                    let isCap = testBit enemies toI
                    let gm = if isCap
                             then GenCapture from to King (findPieceType b oppC to)
                             else GenQuiet from to King

                    -- Check legality
                    when (isLegal b gs gm) $ emit gm

{-# INLINE fillPieceEvasions #-}
fillPieceEvasions :: MonadEmit GenMove m => Board -> GameState -> PieceType -> Bitboard -> m ()
fillPieceEvasions b gs pt targetMask = do
    let c       = turn gs
        bb      = pieceBitboard b c pt
        occ     = occupiedTotal b
        friends = occupiedBy b c
        oppC    = oppositeColor c
        enemies = occupiedBy b oppC

    let {-# INLINE go #-}
        go attcks = forBitboard bb $ \from -> do
            let att = attcks from
            let valid = att .&~. friends .&. targetMask
            forBitboard valid $ \to -> do
                    let toI = unSquare to
                    let isCap = testBit enemies toI
                    let gm = if isCap
                             then GenCapture from to pt (findPieceType b oppC to)
                             else GenQuiet from to pt

                    when (isLegal b gs gm) $ emit gm

    case pt of
        Knight -> go knightAttacks
        Bishop -> go (`bishopAttacks` occ)
        Rook   -> go (`rookAttacks` occ)
        Queen  -> go (\from -> bishopAttacks from occ .|. rookAttacks from occ)
        _      -> pure ()
