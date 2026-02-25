{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE BangPatterns #-}

module Chess.Board.MoveGen.Pawn where

import Data.Bits
import Control.Monad (when, unless)
import qualified Data.Vector.Unboxed as U

import Chess.Types
import Chess.Bitboard
import Chess.Board.Base
import Chess.Board.GameState
import Chess.Internal.Builder
import Chess.Board.MoveGen.Common
import Chess.Board.MoveGen.KingSafety

pawnMoves :: Board -> GameState -> U.Vector GenMove
pawnMoves b gs = runBuilder256 $ do
       fillPawnQuiets     b gs
       fillPawnCaptures   b gs
       fillPawnPromotions b gs

pawnQuiets :: Board -> GameState -> U.Vector GenMove
pawnQuiets b gs = runBuilder256 $ fillPawnQuiets b gs

{-# INLINE fillPawnQuiets #-}
fillPawnQuiets :: Board -> GameState -> Builder s GenMove ()
fillPawnQuiets b gs = do
    let c = turn gs
        pawns = pieceBitboard b c Pawn
        occ = occupiedTotal b
    forBitboard pawns $ \from -> do
            let i = unSquare from
            if c == White
               then do
                   let to8 = i + 8
                   unless (testBit occ to8) $ do
                          unless (to8 >= 56) $ do
                                      emit (GenQuiet from (Square to8) Pawn)

                          let to16 = i + 16
                          when (i >= 8 && i <= 15 && not (testBit occ to16)) $ do
                              emit (GenQuiet from (Square to16) Pawn)
               else do
                   let to8 = i - 8
                   unless (testBit occ to8) $ do
                          unless (to8 <= 7) $ do
                                      emit (GenQuiet from (Square to8) Pawn)

                          let to16 = i - 16
                          when (i >= 48 && i <= 55 && not (testBit occ to16)) $ do
                              emit (GenQuiet from (Square to16) Pawn)

pawnPromotions :: Board -> GameState -> U.Vector GenMove
pawnPromotions b gs = runBuilder256 $ fillPawnPromotions b gs

{-# INLINE fillPawnPromotions #-}
fillPawnPromotions :: Board -> GameState -> Builder s GenMove ()
fillPawnPromotions b gs = do
    let c = turn gs
        pawns = pieceBitboard b c Pawn
        occ = occupiedTotal b
    forBitboard pawns $ \from -> do
            let i = unSquare from
            if c == White
               then do
                   let to8 = i + 8
                       dest = Square to8
                   when (not (testBit occ to8) && to8 >= 56) $ do
                          emit (GenPromotion from dest Queen)
                          emit (GenPromotion from dest Rook)
                          emit (GenPromotion from dest Bishop)
                          emit (GenPromotion from dest Knight)
               else do
                   let to8 = i - 8
                       dest = Square to8
                   when (not (testBit occ to8) && to8 <= 7) $ do
                          emit (GenPromotion from dest Queen)
                          emit (GenPromotion from dest Rook)
                          emit (GenPromotion from dest Bishop)
                          emit (GenPromotion from dest Knight)

pawnCaptures :: Board -> GameState -> U.Vector GenMove
pawnCaptures b gs = runBuilder256 $ fillPawnCaptures b gs

{-# INLINE fillPawnCaptures #-}
fillPawnCaptures :: Board -> GameState -> Builder s GenMove ()
fillPawnCaptures b gs = do
    let c = turn gs
        pawns = pieceBitboard b c Pawn
        enemy = occupiedBy b (oppositeColor c)
        oppC = oppositeColor c
        ep = epSquare gs
        epIdx = unSquare ep

    forBitboard pawns $ \from -> do
            let i = unSquare from
            if c == White then do
                -- EP
                when (ep /= NoSquare) $ do
                     if (i + 7) == epIdx && (i `mod` 8) /= 0
                     then emit (GenEnPassant from ep)
                     else when ((i + 9) == epIdx && (i `mod` 8) /= 7) $ do
                          emit (GenEnPassant from ep)

                -- Right Capture (i+9)
                when ((i `mod` 8) /= 7) $ do
                        let to9 = i + 9
                        when (testBit enemy to9) $ do
                                let dest = Square to9
                                    capPt = findPieceType b oppC dest
                                if to9 >= 56
                                then do
                                    emit (GenPromotionCapture from dest Queen capPt)
                                    emit (GenPromotionCapture from dest Rook capPt)
                                    emit (GenPromotionCapture from dest Bishop capPt)
                                    emit (GenPromotionCapture from dest Knight capPt)
                                else do
                                    emit (GenCapture from dest Pawn capPt)

                -- Left Capture (i+7)
                when ((i `mod` 8) /= 0) $ do
                        let to7 = i + 7
                        when (testBit enemy to7) $ do
                                let dest = Square to7
                                    capPt = findPieceType b oppC dest
                                if to7 >= 56
                                then do
                                    emit (GenPromotionCapture from dest Queen capPt)
                                    emit (GenPromotionCapture from dest Rook capPt)
                                    emit (GenPromotionCapture from dest Bishop capPt)
                                    emit (GenPromotionCapture from dest Knight capPt)
                                else do
                                    emit (GenCapture from dest Pawn capPt)

            else do -- Black
                -- EP
                when (ep /= NoSquare) $ do
                     if (i - 9) == epIdx && (i `mod` 8) /= 0
                     then emit (GenEnPassant from ep)
                     else when ((i - 7) == epIdx && (i `mod` 8) /= 7) $ do
                          emit (GenEnPassant from ep)

                -- Right Capture (i-7)
                when ((i `mod` 8) /= 7) $ do
                        let to7 = i - 7
                        when (testBit enemy to7) $ do
                                let dest = Square to7
                                    capPt = findPieceType b oppC dest
                                if to7 <= 7
                                then do
                                    emit (GenPromotionCapture from dest Queen capPt)
                                    emit (GenPromotionCapture from dest Rook capPt)
                                    emit (GenPromotionCapture from dest Bishop capPt)
                                    emit (GenPromotionCapture from dest Knight capPt)
                                else do
                                    emit (GenCapture from dest Pawn capPt)

                -- Left Capture (i-9)
                when ((i `mod` 8) /= 0) $ do
                        let to9 = i - 9
                        when (testBit enemy to9) $ do
                                let dest = Square to9
                                    capPt = findPieceType b oppC dest
                                if to9 <= 7
                                then do
                                    emit (GenPromotionCapture from dest Queen capPt)
                                    emit (GenPromotionCapture from dest Rook capPt)
                                    emit (GenPromotionCapture from dest Bishop capPt)
                                    emit (GenPromotionCapture from dest Knight capPt)
                                else do
                                    emit (GenCapture from dest Pawn capPt)

{-# INLINE fillPawnEvasionPromotions #-}
fillPawnEvasionPromotions :: Board -> GameState -> Bitboard -> Builder s GenMove ()
fillPawnEvasionPromotions b gs targetMask = do
    let c = turn gs
        pawns = pieceBitboard b c Pawn
        occ = occupiedTotal b
        enemy = occupiedBy b (oppositeColor c)
        oppC = oppositeColor c

    forBitboard pawns $ \from -> do
            let i = unSquare from
            -- Quiets (Push)
            if c == White
               then do
                   let to8 = i + 8
                   when (to8 >= 56 && not (testBit occ to8) && testBit targetMask to8) $ do
                       let dest = Square to8
                       when (isLegal b gs (GenPromotion from dest Queen)) $ do
                           emit (GenPromotion from dest Queen)
                           emit (GenPromotion from dest Rook)
                           emit (GenPromotion from dest Bishop)
                           emit (GenPromotion from dest Knight)
               else do
                   let to8 = i - 8
                   when (to8 <= 7 && not (testBit occ to8) && testBit targetMask to8) $ do
                       let dest = Square to8
                       when (isLegal b gs (GenPromotion from dest Queen)) $ do
                           emit (GenPromotion from dest Queen)
                           emit (GenPromotion from dest Rook)
                           emit (GenPromotion from dest Bishop)
                           emit (GenPromotion from dest Knight)

            -- Captures
            let checkCapture toSq = do
                    when (testBit enemy (unSquare toSq) && testBit targetMask (unSquare toSq)) $ do
                        let dest = toSq
                        if unSquare dest >= 56 || unSquare dest <= 7
                        then do -- Promotion Capture
                            let capPt = findPieceType b oppC dest
                            when (isLegal b gs (GenPromotionCapture from dest Queen capPt)) $ do
                                emit (GenPromotionCapture from dest Queen capPt)
                                emit (GenPromotionCapture from dest Rook capPt)
                                emit (GenPromotionCapture from dest Bishop capPt)
                                emit (GenPromotionCapture from dest Knight capPt)
                        else return ()

            if c == White
            then do
                when ((i `mod` 8) /= 7) $ checkCapture (Square (i+9))
                when ((i `mod` 8) /= 0) $ checkCapture (Square (i+7))
            else do
                when ((i `mod` 8) /= 7) $ checkCapture (Square (i-7))
                when ((i `mod` 8) /= 0) $ checkCapture (Square (i-9))


{-# INLINE fillPawnEvasions #-}
fillPawnEvasions :: Board -> GameState -> Bitboard -> Builder s GenMove ()
fillPawnEvasions b gs targetMask = do
    let c = turn gs
        pawns = pieceBitboard b c Pawn
        occ = occupiedTotal b
        enemies = occupiedBy b (oppositeColor c)
        oppC = oppositeColor c
        ep = epSquare gs

    forBitboard pawns $ \from -> do
            let i = unSquare from
            -- Quiets
            if c == White
               then do
                   let to8 = i + 8
                   when (to8 < 64 && not (testBit occ to8) && testBit targetMask to8) $ do
                       -- Promotion?
                       if to8 >= 56
                       then do
                           let dest = Square to8
                           when (isLegal b gs (GenPromotion from dest Queen)) $ do
                               emit (GenPromotion from dest Queen)
                               emit (GenPromotion from dest Rook)
                               emit (GenPromotion from dest Bishop)
                               emit (GenPromotion from dest Knight)
                       else do
                           let gm = GenQuiet from (Square to8) Pawn
                           when (isLegal b gs gm) $ emit gm
               else do
                   let to8 = i - 8
                   when (to8 >= 0 && not (testBit occ to8) && testBit targetMask to8) $ do
                       if to8 <= 7
                       then do
                           let dest = Square to8
                           when (isLegal b gs (GenPromotion from dest Queen)) $ do
                               emit (GenPromotion from dest Queen)
                               emit (GenPromotion from dest Rook)
                               emit (GenPromotion from dest Bishop)
                               emit (GenPromotion from dest Knight)
                       else do
                           let gm = GenQuiet from (Square to8) Pawn
                           when (isLegal b gs gm) $ emit gm

            -- Double Push
            if c == White
               then do
                   let to8 = i + 8
                       to16 = i + 16
                   when (i >= 8 && i <= 15 && not (testBit occ to8) && not (testBit occ to16) && testBit targetMask to16) $ do
                       let gm = GenQuiet from (Square to16) Pawn
                       when (isLegal b gs gm) $ emit gm
               else do
                   let to8 = i - 8
                   let to16 = i - 16
                   when (i >= 48 && i <= 55 && not (testBit occ to8) && not (testBit occ to16) && testBit targetMask to16) $ do
                       let gm = GenQuiet from (Square to16) Pawn
                       when (isLegal b gs gm) $ emit gm

            -- Captures
            let checkCapture toSq = do
                    if testBit enemies (unSquare toSq) && testBit targetMask (unSquare toSq)
                    then do
                        let dest = toSq
                            capPt = findPieceType b oppC dest
                        if unSquare dest >= 56 || unSquare dest <= 7
                        then do -- Promotion Capture
                            when (isLegal b gs (GenPromotionCapture from dest Queen capPt)) $ do
                                emit (GenPromotionCapture from dest Queen capPt)
                                emit (GenPromotionCapture from dest Rook capPt)
                                emit (GenPromotionCapture from dest Bishop capPt)
                                emit (GenPromotionCapture from dest Knight capPt)
                        else do
                            let gm = GenCapture from dest Pawn capPt
                            when (isLegal b gs gm) $ emit gm
                    else when (ep /= NoSquare && toSq == ep && testBit targetMask (unSquare toSq)) $ do
                             let gm = GenEnPassant from ep
                             when (isLegal b gs gm) $ emit gm

            if c == White
            then do
                when ((i `mod` 8) /= 7) $ checkCapture (Square (i+9))
                when ((i `mod` 8) /= 0) $ checkCapture (Square (i+7))
            else do
                when ((i `mod` 8) /= 7) $ checkCapture (Square (i-7))
                when ((i `mod` 8) /= 0) $ checkCapture (Square (i-9))
