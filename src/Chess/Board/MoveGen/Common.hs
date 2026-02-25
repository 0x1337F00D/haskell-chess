{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE BangPatterns #-}

module Chess.Board.MoveGen.Common
  ( GenMove
  , pattern GenQuiet
  , pattern GenCapture
  , pattern GenEnPassant
  , pattern GenCastling
  , pattern GenPromotion
  , pattern GenPromotionCapture
  , pattern GenDrop
  , pattern GenCastling960
  , genMoveToMove
  , mkQuiet
  , mkCapture
  , mkEnPassant
  , mkCastling
  , mkPromotion
  , mkPromotionCapture
  , mkGenDrop
  , mkGenCastling960
  ) where

import Chess.Types
import Chess.Board.MoveGen.Internal

-- Pattern Synonyms

pattern GenQuiet :: Square -> Square -> PieceType -> GenMove
pattern GenQuiet f t p <- (unpackQuiet -> Just (f, t, p))
  where GenQuiet f t p = mkQuiet f t p

pattern GenCapture :: Square -> Square -> PieceType -> PieceType -> GenMove
pattern GenCapture f t p c <- (unpackCapture -> Just (f, t, p, c))
  where GenCapture f t p c = mkCapture f t p c

pattern GenEnPassant :: Square -> Square -> GenMove
pattern GenEnPassant f t <- (unpackEnPassant -> Just (f, t))
  where GenEnPassant f t = mkEnPassant f t

pattern GenCastling :: Square -> Square -> GenMove
pattern GenCastling f t <- (unpackCastling -> Just (f, t))
  where GenCastling f t = mkCastling f t

pattern GenPromotion :: Square -> Square -> PieceType -> GenMove
pattern GenPromotion f t p <- (unpackPromotion -> Just (f, t, p))
  where GenPromotion f t p = mkPromotion f t p

pattern GenPromotionCapture :: Square -> Square -> PieceType -> PieceType -> GenMove
pattern GenPromotionCapture f t p c <- (unpackPromotionCapture -> Just (f, t, p, c))
  where GenPromotionCapture f t p c = mkPromotionCapture f t p c

pattern GenDrop :: PieceType -> Square -> GenMove
pattern GenDrop p t <- (unpackGenDrop -> Just (p, t))
  where GenDrop p t = mkGenDrop p t

pattern GenCastling960 :: Square -> Square -> GenMove
pattern GenCastling960 f t <- (unpackGenCastling960 -> Just (f, t))
  where GenCastling960 f t = mkGenCastling960 f t
{-# COMPLETE GenQuiet, GenCapture, GenEnPassant, GenCastling, GenPromotion, GenPromotionCapture, GenDrop, GenCastling960 #-}


-- View helpers using fast accessors
{-# INLINE unpackQuiet #-}
unpackQuiet :: GenMove -> Maybe (Square, Square, PieceType)
unpackQuiet m =
    if getTag m == tagQuiet
    then Just (getFrom m, getTo m, getP1 m)
    else Nothing

{-# INLINE unpackCapture #-}
unpackCapture :: GenMove -> Maybe (Square, Square, PieceType, PieceType)
unpackCapture m =
    if getTag m == tagCapture
    then Just (getFrom m, getTo m, getP1 m, getP2 m)
    else Nothing

{-# INLINE unpackEnPassant #-}
unpackEnPassant :: GenMove -> Maybe (Square, Square)
unpackEnPassant m =
    if getTag m == tagEnPassant
    then Just (getFrom m, getTo m)
    else Nothing

{-# INLINE unpackCastling #-}
unpackCastling :: GenMove -> Maybe (Square, Square)
unpackCastling m =
    if getTag m == tagCastling
    then Just (getFrom m, getTo m)
    else Nothing

{-# INLINE unpackPromotion #-}
unpackPromotion :: GenMove -> Maybe (Square, Square, PieceType)
unpackPromotion m =
    if getTag m == tagPromotion
    then Just (getFrom m, getTo m, getP1 m)
    else Nothing

{-# INLINE unpackPromotionCapture #-}
unpackPromotionCapture :: GenMove -> Maybe (Square, Square, PieceType, PieceType)
unpackPromotionCapture m =
    if getTag m == tagPromotionCapture
    then Just (getFrom m, getTo m, getP1 m, getP2 m)
    else Nothing

{-# INLINE unpackGenDrop #-}
unpackGenDrop :: GenMove -> Maybe (PieceType, Square)
unpackGenDrop m =
    if getTag m == tagDrop
    then Just (getP1 m, getTo m)
    else Nothing

{-# INLINE unpackGenCastling960 #-}
unpackGenCastling960 :: GenMove -> Maybe (Square, Square)
unpackGenCastling960 m =
    if getTag m == tagCastling960
    then Just (getFrom m, getTo m)
    else Nothing


-- | Convert a GenMove back to a standard Move.
genMoveToMove :: GenMove -> Move
genMoveToMove (GenQuiet f t _) = Move f t Nothing
genMoveToMove (GenCapture f t _ _) = Move f t Nothing
genMoveToMove (GenEnPassant f t) = Move f t Nothing
genMoveToMove (GenCastling f t) = Move f t Nothing
genMoveToMove (GenPromotion f t p) = Move f t (Just p)
genMoveToMove (GenPromotionCapture f t p _) = Move f t (Just p)
genMoveToMove (GenDrop p t) = DropMove p t
genMoveToMove (GenCastling960 f t) = Move f t Nothing
