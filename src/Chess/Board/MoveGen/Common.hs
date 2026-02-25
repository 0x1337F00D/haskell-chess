{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE BangPatterns #-}

module Chess.Board.MoveGen.Common where

import Data.Bits
import Data.Word (Word64)
import Foreign.Storable (Storable)
import Control.Monad (liftM)
import Data.Coerce (coerce)

import qualified Data.Vector.Generic         as G
import qualified Data.Vector.Generic.Mutable as M
import qualified Data.Vector.Unboxed         as U

import Chess.Types

-- | A move coupled with explicit semantics, packed into a Word64.
-- Layout:
-- Bits 0-5: From Square
-- Bits 6-11: To Square
-- Bits 12-14: Tag
--    0: Quiet (Moving)
--    1: Capture (Moving, Captured)
--    2: EnPassant
--    3: Castling
--    4: Promotion (Promo)
--    5: PromotionCapture (Promo, Captured)
-- Bits 15-17: Piece 1 (Moving for Quiet/Cap, Promo for Prom/PromCap)
-- Bits 18-20: Piece 2 (Captured for Cap/PromCap)
newtype GenMove = MkGenMove Word64
  deriving (Eq, Ord)
  deriving newtype (Show, Storable)

-- Unbox Instances
newtype instance U.MVector s GenMove = MV_GenMove (U.MVector s Word64)
newtype instance U.Vector    GenMove = V_GenMove  (U.Vector    Word64)

instance U.Unbox GenMove

instance M.MVector U.MVector GenMove where
  basicLength (MV_GenMove v) = M.basicLength v
  basicUnsafeSlice i n (MV_GenMove v) = MV_GenMove (M.basicUnsafeSlice i n v)
  basicOverlaps (MV_GenMove v1) (MV_GenMove v2) = M.basicOverlaps v1 v2
  basicUnsafeNew n = MV_GenMove `liftM` M.basicUnsafeNew n
  basicInitialize (MV_GenMove v) = M.basicInitialize v
  basicUnsafeReplicate n x = MV_GenMove `liftM` M.basicUnsafeReplicate n (coerce x)
  basicUnsafeRead (MV_GenMove v) i = coerce `liftM` M.basicUnsafeRead v i
  basicUnsafeWrite (MV_GenMove v) i x = M.basicUnsafeWrite v i (coerce x)
  basicClear (MV_GenMove v) = M.basicClear v
  basicSet (MV_GenMove v) x = M.basicSet v (coerce x)
  basicUnsafeCopy (MV_GenMove v1) (MV_GenMove v2) = M.basicUnsafeCopy v1 v2
  basicUnsafeMove (MV_GenMove v1) (MV_GenMove v2) = M.basicUnsafeMove v1 v2
  basicUnsafeGrow (MV_GenMove v) n = MV_GenMove `liftM` M.basicUnsafeGrow v n

instance G.Vector U.Vector GenMove where
  basicUnsafeFreeze (MV_GenMove v) = V_GenMove `liftM` G.basicUnsafeFreeze v
  basicUnsafeThaw (V_GenMove v) = MV_GenMove `liftM` G.basicUnsafeThaw v
  basicLength (V_GenMove v) = G.basicLength v
  basicUnsafeSlice i n (V_GenMove v) = V_GenMove (G.basicUnsafeSlice i n v)
  basicUnsafeIndexM (V_GenMove v) i = coerce `liftM` G.basicUnsafeIndexM v i
  basicUnsafeCopy (MV_GenMove mv) (V_GenMove v) = G.basicUnsafeCopy mv v
  elemseq _ = seq

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

-- Helpers for packing/unpacking

mkQuiet :: Square -> Square -> PieceType -> GenMove
mkQuiet (Square f) (Square t) p = MkGenMove $
    fromIntegral f .|. (fromIntegral t `shiftL` 6) .|. (0 `shiftL` 12) .|. (fromIntegral (fromEnum p) `shiftL` 15)

unpackQuiet :: GenMove -> Maybe (Square, Square, PieceType)
unpackQuiet (MkGenMove w) =
    if (w `shiftR` 12) .&. 0x7 == 0
    then Just (Square (fromIntegral (w .&. 0x3F)), Square (fromIntegral ((w `shiftR` 6) .&. 0x3F)), toEnum (fromIntegral ((w `shiftR` 15) .&. 0x7)))
    else Nothing

mkCapture :: Square -> Square -> PieceType -> PieceType -> GenMove
mkCapture (Square f) (Square t) p c = MkGenMove $
    fromIntegral f .|. (fromIntegral t `shiftL` 6) .|. (1 `shiftL` 12) .|. (fromIntegral (fromEnum p) `shiftL` 15) .|. (fromIntegral (fromEnum c) `shiftL` 18)

unpackCapture :: GenMove -> Maybe (Square, Square, PieceType, PieceType)
unpackCapture (MkGenMove w) =
    if (w `shiftR` 12) .&. 0x7 == 1
    then Just (Square (fromIntegral (w .&. 0x3F)), Square (fromIntegral ((w `shiftR` 6) .&. 0x3F)), toEnum (fromIntegral ((w `shiftR` 15) .&. 0x7)), toEnum (fromIntegral ((w `shiftR` 18) .&. 0x7)))
    else Nothing

mkEnPassant :: Square -> Square -> GenMove
mkEnPassant (Square f) (Square t) = MkGenMove $
    fromIntegral f .|. (fromIntegral t `shiftL` 6) .|. (2 `shiftL` 12)

unpackEnPassant :: GenMove -> Maybe (Square, Square)
unpackEnPassant (MkGenMove w) =
    if (w `shiftR` 12) .&. 0x7 == 2
    then Just (Square (fromIntegral (w .&. 0x3F)), Square (fromIntegral ((w `shiftR` 6) .&. 0x3F)))
    else Nothing

mkCastling :: Square -> Square -> GenMove
mkCastling (Square f) (Square t) = MkGenMove $
    fromIntegral f .|. (fromIntegral t `shiftL` 6) .|. (3 `shiftL` 12)

unpackCastling :: GenMove -> Maybe (Square, Square)
unpackCastling (MkGenMove w) =
    if (w `shiftR` 12) .&. 0x7 == 3
    then Just (Square (fromIntegral (w .&. 0x3F)), Square (fromIntegral ((w `shiftR` 6) .&. 0x3F)))
    else Nothing

mkPromotion :: Square -> Square -> PieceType -> GenMove
mkPromotion (Square f) (Square t) p = MkGenMove $
    fromIntegral f .|. (fromIntegral t `shiftL` 6) .|. (4 `shiftL` 12) .|. (fromIntegral (fromEnum p) `shiftL` 15)

unpackPromotion :: GenMove -> Maybe (Square, Square, PieceType)
unpackPromotion (MkGenMove w) =
    if (w `shiftR` 12) .&. 0x7 == 4
    then Just (Square (fromIntegral (w .&. 0x3F)), Square (fromIntegral ((w `shiftR` 6) .&. 0x3F)), toEnum (fromIntegral ((w `shiftR` 15) .&. 0x7)))
    else Nothing

mkPromotionCapture :: Square -> Square -> PieceType -> PieceType -> GenMove
mkPromotionCapture (Square f) (Square t) p c = MkGenMove $
    fromIntegral f .|. (fromIntegral t `shiftL` 6) .|. (5 `shiftL` 12) .|. (fromIntegral (fromEnum p) `shiftL` 15) .|. (fromIntegral (fromEnum c) `shiftL` 18)

unpackPromotionCapture :: GenMove -> Maybe (Square, Square, PieceType, PieceType)
unpackPromotionCapture (MkGenMove w) =
    if (w `shiftR` 12) .&. 0x7 == 5
    then Just (Square (fromIntegral (w .&. 0x3F)), Square (fromIntegral ((w `shiftR` 6) .&. 0x3F)), toEnum (fromIntegral ((w `shiftR` 15) .&. 0x7)), toEnum (fromIntegral ((w `shiftR` 18) .&. 0x7)))
    else Nothing

mkGenDrop :: PieceType -> Square -> GenMove
mkGenDrop p (Square t) = MkGenMove $
    fromIntegral t `shiftL` 6 .|. (6 `shiftL` 12) .|. (fromIntegral (fromEnum p) `shiftL` 15)

unpackGenDrop :: GenMove -> Maybe (PieceType, Square)
unpackGenDrop (MkGenMove w) =
    if (w `shiftR` 12) .&. 0x7 == 6
    then Just (toEnum (fromIntegral ((w `shiftR` 15) .&. 0x7)), Square (fromIntegral ((w `shiftR` 6) .&. 0x3F)))
    else Nothing

mkGenCastling960 :: Square -> Square -> GenMove
mkGenCastling960 (Square f) (Square t) = MkGenMove $
    fromIntegral f .|. (fromIntegral t `shiftL` 6) .|. (7 `shiftL` 12)

unpackGenCastling960 :: GenMove -> Maybe (Square, Square)
unpackGenCastling960 (MkGenMove w) =
    if (w `shiftR` 12) .&. 0x7 == 7
    then Just (Square (fromIntegral (w .&. 0x3F)), Square (fromIntegral ((w `shiftR` 6) .&. 0x3F)))
    else Nothing

-- | Convert a GenMove back to a standard Move.
genMoveToMove :: GenMove -> Move
genMoveToMove (GenQuiet f t _) = Move f t Nothing
genMoveToMove (GenCapture f t _ _) = Move f t Nothing
genMoveToMove (GenEnPassant f t) = Move f t Nothing
genMoveToMove (GenCastling f t) = Move f t Nothing
genMoveToMove (GenPromotion f t p) = Move f t (Just p)
genMoveToMove (GenPromotionCapture f t p _) = Move f t (Just p)
genMoveToMove _ = NullMove -- Should not happen in standard chess
