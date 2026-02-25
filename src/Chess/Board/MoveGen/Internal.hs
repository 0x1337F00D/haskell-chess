{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE BangPatterns #-}

module Chess.Board.MoveGen.Internal where

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
--    0: Quiet
--    1: Capture
--    2: EnPassant
--    3: Castling
--    4: Promotion
--    5: PromotionCapture
--    6: Drop
--    7: Castling960
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

-- Tags
tagQuiet, tagCapture, tagEnPassant, tagCastling, tagPromotion, tagPromotionCapture, tagDrop, tagCastling960 :: Word64
tagQuiet = 0
tagCapture = 1
tagEnPassant = 2
tagCastling = 3
tagPromotion = 4
tagPromotionCapture = 5
tagDrop = 6
tagCastling960 = 7

-- Extractors
{-# INLINE getTag #-}
getTag :: GenMove -> Word64
getTag (MkGenMove w) = (w `shiftR` 12) .&. 0x7

{-# INLINE getFrom #-}
getFrom :: GenMove -> Square
getFrom (MkGenMove w) = Square (fromIntegral (w .&. 0x3F))

{-# INLINE getTo #-}
getTo :: GenMove -> Square
getTo (MkGenMove w) = Square (fromIntegral ((w `shiftR` 6) .&. 0x3F))

{-# INLINE getP1 #-}
getP1 :: GenMove -> PieceType
getP1 (MkGenMove w) = toEnum (fromIntegral ((w `shiftR` 15) .&. 0x7))

{-# INLINE getP2 #-}
getP2 :: GenMove -> PieceType
getP2 (MkGenMove w) = toEnum (fromIntegral ((w `shiftR` 18) .&. 0x7))

-- Generic Packers
{-# INLINE pack0 #-}
pack0 :: Word64 -> Square -> Square -> GenMove
pack0 tag (Square f) (Square t) = MkGenMove $
  fromIntegral f .|. (fromIntegral t `shiftL` 6) .|. (tag `shiftL` 12)

{-# INLINE pack1 #-}
pack1 :: Word64 -> Square -> Square -> PieceType -> GenMove
pack1 tag (Square f) (Square t) p = MkGenMove $
  fromIntegral f .|. (fromIntegral t `shiftL` 6) .|. (tag `shiftL` 12) .|. (fromIntegral (fromEnum p) `shiftL` 15)

{-# INLINE pack2 #-}
pack2 :: Word64 -> Square -> Square -> PieceType -> PieceType -> GenMove
pack2 tag (Square f) (Square t) p c = MkGenMove $
  fromIntegral f .|. (fromIntegral t `shiftL` 6) .|. (tag `shiftL` 12) .|. (fromIntegral (fromEnum p) `shiftL` 15) .|. (fromIntegral (fromEnum c) `shiftL` 18)

-- Specific Packers (Wrappers)
{-# INLINE mkQuiet #-}
mkQuiet :: Square -> Square -> PieceType -> GenMove
mkQuiet = pack1 tagQuiet

{-# INLINE mkCapture #-}
mkCapture :: Square -> Square -> PieceType -> PieceType -> GenMove
mkCapture = pack2 tagCapture

{-# INLINE mkEnPassant #-}
mkEnPassant :: Square -> Square -> GenMove
mkEnPassant = pack0 tagEnPassant

{-# INLINE mkCastling #-}
mkCastling :: Square -> Square -> GenMove
mkCastling = pack0 tagCastling

{-# INLINE mkPromotion #-}
mkPromotion :: Square -> Square -> PieceType -> GenMove
mkPromotion = pack1 tagPromotion

{-# INLINE mkPromotionCapture #-}
mkPromotionCapture :: Square -> Square -> PieceType -> PieceType -> GenMove
mkPromotionCapture = pack2 tagPromotionCapture

{-# INLINE mkGenDrop #-}
mkGenDrop :: PieceType -> Square -> GenMove
mkGenDrop p t = pack1 tagDrop (Square 0) t p

{-# INLINE mkGenCastling960 #-}
mkGenCastling960 :: Square -> Square -> GenMove
mkGenCastling960 = pack0 tagCastling960
