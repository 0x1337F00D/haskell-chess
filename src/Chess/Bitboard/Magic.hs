{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Chess.Bitboard.Magic (Magic(..)) where

import Data.Word (Word64)
import Control.Monad (liftM)
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as M
import qualified Data.Vector.Unboxed as U

type Bitboard = Word64

data Magic = Magic
    { mMask   :: !Bitboard
    , mMagic  :: !Word64
    , mShift  :: !Int
    , mOffset :: !Int     -- Offset into the global attack table
    } deriving (Show)

-- Unbox Instances for Magic
newtype instance U.MVector s Magic = MV_Magic (U.MVector s (Word64, Word64, Int, Int))
newtype instance U.Vector    Magic = V_Magic  (U.Vector    (Word64, Word64, Int, Int))

instance U.Unbox Magic

instance M.MVector U.MVector Magic where
    {-# INLINE basicLength #-}
    {-# INLINE basicUnsafeSlice #-}
    {-# INLINE basicOverlaps #-}
    {-# INLINE basicUnsafeNew #-}
    {-# INLINE basicInitialize #-}
    {-# INLINE basicUnsafeReplicate #-}
    {-# INLINE basicUnsafeRead #-}
    {-# INLINE basicUnsafeWrite #-}
    {-# INLINE basicClear #-}
    {-# INLINE basicSet #-}
    {-# INLINE basicUnsafeCopy #-}
    {-# INLINE basicUnsafeMove #-}
    {-# INLINE basicUnsafeGrow #-}
    basicLength (MV_Magic v) = M.basicLength v
    basicUnsafeSlice i n (MV_Magic v) = MV_Magic (M.basicUnsafeSlice i n v)
    basicOverlaps (MV_Magic v1) (MV_Magic v2) = M.basicOverlaps v1 v2
    basicUnsafeNew n = MV_Magic `liftM` M.basicUnsafeNew n
    basicInitialize (MV_Magic v) = M.basicInitialize v
    basicUnsafeReplicate n (Magic m g s o) = MV_Magic `liftM` M.basicUnsafeReplicate n (m, g, s, o)
    basicUnsafeRead (MV_Magic v) i = do
        (m, g, s, o) <- M.basicUnsafeRead v i
        return (Magic m g s o)
    basicUnsafeWrite (MV_Magic v) i (Magic m g s o) = M.basicUnsafeWrite v i (m, g, s, o)
    basicClear (MV_Magic v) = M.basicClear v
    basicSet (MV_Magic v) (Magic m g s o) = M.basicSet v (m, g, s, o)
    basicUnsafeCopy (MV_Magic v1) (MV_Magic v2) = M.basicUnsafeCopy v1 v2
    basicUnsafeMove (MV_Magic v1) (MV_Magic v2) = M.basicUnsafeMove v1 v2
    basicUnsafeGrow (MV_Magic v) n = MV_Magic `liftM` M.basicUnsafeGrow v n

instance G.Vector U.Vector Magic where
    {-# INLINE basicUnsafeFreeze #-}
    {-# INLINE basicUnsafeThaw #-}
    {-# INLINE basicLength #-}
    {-# INLINE basicUnsafeSlice #-}
    {-# INLINE basicUnsafeIndexM #-}
    {-# INLINE basicUnsafeCopy #-}
    {-# INLINE elemseq #-}
    basicUnsafeFreeze (MV_Magic v) = V_Magic `liftM` G.basicUnsafeFreeze v
    basicUnsafeThaw (V_Magic v) = MV_Magic `liftM` G.basicUnsafeThaw v
    basicLength (V_Magic v) = G.basicLength v
    basicUnsafeSlice i n (V_Magic v) = V_Magic (G.basicUnsafeSlice i n v)
    basicUnsafeIndexM (V_Magic v) i = do
        (m, g, s, o) <- G.basicUnsafeIndexM v i
        return (Magic m g s o)
    basicUnsafeCopy (MV_Magic mv) (V_Magic v) = G.basicUnsafeCopy mv v
    elemseq _ (Magic m g s o) z = G.elemseq (undefined :: U.Vector Word64) m
                               $ G.elemseq (undefined :: U.Vector Word64) g
                               $ G.elemseq (undefined :: U.Vector Int) s
                               $ G.elemseq (undefined :: U.Vector Int) o z
