{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}

module Chess.Internal.Builder
  ( Builder
  , runBuilder
  , runBuilder256
  , emit
  , emitWhen
  ) where

import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as M

import GHC.Exts (Int(I#))
import GHC.Prim (Int#, State#, (+#))
import GHC.ST (ST(..))

newtype Builder s e a =
  Builder { unBuilder :: M.MVector s e -> Int# -> State# s -> (# State# s, Int#, a #) }

instance Functor (Builder s e) where
  {-# INLINE fmap #-}
  fmap f (Builder k) =
    Builder $ \mv i s0 ->
      case k mv i s0 of
        (# s1, i1, a #) -> (# s1, i1, f a #)

instance Applicative (Builder s e) where
  {-# INLINE pure #-}
  pure a = Builder $ \_ i s -> (# s, i, a #)

  {-# INLINE (<*>) #-}
  Builder kf <*> Builder ka =
    Builder $ \mv i s0 ->
      case kf mv i s0 of
        (# s1, i1, f #) ->
          case ka mv i1 s1 of
            (# s2, i2, a #) -> (# s2, i2, f a #)

instance Monad (Builder s e) where
  {-# INLINE (>>=) #-}
  Builder km >>= f =
    Builder $ \mv i s0 ->
      case km mv i s0 of
        (# s1, i1, a #) ->
          case f a of
            Builder k2 -> k2 mv i1 s1

instance Semigroup (Builder s e ()) where
  {-# INLINE (<>) #-}
  a <> b = a >> b

instance Monoid (Builder s e ()) where
  {-# INLINE mempty #-}
  mempty = pure ()

{-# INLINE runBuilder #-}
runBuilder :: U.Unbox e => Int -> (forall s. Builder s e ()) -> U.Vector e
runBuilder cap b = U.create $ do
  mv <- M.unsafeNew cap
  let fillST = ST $ \s0 ->
        case unBuilder b mv 0# s0 of
          (# s1, n#, () #) -> (# s1, I# n# #)
  n <- fillST
  pure (M.slice 0 n mv)

{-# INLINE runBuilder256 #-}
runBuilder256 :: U.Unbox e => (forall s. Builder s e ()) -> U.Vector e
runBuilder256 = runBuilder 256

{-# INLINE emit #-}
emit :: U.Unbox e => e -> Builder s e ()
emit !x =
  Builder $ \mv i s0 ->
    case M.unsafeWrite mv (I# i) x of
      ST writeST ->
        case writeST s0 of
          (# s1, () #) -> (# s1, i +# 1#, () #)

{-# INLINE emitWhen #-}
emitWhen :: Bool -> Builder s e () -> Builder s e ()
emitWhen False _ = mempty
emitWhen True  b = b
