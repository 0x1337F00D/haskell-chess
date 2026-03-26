{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}

module Chess.Internal.Builder
  ( MonadEmit(..)
  , Builder
  , runBuilder
  , runBuilder256
  , SafeBuilder2(..)
  , runSafeBuilder22
  , runSafeBuilder22256
  , CountBuilder(..)
  , runCountBuilder
  , SafeBuilder(..)
  , runSafeBuilder
  , runSafeBuilder256
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

class Monad m => MonadEmit e m | m -> e where
  emit :: e -> m ()
  emitWhen :: Bool -> m () -> m ()

instance U.Unbox e => MonadEmit e (Builder s e) where
  {-# INLINE emit #-}
  emit !x =
    Builder $ \mv i s0 ->
      case M.unsafeWrite mv (I# i) x of
        ST writeST ->
          case writeST s0 of
            (# s1, () #) -> (# s1, i +# 1#, () #)

  {-# INLINE emitWhen #-}
  emitWhen False _ = mempty
  emitWhen True  b = b

newtype CountBuilder e a =
  CountBuilder { unCountBuilder :: (e -> Bool) -> Int# -> (# Int#, a #) }

instance Functor (CountBuilder e) where
  {-# INLINE fmap #-}
  fmap f (CountBuilder k) =
    CountBuilder $ \p i ->
      case k p i of
        (# i1, a #) -> (# i1, f a #)

instance Applicative (CountBuilder e) where
  {-# INLINE pure #-}
  pure a = CountBuilder $ \_ i -> (# i, a #)

  {-# INLINE (<*>) #-}
  CountBuilder kf <*> CountBuilder ka =
    CountBuilder $ \p i0 ->
      case kf p i0 of
        (# i1, f #) ->
          case ka p i1 of
            (# i2, a #) -> (# i2, f a #)

instance Monad (CountBuilder e) where
  {-# INLINE (>>=) #-}
  CountBuilder km >>= f =
    CountBuilder $ \p i0 ->
      case km p i0 of
        (# i1, a #) ->
          case f a of
            CountBuilder k2 -> k2 p i1

instance Semigroup (CountBuilder e ()) where
  {-# INLINE (<>) #-}
  a <> b = a >> b

instance Monoid (CountBuilder e ()) where
  {-# INLINE mempty #-}
  mempty = pure ()

newtype SafeBuilder2 s e a =
  SafeBuilder2 { unSafeBuilder2 :: (e -> Bool) -> M.MVector s e -> Int# -> State# s -> (# State# s, Int#, a #) }

instance Functor (SafeBuilder2 s e) where
  {-# INLINE fmap #-}
  fmap f (SafeBuilder2 k) =
    SafeBuilder2 $ \p mv i s0 ->
      case k p mv i s0 of
        (# s1, i1, a #) -> (# s1, i1, f a #)

instance Applicative (SafeBuilder2 s e) where
  {-# INLINE pure #-}
  pure a = SafeBuilder2 $ \_ _ i s -> (# s, i, a #)

  {-# INLINE (<*>) #-}
  SafeBuilder2 kf <*> SafeBuilder2 ka =
    SafeBuilder2 $ \p mv i s0 ->
      case kf p mv i s0 of
        (# s1, i1, f #) ->
          case ka p mv i1 s1 of
            (# s2, i2, a #) -> (# s2, i2, f a #)

instance Monad (SafeBuilder2 s e) where
  {-# INLINE (>>=) #-}
  SafeBuilder2 km >>= f =
    SafeBuilder2 $ \p mv i s0 ->
      case km p mv i s0 of
        (# s1, i1, a #) ->
          case f a of
            SafeBuilder2 k2 -> k2 p mv i1 s1

instance Semigroup (SafeBuilder2 s e ()) where
  {-# INLINE (<>) #-}
  a <> b = a >> b

instance Monoid (SafeBuilder2 s e ()) where
  {-# INLINE mempty #-}
  mempty = pure ()

instance U.Unbox e => MonadEmit e (SafeBuilder2 s e) where
  {-# INLINE emit #-}
  emit !x =
    SafeBuilder2 $ \p mv i s0 ->
      if p x then
        case M.unsafeWrite mv (I# i) x of
          ST writeST ->
            case writeST s0 of
              (# s1, () #) -> (# s1, i +# 1#, () #)
      else
        (# s0, i, () #)

  {-# INLINE emitWhen #-}
  emitWhen False _ = mempty
  emitWhen True  b = b

{-# INLINE runSafeBuilder22 #-}
runSafeBuilder22 :: U.Unbox e => Int -> (e -> Bool) -> (forall s. SafeBuilder2 s e ()) -> U.Vector e
runSafeBuilder22 cap p b = U.create $ do
  mv <- M.unsafeNew cap
  let fillST = ST $ \s0 ->
        case unSafeBuilder2 b p mv 0# s0 of
          (# s1, n#, () #) -> (# s1, I# n# #)
  n <- fillST
  pure (M.slice 0 n mv)

{-# INLINE runSafeBuilder22256 #-}
runSafeBuilder22256 :: U.Unbox e => (e -> Bool) -> (forall s. SafeBuilder2 s e ()) -> U.Vector e
runSafeBuilder22256 p b = runSafeBuilder22 256 p b

instance MonadEmit e (CountBuilder e) where
  {-# INLINE emit #-}
  emit !x = CountBuilder $ \p i ->
    if p x then (# i +# 1#, () #) else (# i, () #)

  {-# INLINE emitWhen #-}
  emitWhen False _ = mempty
  emitWhen True  b = b

{-# INLINE runCountBuilder #-}
runCountBuilder :: (e -> Bool) -> CountBuilder e () -> Int
runCountBuilder p b =
  case unCountBuilder b p 0# of
    (# n#, () #) -> I# n#

newtype SafeBuilder s e a =
  SafeBuilder { unSafeBuilder :: (e -> Bool) -> M.MVector s e -> Int# -> State# s -> (# State# s, Int#, a #) }

instance Functor (SafeBuilder s e) where
  {-# INLINE fmap #-}
  fmap f (SafeBuilder k) =
    SafeBuilder $ \p mv i s0 ->
      case k p mv i s0 of
        (# s1, i1, a #) -> (# s1, i1, f a #)

instance Applicative (SafeBuilder s e) where
  {-# INLINE pure #-}
  pure a = SafeBuilder $ \_ _ i s -> (# s, i, a #)

  {-# INLINE (<*>) #-}
  SafeBuilder kf <*> SafeBuilder ka =
    SafeBuilder $ \p mv i s0 ->
      case kf p mv i s0 of
        (# s1, i1, f #) ->
          case ka p mv i1 s1 of
            (# s2, i2, a #) -> (# s2, i2, f a #)

instance Monad (SafeBuilder s e) where
  {-# INLINE (>>=) #-}
  SafeBuilder km >>= f =
    SafeBuilder $ \p mv i s0 ->
      case km p mv i s0 of
        (# s1, i1, a #) ->
          case f a of
            SafeBuilder k2 -> k2 p mv i1 s1

instance Semigroup (SafeBuilder s e ()) where
  {-# INLINE (<>) #-}
  a <> b = a >> b

instance Monoid (SafeBuilder s e ()) where
  {-# INLINE mempty #-}
  mempty = pure ()

instance U.Unbox e => MonadEmit e (SafeBuilder s e) where
  {-# INLINE emit #-}
  emit !x =
    SafeBuilder $ \p mv i s0 ->
      if p x then
        case M.unsafeWrite mv (I# i) x of
          ST writeST ->
            case writeST s0 of
              (# s1, () #) -> (# s1, i +# 1#, () #)
      else
        (# s0, i, () #)

  {-# INLINE emitWhen #-}
  emitWhen False _ = mempty
  emitWhen True  b = b

{-# INLINE runSafeBuilder #-}
runSafeBuilder :: U.Unbox e => Int -> (e -> Bool) -> (forall s. SafeBuilder s e ()) -> U.Vector e
runSafeBuilder cap p b = U.create $ do
  mv <- M.unsafeNew cap
  let fillST = ST $ \s0 ->
        case unSafeBuilder b p mv 0# s0 of
          (# s1, n#, () #) -> (# s1, I# n# #)
  n <- fillST
  pure (M.slice 0 n mv)

{-# INLINE runSafeBuilder256 #-}
runSafeBuilder256 :: U.Unbox e => (e -> Bool) -> (forall s. SafeBuilder s e ()) -> U.Vector e
runSafeBuilder256 = runSafeBuilder 256
