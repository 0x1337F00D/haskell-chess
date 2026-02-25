{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}

module Chess.Internal.Builder
  ( Builder
  , runBuilder
  , runBuilder256
  , emit
  , emitWhen
  ) where

import Control.Monad.ST (ST)
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as M

-- | A builder monad that builds a vector of elements 'e' in 'ST'.
-- It carries the current index in the state.
newtype Builder s e a = Builder { unBuilder :: M.MVector s e -> Int -> ST s (Int, a) }

instance Functor (Builder s e) where
  {-# INLINE fmap #-}
  fmap f (Builder m) = Builder $ \v i -> do
    (i', x) <- m v i
    pure (i', f x)

instance Applicative (Builder s e) where
  {-# INLINE pure #-}
  pure x = Builder $ \_ i -> pure (i, x)
  {-# INLINE (<*>) #-}
  Builder mf <*> Builder mx = Builder $ \v i -> do
    (i', f) <- mf v i
    (i'', x) <- mx v i'
    pure (i'', f x)

instance Monad (Builder s e) where
  {-# INLINE (>>=) #-}
  Builder m >>= k = Builder $ \v i -> do
    (i', x) <- m v i
    unBuilder (k x) v i'

instance Semigroup (Builder s e ()) where
  {-# INLINE (<>) #-}
  a <> b = a >> b

instance Monoid (Builder s e ()) where
  {-# INLINE mempty #-}
  mempty = pure ()

-- | Run a builder with a fixed upper bound (cap).
--   Allocates one vector, fills it, and returns the used slice.
{-# INLINE runBuilder #-}
runBuilder :: U.Unbox e => Int -> (forall s. Builder s e ()) -> U.Vector e
runBuilder cap b = U.create $ do
  mv <- M.unsafeNew cap
  (n, _) <- unBuilder b mv 0
  pure (M.slice 0 n mv)

-- | Run a builder with the standard 256 size.
{-# INLINE runBuilder256 #-}
runBuilder256 :: U.Unbox e => (forall s. Builder s e ()) -> U.Vector e
runBuilder256 = runBuilder 256

-- | Emit one element.
{-# INLINE emit #-}
emit :: U.Unbox e => e -> Builder s e ()
emit !x = Builder $ \mv !i -> do
  M.unsafeWrite mv i x
  pure (i + 1, ())

-- | Emit conditionally.
{-# INLINE emitWhen #-}
emitWhen :: Bool -> Builder s e () -> Builder s e ()
emitWhen False _ = mempty
emitWhen True  b = b
