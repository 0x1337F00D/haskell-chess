{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE BangPatterns #-}

module Chess.Internal.Builder
  ( Builder(..)
  , runBuilder256
  , emit1
  , emitFill
  , emitWhen
  ) where

import Control.Monad.ST (ST)
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as M

-- | Abstract builder handle. Users cannot access the mutable vector directly.
newtype Builder s a = Builder { unBuilder :: M.MVector s a -> Int -> ST s Int }

-- | Run a builder with a fixed upper bound (256 here).
--   One allocation, one pass. Returns the final slice.
{-# INLINE runBuilder256 #-}
runBuilder256 :: U.Unbox a => (forall s. Builder s a) -> U.Vector a
runBuilder256 (Builder k) = U.create $ do
  mv <- M.unsafeNew 256
  !n  <- k mv 0
  pure (M.slice 0 n mv)

-- | Emit exactly one element (very common).
{-# INLINE emit1 #-}
emit1 :: U.Unbox a => a -> Builder s a
emit1 !x = Builder $ \mv !i -> do
  M.unsafeWrite mv i x
  pure (i + 1)

-- | Wrap an existing “fillXYZ mv idx -> idx'” function as a Builder step.
{-# INLINE emitFill #-}
emitFill :: (M.MVector s a -> Int -> ST s Int) -> Builder s a
emitFill f = Builder $ \mv !i -> f mv i

-- | Helper to emit conditionally.
{-# INLINE emitWhen #-}
emitWhen :: Bool -> Builder s a -> Builder s a
emitWhen False _ = mempty
emitWhen True  b = b

-- | Monoidal composition (so call sites read nicely)
instance Semigroup (Builder s a) where
  {-# INLINE (<>) #-}
  Builder f <> Builder g = Builder $ \mv !i -> do
    !j <- f mv i
    g mv j

instance Monoid (Builder s a) where
  {-# INLINE mempty #-}
  mempty = Builder $ \_ !i -> pure i
