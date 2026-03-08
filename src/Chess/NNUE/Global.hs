module Chess.NNUE.Global (globalNnue) where

import Chess.NNUE.Types (Nnue)
import Chess.NNUE.Flat (loadNnueFlat)
import System.IO.Unsafe (unsafePerformIO)
import Control.Exception (try, SomeException)

{-# NOINLINE globalNnue #-}
globalNnue :: Maybe Nnue
globalNnue = unsafePerformIO $ do
  res <- try (loadNnueFlat "engine.hsnn") :: IO (Either SomeException Nnue)
  case res of
    Left _ -> pure Nothing
    Right n -> pure (Just n)
