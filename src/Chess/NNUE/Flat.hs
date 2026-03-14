{-# LANGUAGE BangPatterns #-}
module Chess.NNUE.Flat
  ( loadNnueFlat
  ) where

import Chess.NNUE.Types
import Control.Monad
import Data.Binary.Get
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as BS
import Data.Word
import Data.Primitive.ByteArray
import Control.Monad.ST (RealWorld)

import System.IO.Unsafe (unsafePerformIO)

magicWord :: Word32
magicWord = 0x48534E4E  -- "HSNN"

loadNnueFlat :: FilePath -> IO Nnue
loadNnueFlat fp = do
  bs <- BL.readFile fp
  case runGetOrFail getNnue bs of
    Left (_, _, err) -> fail err
    Right (_, _, nnue) -> pure nnue

getNnue :: Get Nnue
getNnue = do
  m <- getWord32le
  when (m /= magicWord) $
    fail "invalid HSNN file"
  !ftIn <- fromIntegral <$> getWord32le
  !acc  <- fromIntegral <$> getWord32le
  !hid  <- fromIntegral <$> getWord32le
  !sc   <- fromIntegral <$> getInt32le

  !ftB  <- getInt16Array acc
  !ftW  <- getInt16Array (ftIn * acc)
  !h1B  <- getInt32Array hid
  !h1W  <- getInt16Array (hid * (acc * 2))
  !h2B  <- getInt32Array hid
  !h2W  <- getInt16Array (hid * hid)
  !outB <- getInt32le
  !outW <- getInt16Array hid

  pure Nnue
    { ftInputSize = ftIn
    , accSize     = acc
    , hiddenSize  = hid
    , ftBias      = ftB
    , ftWeights   = ftW
    , h1Bias      = h1B
    , h1Weights   = h1W
    , h2Bias      = h2B
    , h2Weights   = h2W
    , outBias     = outB
    , outWeights  = outW
    , scale       = sc
    }

getInt16Array :: Int -> Get ByteArray
getInt16Array n = do
  let totalBytes = n * 2
  bs <- getByteString totalBytes
  pure $ unsafePerformIO $ do
    mba <- newByteArray totalBytes
    let go !i
          | i == totalBytes = pure ()
          | otherwise = do
              let byte = BS.index bs i
              writeByteArray mba i byte
              go (i + 1)
    go 0
    unsafeFreezeByteArray mba

getInt32Array :: Int -> Get ByteArray
getInt32Array n = do
  let totalBytes = n * 4
  bs <- getByteString totalBytes
  pure $ unsafePerformIO $ do
    mba <- newByteArray totalBytes
    let go !i
          | i == totalBytes = pure ()
          | otherwise = do
              let byte = BS.index bs i
              writeByteArray mba i byte
              go (i + 1)
    go 0
    unsafeFreezeByteArray mba
