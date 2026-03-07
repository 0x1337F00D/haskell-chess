module Main where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Binary.Put
import Data.Binary.Get
import Data.Int
import System.Environment (getArgs)
import Control.Monad (replicateM)
import Data.Word (Word32)

-- | Read a standard Stockfish .nnue file (HalfKP 256x2-32-32-1)
-- and output our flat .hsnn layout.
-- Stockfish NNUE Layout:
-- 4 bytes: Version
-- 4 bytes: Hash
-- 4 bytes: Description length
-- N bytes: Description
-- Feature Transformer (HalfKP):
--   4 bytes: Hash
--   256 * 2 bytes: Biases (Int16)
--   (256 * 41024) * 2 bytes: Weights (Int16)
-- FC layers:
--   4 bytes: Hash
--   FC 1:
--     32 * 4 bytes: Biases (Int32)
--     (32 * 512) * 2 bytes: Weights (Int16) -- 512 = 256 (white) + 256 (black)
--   FC 2:
--     32 * 4 bytes: Biases (Int32)
--     (32 * 32) * 2 bytes: Weights (Int16)
--   FC 3:
--     1 * 4 bytes: Bias (Int32)
--     (1 * 32) * 2 bytes: Weights (Int16)

data NnueRaw = NnueRaw
  { rawFtBias :: [Int16]
  , rawFtWeights :: [Int16]
  , rawH1Bias :: [Int32]
  , rawH1Weights :: [Int16]
  , rawH2Bias :: [Int32]
  , rawH2Weights :: [Int16]
  , rawOutBias :: Int32
  , rawOutWeights :: [Int16]
  }

getNnueRaw :: Get NnueRaw
getNnueRaw = do
  _version <- getWord32le
  _hash <- getWord32le
  descLen <- fromIntegral <$> getWord32le
  _desc <- getByteString descLen

  -- Feature Transformer
  _ftHash <- getWord32le
  let ftIn = 41024
      accSize = 256
  ftB <- replicateM accSize getInt16le
  ftW <- replicateM (ftIn * accSize) getInt16le

  -- FC Layers
  _fcHash <- getWord32le
  let hidSize = 32
  h1B <- replicateM hidSize getInt32le
  h1W <- replicateM (hidSize * (accSize * 2)) getInt16le

  h2B <- replicateM hidSize getInt32le
  h2W <- replicateM (hidSize * hidSize) getInt16le

  outB <- getInt32le
  outW <- replicateM hidSize getInt16le

  pure $ NnueRaw ftB ftW h1B h1W h2B h2W outB outW

main :: IO ()
main = do
  args <- getArgs
  case args of
    [inFile, outFile] -> do
      bs <- BL.readFile inFile
      let raw = runGet getNnueRaw bs

      let ftIn  = 41024
          acc   = 256
          hid   = 32
          sc    = 400 -- Default scale

      BL.writeFile outFile $ runPut $ do
        putWord32le 0x48534E4E
        putWord32le (fromIntegral ftIn)
        putWord32le (fromIntegral acc)
        putWord32le (fromIntegral hid)
        putInt32le sc
        mapM_ putInt16le (rawFtBias raw)
        mapM_ putInt16le (rawFtWeights raw)
        -- We only use 1 hidden layer in the tiny flat format currently.
        -- If we want the full 3-layer FC we'd need to update Nnue Types.
        -- We will just pack the first hidden layer into the flat file
        -- and truncate the rest for the MWE scope, or the user can expand it.
        mapM_ putInt32le (rawH1Bias raw)
        mapM_ putInt16le (rawH1Weights raw)
        putInt32le (rawOutBias raw)
        mapM_ putInt16le (rawOutWeights raw)
      putStrLn $ "Successfully converted " ++ inFile ++ " to " ++ outFile
    _ -> putStrLn "Usage: convert-nnue <input.nnue> <output.hsnn>"
