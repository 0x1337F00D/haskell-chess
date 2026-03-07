module Main where

import Data.Binary.Put
import qualified Data.ByteString.Lazy as BL
import Data.Int

main :: IO ()
main = do
  let ftIn  = 2 * 64 * 12 * 64
      acc   = 256
      hid   = 32
      sc    = 400

      ftB   = replicate acc (0 :: Int16)
      ftW   = replicate (ftIn * acc) (0 :: Int16)
      h1B   = replicate hid (0 :: Int32)
      h1W   = replicate (hid * acc) (0 :: Int16)
      outB  = 0 :: Int32
      outW  = replicate hid (0 :: Int16)

  BL.writeFile "tiny.hsnn" $ runPut $ do
    putWord32le 0x48534E4E
    putWord32le (fromIntegral ftIn)
    putWord32le (fromIntegral acc)
    putWord32le (fromIntegral hid)
    putInt32le sc
    mapM_ putInt16le ftB
    mapM_ putInt16le ftW
    mapM_ putInt32le h1B
    mapM_ putInt16le h1W
    putInt32le outB
    mapM_ putInt16le outW
