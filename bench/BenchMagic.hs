{-# LANGUAGE BangPatterns #-}
module Main where

import Data.Time.Clock
import Chess.Bitboard
import Chess.Types (Square(..))
import Data.Bits
import Data.Word
import Text.Printf

-- Xorshift64*
nextRand :: Word64 -> Word64
nextRand x =
  let x1 = x `xor` (x `shiftR` 12)
      x2 = x1 `xor` (x1 `shiftL` 25)
      x3 = x2 `xor` (x2 `shiftR` 27)
  in x3 * 0x2545F4914F6CDD1D

benchAttacks :: String -> Int -> IO ()
benchAttacks name iter = do
    start <- getCurrentTime
    let !dummy = loop iter 123456789 0
    end <- getCurrentTime
    let diff = realToFrac (diffUTCTime end start) :: Double
    printf "%s: %d iterations in %.4fs (%.2f M/s)\n" name iter diff (fromIntegral iter / diff / 1e6)
    print dummy -- prevent optimization

loop :: Int -> Word64 -> Word64 -> Word64
loop 0 _ !acc = acc
loop n seed !acc =
    let seed1 = nextRand seed
        seed2 = nextRand seed1
        sq = Square (fromIntegral (seed1 `mod` 64))
        occ = seed2
        attacks = bishopAttacks sq occ `xor` rookAttacks sq occ
    in loop (n-1) seed2 (acc `xor` attacks)

main :: IO ()
main = do
    putStrLn "Initializing Magics..."
    let !start = bishopAttacks (Square 0) 0
    print start
    putStrLn "Benchmarking Sliding Attacks..."
    benchAttacks "Mix" 10000000
