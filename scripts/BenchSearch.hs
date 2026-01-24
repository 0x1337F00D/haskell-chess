module Main where

import Chess (parseFen)
import Chess.Board (Board, uci)
import Chess.Types (Move)
import Chess.Engine.Search (search)
import Data.Time.Clock
import Text.Printf
import Control.Exception (evaluate)
import System.IO (hSetBuffering, stdout, BufferMode(NoBuffering))

runBench :: String -> String -> Int -> IO ()
runBench name fenStr depth = do
    let Just board = parseFen fenStr
    start <- getCurrentTime
    _ <- search board depth
    end <- getCurrentTime
    let duration = diffUTCTime end start
    let seconds = realToFrac duration :: Double

    printf "Search | %-10s | Depth %d | Time: %6.3fs\n" name depth seconds

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    runBench "Start" "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1" 2
    runBench "KiwiPete" "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1" 2
