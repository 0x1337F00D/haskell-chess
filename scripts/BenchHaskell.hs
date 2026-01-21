module Main where

import Chess (parseFen)
import Chess.Board (Board, legalMoves, applyMove)
import Data.Time.Clock
import Text.Printf
import Control.Exception (evaluate)

perft :: Int -> Board -> Int
perft 0 _ = 1
perft depth board =
    let moves = legalMoves board
    in if depth == 1
       then length moves
       else sum $ map (\m -> perft (depth - 1) (applyMove board m)) moves

runBench :: String -> String -> Int -> IO ()
runBench name fenStr depth = do
    let Just board = parseFen fenStr
    start <- getCurrentTime
    let nodes = perft depth board
    _ <- evaluate nodes
    end <- getCurrentTime
    let duration = diffUTCTime end start
    let seconds = realToFrac duration :: Double
    let nps = fromIntegral nodes / seconds

    printf "Haskell | %-10s | Depth %d | Nodes: %10d | Time: %6.3fs | NPS: %10.0f\n" name depth nodes seconds nps

main :: IO ()
main = do
    runBench "Start" "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1" 5
    runBench "KiwiPete" "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1" 4
