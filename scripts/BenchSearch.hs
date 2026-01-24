module Main where

import Data.Time.Clock (getCurrentTime, diffUTCTime)
import Chess.Board (parseFen, Board(..))
import Chess.Engine.Search (search)

-- KiwiPete position
fen :: String
fen = "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1"

main :: IO ()
main = do
    putStrLn "Starting Benchmark (KiwiPete Depth 2)..."
    let Just b = parseFen fen
    start <- getCurrentTime
    _ <- search b 2
    end <- getCurrentTime
    putStrLn $ "Time: " ++ show (diffUTCTime end start)
