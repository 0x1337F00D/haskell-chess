module Main where

import System.Environment (getArgs)
import Data.Time.Clock (getCurrentTime, diffUTCTime)
import Chess.Board (parseFen)
import Chess.Engine.Search (search)
import Chess.Engine.TT (newTT)

-- KiwiPete position
fen :: String
fen = "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1"

main :: IO ()
main = do
    args <- getArgs
    let depth = case args of
                  [d] -> read d :: Int
                  _ -> 6
    putStrLn $ "Starting Benchmark (KiwiPete Depth " ++ show depth ++ ")..."
    let Just b = parseFen fen
    tt <- newTT 20
    start <- getCurrentTime
    _ <- search b tt depth
    end <- getCurrentTime
    putStrLn $ "Time: " ++ show (diffUTCTime end start)
