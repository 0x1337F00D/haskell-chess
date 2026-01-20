module Main where

import Data.Time.Clock (getCurrentTime, diffUTCTime)
import Chess.Board (parseFen, Board)
import Chess.Engine.Evaluation (evaluate)
import Chess.Engine.ArrowEval (evaluateArrow)
import Text.Printf (printf)
import Data.Maybe (fromJust)

timeIt :: String -> Int -> (Board -> Int) -> [Board] -> IO ()
timeIt name n f boards = do
    start <- getCurrentTime
    let loop 0 _ acc = return acc
        loop k [] acc = loop k boards acc -- Recycle list
        loop k (b:bs) acc = let r = f b in r `seq` loop (k-1) bs (acc + r)

    res <- loop n boards 0
    end <- getCurrentTime
    let diff = realToFrac (diffUTCTime end start) :: Double
    printf "%s: %d iterations in %.4fs (%.2f iter/s)\n" name n diff (fromIntegral n / diff)
    print res

main :: IO ()
main = do
    let fens = [ "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
               , "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1"
               , "8/2p5/3p4/KP5r/1R3p1k/8/4P1P1/8 w - - 0 1"
               , "r3k2r/Pppp1ppp/1b3nbN/nP6/BBP1P3/q4N2/Pp1P2PP/R2Q1RK1 w kq - 0 1"
               ]
    let boards = map (fromJust . parseFen) fens
    let n = 1000000 -- 1 million

    putStrLn "Benchmarking Evaluation..."
    timeIt "Standard" n evaluate boards
    timeIt "Arrow"    n evaluateArrow boards
