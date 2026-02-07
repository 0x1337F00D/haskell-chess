{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.Environment (getArgs)
import Data.Time.Clock (getCurrentTime, diffUTCTime)
import Text.Printf (printf)

import Chess.Board (parseFen, trustBoard, Board, ValidatedBoard)
import Chess.Engine.Evaluation (evaluate)

main :: IO ()
main = do
    let kiwiFen = "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1"
    let board = case parseFen kiwiFen of
                    Just b -> b
                    Nothing -> error "Invalid FEN"
    let vBoard = trustBoard board

    putStrLn "Benchmarking evaluation..."
    start <- getCurrentTime
    let n = 10000000 :: Int

    loop n vBoard 0

    end <- getCurrentTime
    let duration = realToFrac (diffUTCTime end start) :: Double
    printf "Evaluations: %d\n" n
    printf "Time: %.4f s\n" duration
    printf "NPS: %.0f\n" (fromIntegral n / duration)

loop :: Int -> ValidatedBoard -> Int -> IO ()
loop 0 _ !acc = print acc
loop k vb !acc = do
    let s = evaluate vb
    loop (k-1) vb (acc + s)
