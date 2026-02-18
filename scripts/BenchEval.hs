{-# LANGUAGE BangPatterns #-}

module Main where

import Control.Monad (forM_)
import Data.Time.Clock (getCurrentTime, diffUTCTime)
import Chess.Board (parseFen, trustBoard, SomeValidatedBoard(..))
import Chess.Engine.Evaluation (evaluate)

main :: IO ()
main = do
    let fens = [ "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
               , "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1"
               , "8/2p5/3p4/KP5r/1R3p1k/8/4P1P1/8 w - - 0 1"
               , "r3k2r/Pppp1ppp/1b3nbN/nP6/BBP1P3/q4N2/Pp1P2PP/R2Q1RK1 w kq - 0 1"
               , "rnbq1k1r/pp1Pbppp/2p5/8/2B5/8/PPP1NnPP/RNBQK2R w KQ - 1 8"
               , "r4rk1/1pp1qppp/p1np1n2/2b1p1B1/2B1P1b1/P1NP1N2/1PP1QPPP/R4RK1 w - - 0 10"
               ]

    let boards = map (\f -> case parseFen f of Just b -> b; Nothing -> error "bad fen") fens
    let vBoards = map trustBoard boards

    -- Flatten the validated boards into a list of "SomeValidatedBoard"
    -- We'll just loop over this list many times.
    -- To avoid "lifting", we need the list to be long or loop over it.

    let n = 1000000 -- 1 million iterations total
    let infiniteBoards = cycle vBoards
    let testBoards = take n infiniteBoards

    putStrLn $ "Benchmarking evaluate on " ++ show (length fens) ++ " positions, " ++ show n ++ " times..."

    start <- getCurrentTime

    let loop [] !acc = return acc
        loop (vb:rest) !acc = do
            let score = case vb of
                          InCheckBoard b -> evaluate b
                          NotInCheckBoard b -> evaluate b
            loop rest (acc + score)

    totalScore <- loop testBoards 0

    end <- getCurrentTime
    let duration = diffUTCTime end start
    putStrLn $ "Total Score: " ++ show totalScore -- force evaluation
    putStrLn $ "Time: " ++ show duration
    let nps = fromIntegral n / realToFrac duration
    putStrLn $ "NPS: " ++ show nps
