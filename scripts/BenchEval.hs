{-# LANGUAGE BangPatterns #-}

module Main where

import Control.Monad (replicateM_)
import Data.Time.Clock (getCurrentTime, diffUTCTime)
import Chess.Board (parseFen, trustBoard, SomeValidatedBoard(..))
import Chess.Engine.Evaluation (evaluate)

main :: IO ()
main = do
    -- KiwiPete FEN
    let fen = "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1"
    case parseFen fen of
        Nothing -> putStrLn "Failed to parse FEN"
        Just board -> do
            let svBoard = trustBoard board

            putStrLn "Benchmarking evaluate..."
            start <- getCurrentTime

            let n = 10000000

            case svBoard of
                InCheckBoard vb ->
                    replicateM_ n $ do
                        let !_ = evaluate vb
                        return ()
                NotInCheckBoard vb ->
                    replicateM_ n $ do
                        let !_ = evaluate vb
                        return ()

            end <- getCurrentTime
            let duration = diffUTCTime end start
            putStrLn $ "Time: " ++ show duration
            let nps = fromIntegral n / realToFrac duration
            putStrLn $ "NPS: " ++ show nps
