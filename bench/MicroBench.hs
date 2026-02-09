{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

module Main where

import Control.Monad (replicateM_)
import Data.Time.Clock (getCurrentTime, diffUTCTime)
import qualified Data.Vector.Unboxed as U

import Chess.Board (parseFen, trustBoard, legalMovesValidated, ValidatedBoard, SomeValidatedBoard(..))
import Chess.Engine.Search.Ordering (orderGenMoves)
import Chess.Types (Move)

main :: IO ()
main = do
    let fen = "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1"
    case parseFen fen of
        Nothing -> putStrLn "Failed to parse FEN"
        Just board -> do
            case trustBoard board of
                SomeValidatedBoard vBoard -> do
                    let moves = legalMovesValidated vBoard

                    putStrLn $ "Number of moves: " ++ show (length moves)

                    start <- getCurrentTime
                    replicateM_ 200000 $ do
                        let !sorted = orderGenMoves vBoard moves Nothing
                        return ()
                    end <- getCurrentTime

                    let duration = diffUTCTime end start
                    putStrLn $ "Time for 200k sorts: " ++ show duration
