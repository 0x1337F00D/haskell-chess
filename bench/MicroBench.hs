{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

module Main where

import Control.Monad (replicateM_)
import Data.Time.Clock (getCurrentTime, diffUTCTime)
import Data.Word
import Data.Bits
import System.Environment (getArgs)

import Chess.Board (parseFen, trustBoard, legalMovesValidated, ValidatedBoard, SomeValidatedBoard(..), MoveGenerator)
import Chess.Engine.Search.Ordering (orderGenMoves)
import Chess.Bitboard ((.&~.))

{-# NOINLINE getDynamicA #-}
getDynamicA :: IO Word64
getDynamicA = return 0x1234567890abcdef

{-# NOINLINE getDynamicB #-}
getDynamicB :: IO Word64
getDynamicB = return 0xfedcba0987654321

main :: IO ()
main = do
    putStrLn "Benchmarking Bitwise Operator..."
    a <- getDynamicA
    b <- getDynamicB

    start1 <- getCurrentTime
    replicateM_ 50000000 $ do
        let !_c = a .&. complement b
        return ()
    end1 <- getCurrentTime

    let duration1 = diffUTCTime end1 start1
    putStrLn $ "Time for 50M 'a .&. complement b': " ++ show duration1

    start2 <- getCurrentTime
    replicateM_ 50000000 $ do
        let !_c = a .&~. b
        return ()
    end2 <- getCurrentTime

    let duration2 = diffUTCTime end2 start2
    putStrLn $ "Time for 50M 'a .&~. b': " ++ show duration2


    let fen = "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1"
    case parseFen fen of
        Nothing -> putStrLn "Failed to parse FEN"
        Just board -> case trustBoard board of
            InCheckBoard vb -> runBench vb
            NotInCheckBoard vb -> runBench vb

runBench :: MoveGenerator s => ValidatedBoard s -> IO ()
runBench vBoard = do
    let moves = legalMovesValidated vBoard

    putStrLn $ "Number of moves: " ++ show (length moves)

    start <- getCurrentTime
    replicateM_ 200000 $ do
        let !_sorted = orderGenMoves vBoard moves Nothing
        return ()
    end <- getCurrentTime

    let duration = diffUTCTime end start
    putStrLn $ "Time for 200k sorts: " ++ show duration
