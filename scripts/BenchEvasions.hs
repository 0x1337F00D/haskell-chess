{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -fno-cse -fno-full-laziness #-}

module Main where

import Control.Monad (foldM)
import Data.Time.Clock (getCurrentTime, diffUTCTime)
import Chess.Board (parseFen, trustBoard, captureMovesValidated, legalQuietsValidated, legalPromotionsValidated, ValidatedBoard, SomeValidatedBoard(..))
import Chess.Types (CheckStatus(..))

checkFens :: [String]
checkFens =
    [ "rnbqkbnr/pppp1ppp/8/4p3/1b1P4/8/PPP1PPPP/RNBQKBNR w KQkq - 1 3" -- Bishop Check
    , "8/8/8/8/8/3n4/8/4K3 w - - 0 1" -- Knight Check
    , "8/8/8/8/8/8/6k1/4K2r w - - 0 1" -- Rook Check
    , "4k3/8/8/8/8/8/4q3/4K3 w - - 0 1" -- Queen Check
    ]

main :: IO ()
main = do
    mapM_ runBenchForFen checkFens

runBenchForFen :: String -> IO ()
runBenchForFen fen = do
    putStrLn $ "FEN: " ++ fen
    case parseFen fen of
        Nothing -> putStrLn $ "Invalid FEN: " ++ fen
        Just board -> case trustBoard board of
            NotInCheckBoard _ -> putStrLn $ "Skipping (Not in check)"
            InCheckBoard vb -> runBench vb

runBench :: ValidatedBoard 'InCheck -> IO ()
runBench vb = do
    let n = 200000 :: Int
    start <- getCurrentTime

    total <- foldM (\acc _ -> do
        let !c = length (captureMovesValidated vb)
        let !q = length (legalQuietsValidated vb)
        let !p = length (legalPromotionsValidated vb)
        return $! acc + c + q + p
        ) 0 [1..n]

    end <- getCurrentTime
    putStrLn $ "Time for " ++ show n ++ " iterations: " ++ show (diffUTCTime end start)
    putStrLn $ "Total moves counted: " ++ show total
