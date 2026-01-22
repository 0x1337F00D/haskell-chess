{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Main where

import Chess.Core.Board.Internal (KnownColor(..), SColor(..))
import Chess.Core.Game
import Chess.Core.Rules
import Chess.Core.Perft
import Chess.Core.Game.Internal (ActiveGame(..), Game(..))
import Data.Time.Clock
import Text.Printf
import Control.Exception (evaluate)

runPerft :: forall v. ChessVariant v => Int -> Game v 'Active -> Int
runPerft d (InProgressGame (ag :: ActiveGame v c s)) =
  case sColor @c of
    SWhite -> perft d ag
    SBlack -> perft d ag

runBench :: String -> String -> Int -> IO ()
runBench name fenStr depth = do
    case gameFromFEN fenStr of
        Nothing -> putStrLn $ "Failed to parse FEN: " ++ fenStr
        Just (InProgressGame ag) -> do
            let game = InProgressGame ag :: Game 'Standard 'Active
            start <- getCurrentTime
            let nodes = runPerft depth game
            _ <- evaluate nodes
            end <- getCurrentTime
            let duration = diffUTCTime end start
            let seconds = realToFrac duration :: Double
            let nps = fromIntegral nodes / seconds

            printf "Core | %-10s | Depth %d | Nodes: %10d | Time: %6.3fs | NPS: %10.0f\n" name depth nodes seconds nps
        Just _ -> putStrLn "Game not in progress"

main :: IO ()
main = do
    runBench "Start" "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1" 5
    runBench "KiwiPete" "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1" 4
