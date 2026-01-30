{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Main where

import Chess.Core.Game
import Chess.Core.Game.Internal
import Chess.Core.Rules
import Chess.Core.Rules.Class (Opposite)
import Chess.Core.Perft
import Chess.Core.Board.Internal (KnownColor(..), SColor(..), sColor)
import Chess.Types (Depth, mkDepth, unDepth)
import Data.Time.Clock
import Text.Printf
import Control.Exception (evaluate)
import System.Environment (getArgs)

withOpposite :: forall c r. KnownColor c => (KnownColor (Opposite c) => r) -> r
withOpposite f = case sColor @c of
    SWhite -> f
    SBlack -> f

-- | Run perft for a specific game state
runPerft :: (ChessVariant v, KnownColor c, KnownColor (Opposite c))
         => String -> Depth -> ActiveGame v c s -> IO ()
runPerft name depth ag = do
    start <- getCurrentTime
    let nodes = perft depth ag
    _ <- evaluate nodes
    end <- getCurrentTime
    let duration = diffUTCTime end start
    let seconds = realToFrac duration :: Double
    let nps = if seconds > 0 then fromIntegral nodes / seconds else 0

    printf "Core | %-10s | Depth %d | Nodes: %10d | Time: %6.3fs | NPS: %10.0f\n" name (unDepth depth) nodes seconds nps

benchGame :: String -> Depth -> Game 'Standard 'Active -> IO ()
benchGame name depth (InProgressGame (ag :: ActiveGame 'Standard c s)) =
    withOpposite @c (runPerft name depth ag)

main :: IO ()
main = do
    putStrLn "Benchmarking Chess.Core..."

    -- Start Position
    let gameStart = initialGame
    benchGame "Start" (mkDepth 5) gameStart

    -- KiwiPete Position
    let kiwiFen = "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1"
    case gameFromFEN kiwiFen of
        Just game -> benchGame "KiwiPete" (mkDepth 4) game
        _ -> putStrLn "Error: Failed to parse KiwiPete FEN"
