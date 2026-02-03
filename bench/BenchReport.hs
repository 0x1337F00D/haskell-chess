{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Main where

import System.Environment (getArgs)
import Data.Time.Clock (getCurrentTime, diffUTCTime)
import Text.Printf (printf)
import Control.Monad (forM_)
import System.IO (hFlush, stdout)

import Chess.Types (Depth(..), mkDepth, unDepth)
import Chess.Board (parseFen, uci, Board)
import Chess.Engine.Search (search)
-- import Chess.Engine.Search.Types (SearchResult(..))
import Chess.Engine.TT (newTT, clearTT)

-- Perft Imports
import Chess.Core.Game
import Chess.Core.Game.Internal
import Chess.Core.Rules
import Chess.Core.Rules.Class (Opposite)
import Chess.Core.Perft
import Chess.Core.Board.Internal (KnownColor(..), SColor(..), sColor)

data TestCase = TestCase
    { tcName :: String
    , tcFen :: String
    , tcExpected :: [String]
    }

tacticsCases :: [TestCase]
tacticsCases =
    [ TestCase "Fool's Mate"
        "rnbqkbnr/pppp1ppp/8/4p3/6P1/5P2/PPPPP2P/RNBQKBNR b KQkq - 0 2"
        ["d8h4"]
    , TestCase "Scholar's Mate"
        "r1bqkb1r/pppp1ppp/2n2n2/4p2Q/2B1P3/8/PPPP1PPP/RNB1K1NR w KQkq - 4 4"
        ["h5f7"]
    , TestCase "Fine #70"
        "8/8/8/8/8/8/4k3/R3n2K w - - 0 1"
        ["h1g1"]
    ]

main :: IO ()
main = do
    args <- getArgs
    let searchDepth = case args of
                        ("--depth":d:_) -> read d
                        _ -> 6

    putStrLn "=== PERF ==="
    runPerftSuite

    putStrLn "\n=== SEARCH ==="
    runSearchSuite searchDepth

    putStrLn "\n=== TACTICS ==="
    runTacticsSuite

-- Perft Suite
runPerftSuite :: IO ()
runPerftSuite = do
    let gameStart = initialGame
    benchPerft "startpos" (mkDepth 5) gameStart

    let kiwiFen = "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1"
    case gameFromFEN kiwiFen of
        Just game -> benchPerft "kiwipete" (mkDepth 4) game
        _ -> putStrLn "Error: Failed to parse KiwiPete FEN"

benchPerft :: String -> Depth -> Game 'Standard 'Active -> IO ()
benchPerft name depth (InProgressGame (ag :: ActiveGame 'Standard c s)) =
    withOpposite @c (doPerft name depth ag)

doPerft :: (ChessVariant v, KnownColor c, KnownColor (Opposite c))
        => String -> Depth -> ActiveGame v c s -> IO ()
doPerft name depth ag = do
    start <- getCurrentTime
    let nodes = perft depth ag
    end <- getCurrentTime
    let duration = realToFrac (diffUTCTime end start) :: Double
    let nps = if duration > 0 then fromIntegral nodes / duration else 0
    printf "perft %s d%d nodes=%d nps=%.0f\n" name (unDepth depth) nodes nps

withOpposite :: forall c r. KnownColor c => (KnownColor (Opposite c) => r) -> r
withOpposite f = case sColor @c of
    SWhite -> f
    SBlack -> f

-- Search Suite
runSearchSuite :: Int -> IO ()
runSearchSuite depth = do
    let kiwiFen = "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1"
    case parseFen kiwiFen of
        Nothing -> putStrLn "Invalid KiwiPete FEN"
        Just board -> do
            tt <- newTT 20
            res <- search board tt depth
            -- let nps = if srTime res > 0 then fromIntegral (srNodes res) / srTime res else 0
            -- printf "kiwipete d%d nodes=%d qnodes=%d time=%.3f nps=%.0f pv=%s\n"
            --    depth (srNodes res) (srQNodes res) (srTime res) nps (uci (srBestMove res))
            putStrLn "Search done"

-- Tactics Suite
runTacticsSuite :: IO ()
runTacticsSuite = do
    tt <- newTT 20
    let depth = 8

    forM_ tacticsCases $ \tc -> do
        case parseFen (tcFen tc) of
            Nothing -> printf "name=%s FAIL (Invalid FEN)\n" (tcName tc)
            Just board -> do
                clearTT tt
                res <- search board tt depth
                -- let bestM = srBestMove res
                -- let uciM = uci bestM
                -- let passed = uciM `elem` tcExpected tc
                -- let status = if passed then "PASS" else "FAIL"
                -- printf "name=%s depth=%d expected=%s got=%s pass=%s\n"
                --    (tcName tc) depth (head (tcExpected tc)) uciM status
                putStrLn "Tactics done"
