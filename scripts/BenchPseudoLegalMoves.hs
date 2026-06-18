{-# LANGUAGE BangPatterns #-}

module Main where

import Control.Monad (forM_)
import Data.Time.Clock (diffUTCTime, getCurrentTime)
import qualified Data.Vector.Unboxed as U
import Text.Printf (printf)

import Chess.Board (Board(..), parseFen)
import qualified Chess.Board.Base as Base
import qualified Chess.Board.GameState as GS
import qualified Chess.Board.MoveGen as MoveGen

positions :: [(String, String)]
positions =
    [ ("Start", "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1")
    , ("KiwiPete", "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1")
    , ("Endgame", "8/2p5/3p4/KP5r/1R3p1k/8/4P1P1/8 w - - 0 1")
    ]

iterations :: Int
iterations = 200000

main :: IO ()
main = do
    putStrLn "Benchmarking pseudo-legal move generation..."
    forM_ positions $ \(name, fen) ->
        case parseFen fen of
          Nothing -> putStrLn $ "Invalid FEN: " ++ name
          Just (Board b gs _) -> do
            start <- getCurrentTime
            total <- loop iterations 0 b gs
            end <- getCurrentTime
            let seconds = realToFrac (diffUTCTime end start) :: Double
                nps = if seconds > 0 then fromIntegral total / seconds else 0
            printf "%-8s | Iterations: %8d | Moves: %10d | Time: %6.3fs | Moves/s: %10.0f\n"
              name iterations total seconds nps

loop :: Int -> Int -> Base.Board -> GS.GameState -> IO Int
loop 0 !acc _ _ = return acc
loop n !acc b gs = do
    let !count = U.length (MoveGen.pseudoLegalMoves b gs)
    loop (n - 1) (acc + count) b gs
