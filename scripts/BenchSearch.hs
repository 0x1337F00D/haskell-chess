module Main where

import Chess (parseFen)
import Chess.Engine.Search (search)
import Data.Time.Clock
import Text.Printf
import Control.Exception (evaluate)
import Chess.Board (uci)

main :: IO ()
main = do
    let fenStr = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
    let Just board = parseFen fenStr
    let depth = 5

    printf "Starting search depth %d...\n" depth
    start <- getCurrentTime
    bestMove <- search board depth
    end <- getCurrentTime

    let duration = diffUTCTime end start
    let seconds = realToFrac duration :: Double

    printf "Search completed. Best move: %s\n" (uci bestMove)
    printf "Time: %6.3fs\n" seconds
