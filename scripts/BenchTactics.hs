module Main where

import Control.Monad (forM)
import Text.Printf (printf)
import System.IO (hFlush, stdout)

import Chess.Board (parseFen, uci, Board)
import Chess.Engine.Search (search)
import Chess.Engine.Search.Types (SearchLimits(..), defaultLimits)
import Chess.Engine.TT (TT, newTT)
import Chess.Types (Move)

data TestCase = TestCase
    { tcName :: String
    , tcFen :: String
    , tcExpected :: [String] -- List of acceptable moves (UCI strings)
    }

cases :: [TestCase]
cases =
    [ TestCase "Fool's Mate (Mate in 1)"
        "rnbqkbnr/pppp1ppp/8/4p3/6P1/5P2/PPPPP2P/RNBQKBNR b KQkq - 0 2"
        ["d8h4"]
    , TestCase "Scholar's Mate (Mate in 1)"
        "r1bqkb1r/pppp1ppp/2n2n2/4p2Q/2B1P3/8/PPPP1PPP/RNB1K1NR w KQkq - 4 4"
        ["h5f7"]
    , TestCase "Fine #70 (Mate in 3)"
        "8/8/8/8/8/8/4k3/R3n2K w - - 0 1"
        ["h1g1"] -- The key waiting move
    ]

main :: IO ()
main = do
    putStrLn "Running Tactical Benchmark Suite..."
    -- Allocate TT (1MB size roughly, 20 bits entries? No, 20 bits size usually means 2^20 entries)
    -- newTT takes an Int. If it's bits, 20 = 1M entries.
    tt <- newTT 20

    let depth = 8 -- Enough for Fine #70

    results <- forM cases $ \tc -> do
        runCase tt depth tc

    let passed = length (filter id results)
    let total = length cases

    printf "\nPassed: %d / %d\n" passed total
    if passed == total
        then putStrLn "SUCCESS"
        else putStrLn "FAILURE"

runCase :: TT -> Int -> TestCase -> IO Bool
runCase tt depth (TestCase name fenStr expected) = do
    putStr $ "Test: " ++ name ++ "... "
    hFlush stdout
    case parseFen fenStr of
        Nothing -> do
            putStrLn "Invalid FEN!"
            return False
        Just board -> do
            -- Run search
            bestMove <- search board tt (defaultLimits { limitDepth = Just depth })
            let uciMove = uci bestMove

            -- Check result
            if uciMove `elem` expected
                then do
                    putStrLn $ "PASS (" ++ uciMove ++ ")"
                    return True
                else do
                    putStrLn $ "FAIL (Got: " ++ uciMove ++ ", Expected: " ++ show expected ++ ")"
                    return False
