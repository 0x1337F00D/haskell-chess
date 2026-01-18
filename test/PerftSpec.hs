module PerftSpec (spec) where

import Test.Hspec
import Data.List (isPrefixOf, dropWhileEnd)
import Data.Char (isSpace)
import Control.Monad (forM_)

import Chess.Types
import Chess.Board

-- | Simple string splitting helper
splitOnChar :: Char -> String -> [String]
splitOnChar _ "" = [""]
splitOnChar c s =
    let (headPart, rest) = break (== c) s
    in case rest of
        "" -> [headPart]
        (_:tailPart) -> headPart : splitOnChar c tailPart

-- | Perft function: counts leaf nodes at given depth.
-- Also validates SAN roundtrip for all legal moves encountered.
perftIO :: Int -> Board -> IO Int
perftIO 0 _ = return 1
perftIO depth board = do
    let moves = legalMoves board

    -- Verify SAN roundtrip for each move
    forM_ moves $ \move -> do
        let sanStr = san board move
        let parsed = parseSan board sanStr
        case parsed of
            Nothing -> expectationFailure $
                "Failed to parse generated SAN: '" ++ sanStr ++ "' for move " ++ show move ++ " in FEN: " ++ fen board
            Just m' -> if m' /= move
                       then expectationFailure $
                           "SAN roundtrip mismatch: '" ++ sanStr ++ "' -> " ++ show m' ++ " /= " ++ show move ++ " in FEN: " ++ fen board
                       else return ()

    if depth == 1
       then return (length moves)
       else do
           counts <- mapM (perftIO (depth - 1) . applyMove board) moves
           return (sum counts)

-- | Trim whitespace from both ends
trim :: String -> String
trim = dropWhile isSpace . dropWhileEnd isSpace

-- | Parse EPD line
-- Returns (FEN, [(Depth, Count)])
parseEpdLine :: String -> Maybe (String, [(Int, Int)])
parseEpdLine line
    | "#" `isPrefixOf` trim line = Nothing
    | null (trim line) = Nothing
    | otherwise =
        let parts = splitOnChar ';' line
            fenStr = trim (head parts)
            depths = map parseDepth (filter (not . null . trim) (tail parts))
        in Just (fenStr, depths)
  where
    parseDepth s =
        let s' = trim s
            -- Format "D1 20" or "D1 20" (spaces handled by trim)
            -- Expected s': "D<depth> <count>"
            -- Remove 'D'
            nums = tail s'
            (dStr, countStr) = break isSpace nums
        in (read dStr, read countStr)

spec :: Spec
spec = do
  describe "Perft Suite with SAN validation" $ do
    it "matches perftsuite.epd counts and verifies SAN (max depth 3)" $ do
        content <- readFile "test/gamefiles/perftsuite.epd"
        let cases = map parseEpdLine (lines content)
        mapM_ runCase cases

    where
        runCase Nothing = return ()
        runCase (Just (fenStr, expected)) = do
            let mBoard = parseFen fenStr
            case mBoard of
                Nothing -> expectationFailure $ "Failed to parse FEN: " ++ fenStr
                Just board -> do
                    -- We verify up to depth 3 for speed
                    let maxDepth = 3
                    mapM_ (checkDepth board) (filter (\(d,_) -> d <= maxDepth) expected)

        checkDepth board (d, count) = do
            result <- perftIO d board
            if result /= count
               then expectationFailure $ "FEN: " ++ fen board ++ " Depth: " ++ show d ++ " Expected: " ++ show count ++ " Got: " ++ show result
               else return ()
