module PerftSpec (spec) where

import Test.Hspec
import Data.List (isPrefixOf, dropWhileEnd)
import Data.Char (isSpace)

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
perft :: Int -> Board -> Int
perft 0 _ = 1
perft depth board =
    let moves = legalMoves board
    in if depth == 1 then length moves
       else sum $ map (perft (depth - 1) . applyMove board) moves

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
            fen = trim (head parts)
            depths = map parseDepth (filter (not . null . trim) (tail parts))
        in Just (fen, depths)
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
  describe "Perft Suite" $ do
    it "matches perftsuite.epd counts (max depth 3)" $ do
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
                    -- We verify up to depth 3 for speed, similar to PyChess testMovegen1
                    let maxDepth = 3
                    mapM_ (checkDepth board) (filter (\(d,_) -> d <= maxDepth) expected)

        checkDepth board (d, count) = do
            let result = perft d board
            -- Using strict equality. If failing, we'll see expected vs actual.
            if result /= count
               then expectationFailure $ "FEN: " ++ fen board ++ " Depth: " ++ show d ++ " Expected: " ++ show count ++ " Got: " ++ show result
               else return ()
