module PgnSpec (spec) where

import Test.Hspec
import Chess.Pgn (parsePgn, Game(..), PgnNode(..), showGame, GameTree(..), GameNode(..))
import qualified Chess.Pgn as Pgn
import Control.Monad (forM_, foldM)
import Chess.Board

spec :: Spec
spec = do
  describe "PGN Suite" $ do
    it "trivial" $ True `shouldBe` True

    it "parses simple game" $ do
        let pgn = "[Event \"Test\"]\n1. e4 e5 1-0"
        let res = parsePgn pgn
        case res of
            Left e -> expectationFailure e
            Right [g] -> do
                Pgn.result g `shouldBe` "1-0"
                Pgn.moves g `shouldBe` ["e4", "e5"]
            Right _ -> expectationFailure "Expected 1 game"

    it "converts SAN game to UCI" $ do
        let pgn = "[Event \"Test\"]\n1. e4 e5 2. Nf3 Nc6 1-0"
        let res = parsePgn pgn
        case res of
            Left e -> expectationFailure e
            Right [g] -> do
                Pgn.gameToUci g `shouldBe` Right ["e2e4", "e7e5", "g1f3", "b8c6"]
            Right _ -> expectationFailure "Expected 1 game"

    it "handles UCI moves in PGN" $ do
        let pgn = "[Event \"Test\"]\n1. e2e4 e7e5 2. g1f3 b8c6 1-0"
        let res = parsePgn pgn
        case res of
            Left e -> expectationFailure e
            Right [g] -> do
                Pgn.gameToUci g `shouldBe` Right ["e2e4", "e7e5", "g1f3", "b8c6"]
            Right _ -> expectationFailure "Expected 1 game"

    it "respects FEN tag" $ do
        -- Position after 1. e4
        let fen = "rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq - 0 1"
        let pgn = "[FEN \"" ++ fen ++ "\"]\n1. e5 2. Nf3 1-0"
        let res = parsePgn pgn
        case res of
            Left e -> expectationFailure e
            Right [g] -> do
                Pgn.gameToUci g `shouldBe` Right ["e7e5", "g1f3"]
            Right _ -> expectationFailure "Expected 1 game"

    it "parses complex PGN with NAGs, comments and variations" $ do
        let pgn = unlines
                [ "[Event \"Complex\"]"
                , "1. e4 $1 {Good move} (1. d4 {Alternative} $2) 1... e5 1-0"
                ]
        let res = parsePgn pgn
        case res of
            Left e -> expectationFailure e
            Right [g] -> do
                let nodes = Pgn.forest g
                length nodes `shouldBe` 2

                let n1 = head nodes
                Pgn.pnSan n1 `shouldBe` "e4"
                Pgn.pnNags n1 `shouldBe` [1]
                Pgn.pnComment n1 `shouldBe` Just "Good move"

                -- Check variation
                length (Pgn.pnVariations n1) `shouldBe` 1
                let v1 = head (Pgn.pnVariations n1)
                length v1 `shouldBe` 1
                let vn = head v1
                Pgn.pnSan vn `shouldBe` "d4"
                Pgn.pnComment vn `shouldBe` Just "Alternative"
                Pgn.pnNags vn `shouldBe` [2]

                let n2 = nodes !! 1
                Pgn.pnSan n2 `shouldBe` "e5"
            Right _ -> expectationFailure "Expected 1 game"

    it "round-trips PGN" $ do
        let pgn = unlines
                [ "[Event \"RoundTrip\"]"
                , "1. e4 $1 {Good} ( 1. d4 ) 1... e5 1-0"
                ]
        let res = parsePgn pgn
        case res of
            Left e -> expectationFailure e
            Right [g] -> do
                let formatted = showGame g
                -- Parse again
                case parsePgn formatted of
                     Left e2 -> expectationFailure ("Failed to re-parse: " ++ e2 ++ "\nOutput:\n" ++ formatted)
                     Right [g2] -> do
                        -- Compare structure. Note that formatting might change whitespace
                        Pgn.tags g2 `shouldBe` Pgn.tags g
                        Pgn.forest g2 `shouldBe` Pgn.forest g
                        Pgn.result g2 `shouldBe` Pgn.result g
                     Right _ -> expectationFailure "Expected 1 game on re-parse"
            Right _ -> expectationFailure "Expected 1 game"

    it "builds a validated GameTree with variations" $ do
        let pgn = unlines
                [ "[Event \"Tree\"]"
                , "1. e4 $1 {Good} (1. d4 {Alt}) 1... e5 1-0"
                ]
        let res = parsePgn pgn
        case res of
            Left e -> expectationFailure e
            Right [g] -> do
                case Pgn.buildGameTree g of
                    Left err -> expectationFailure err
                    Right tree -> do
                        let children = Pgn.gtChildren tree
                        length children `shouldBe` 2 -- e4 (main) and d4 (variation)

                        -- Find e4
                        let e4Nodes = filter (\n -> Pgn.gnSan n == "e4") children
                        length e4Nodes `shouldBe` 1
                        let e4Node = head e4Nodes
                        Pgn.gnComment e4Node `shouldBe` Just "Good"
                        Pgn.gnNags e4Node `shouldBe` [1]

                        -- Find d4
                        let d4Nodes = filter (\n -> Pgn.gnSan n == "d4") children
                        length d4Nodes `shouldBe` 1
                        let d4Node = head d4Nodes
                        Pgn.gnComment d4Node `shouldBe` Just "Alt"

                        -- e4 should have child e5
                        let e4Children = Pgn.gnChildren e4Node
                        length e4Children `shouldBe` 1
                        let e5Node = head e4Children
                        Pgn.gnSan e5Node `shouldBe` "e5"

            Right _ -> expectationFailure "Expected 1 game"
