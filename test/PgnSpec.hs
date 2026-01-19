module PgnSpec (spec) where

import Test.Hspec
import Chess.Pgn (parsePgn, Game(..), PgnPly(..))
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
                map Pgn.plySan (Pgn.plies g) `shouldBe` ["e4", "e5"]
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

    it "parses variations and comments" $ do
        let pgn = "1. e4 {Best by test} (1. d4 {Queen's Pawn} d5) e5 $1 1-0"
        let res = parsePgn pgn
        case res of
            Left e -> expectationFailure e
            Right [g] -> do
                let ps = Pgn.plies g
                length ps `shouldBe` 2 -- e4, e5

                let p1 = head ps -- e4
                Pgn.plySan p1 `shouldBe` "e4"
                Pgn.plyComment p1 `shouldBe` Just "Best by test"

                -- Variations of e4
                let vars = Pgn.plyRavs p1
                length vars `shouldBe` 1
                let var1 = head vars
                length var1 `shouldBe` 2 -- d4, d5
                let v1m1 = head var1
                Pgn.plySan v1m1 `shouldBe` "d4"
                Pgn.plyComment v1m1 `shouldBe` Just "Queen's Pawn"

                let p2 = ps !! 1 -- e5
                Pgn.plySan p2 `shouldBe` "e5"
                Pgn.plyNags p2 `shouldBe` [1]
            Right _ -> expectationFailure "Expected 1 game"

    it "parses nested variations" $ do
        let pgn = "1. e4 (1. d4 (1. c4)) 1-0"
        let res = parsePgn pgn
        case res of
            Left e -> expectationFailure e
            Right [g] -> do
                let ps = Pgn.plies g
                let p1 = head ps
                Pgn.plySan p1 `shouldBe` "e4"

                let v1 = head (Pgn.plyRavs p1) -- d4
                let d4 = head v1
                Pgn.plySan d4 `shouldBe` "d4"

                let v2 = head (Pgn.plyRavs d4) -- c4
                let c4 = head v2
                Pgn.plySan c4 `shouldBe` "c4"
            Right _ -> expectationFailure "Expected 1 game"
