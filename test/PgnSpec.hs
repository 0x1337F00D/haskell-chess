module PgnSpec (spec) where

import Test.Hspec
import Chess.Pgn (parsePgn, Game(..))
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
