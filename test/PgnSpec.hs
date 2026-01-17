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
