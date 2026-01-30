module EngineSpec (spec) where

import Test.Hspec
import Data.Maybe (fromJust)
import Chess.Board (initialBoard, parseFen, uci, trustBoard)
import Chess.Engine.Evaluation (evaluate)
import Chess.Engine.Search (search)
import Chess.Engine.TT (newTT)

spec :: Spec
spec = describe "Engine" $ do
  describe "Evaluation" $ do
    it "evaluates initial board to 0" $ do
      evaluate (trustBoard initialBoard) `shouldBe` 0

  describe "Search" $ do
    it "finds simple mate in 1" $ do
      -- Fool's mate pattern
      -- 1. f3 e5 2. g4 ??
      -- Black to move: Qh4#
      let fenStr = "rnbqkbnr/pppp1ppp/8/4p3/6P1/5P2/PPPPP2P/RNBQKBNR b KQkq - 0 2"
          board = fromJust $ parseFen fenStr
      tt <- newTT 16
      move <- search board tt 2
      uci move `shouldBe` "d8h4"
