module ArrowSpec (spec) where

import Test.Hspec
import Chess.Board (initialBoard, parseFen)
import Chess.Engine.Evaluation (evaluate)
import Chess.Engine.ArrowEval (evaluateArrow)
import Data.Maybe (fromJust)

spec :: Spec
spec = do
    describe "Arrow Evaluation" $ do
        it "matches standard evaluation for initial board (White to move)" $ do
            let standard = evaluate initialBoard
            let arrow = evaluateArrow initialBoard
            arrow `shouldBe` standard

        it "matches standard evaluation for midgame position (White to move)" $ do
            let fen = "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1"
            let board = fromJust (parseFen fen)
            let standard = evaluate board
            let arrow = evaluateArrow board
            arrow `shouldBe` standard

        it "matches standard evaluation for Black to move position" $ do
            let fen = "rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq - 0 1" -- After 1. e4
            let board = fromJust (parseFen fen)
            let standard = evaluate board
            let arrow = evaluateArrow board
            arrow `shouldBe` standard
