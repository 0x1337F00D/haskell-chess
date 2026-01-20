module ArrowSpec (spec) where

import Test.Hspec
import Chess.Board (initialBoard)
import Chess.Engine.Evaluation (evaluate)
import Chess.Engine.ArrowEval (evaluateArrow)
import Control.Arrow (runArrow)

spec :: Spec
spec = do
    describe "Arrow Evaluation" $ do
        it "matches standard evaluation for initial board" $ do
            let standard = evaluate initialBoard
            -- evaluateArrow is (Arrow a => a Board Score).
            -- We run it using the function arrow (->).
            let arrow = evaluateArrow initialBoard
            arrow `shouldBe` standard
