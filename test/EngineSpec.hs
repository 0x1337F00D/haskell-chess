module EngineSpec where

import Test.Hspec
import Chess.Board (initialBoard, parseFen, trustBoard, SomeValidatedBoard(..), uci)
import Chess.Engine.Evaluation (evaluate)
import Chess.Engine.Search (search)
import Chess.Engine.Search.Types (SearchLimits(..), defaultLimits)
import Chess.Engine.TT (newTT)

evaluateSome :: SomeValidatedBoard -> Int
evaluateSome (InCheckBoard vb) = evaluate vb
evaluateSome (NotInCheckBoard vb) = evaluate vb

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Evaluation" $ do
    it "should evaluate initial position as equal" $ do
      evaluateSome (trustBoard initialBoard) `shouldBe` 0

    it "should value material" $ do
      -- White has extra pawn
      let Just board = parseFen "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
      evaluateSome (trustBoard board) `shouldBe` 0

      -- Position with material imbalance
      let Just board = parseFen "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBN1 w KQkq - 0 1" -- White missing Rook at H1
      -- White to move. Missing rook -> Score should be very negative.
      evaluateSome (trustBoard board) `shouldSatisfy` (< (-300))

    it "should recognize space/center control (somewhat)" $ do
       let Just board = parseFen "rnbqkbnr/pppp1ppp/8/4p3/4P3/8/PPPP1PPP/RNBQKBNR w KQkq - 0 2"
       -- Both sides e4/e5. Equal.
       evaluateSome (trustBoard board) `shouldBe` 0

       let Just board = parseFen "rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR w KQkq - 0 1"
       -- White has center pawn, Black doesn't. Material equal.
       evaluateSome (trustBoard board) `shouldSatisfy` (> 0) -- Slight advantage for space/tempo?

    it "should penalize king safety" $ do
       let Just board = parseFen "rnbqkbnr/pppppppp/8/8/8/8/PPP1PPPP/RNBQKBNR w KQkq - 0 1" -- Normal
       evaluateSome (trustBoard board) `shouldSatisfy` (> (-50)) -- Roughly equal

       let Just board = parseFen "rnbqkbnr/pppppppp/8/8/8/8/8/RNBQKBNR w KQkq - 0 1" -- White missing all pawns!
       evaluateSome (trustBoard board) `shouldSatisfy` (< (-500))

  describe "Search" $ do
    it "finds mate in 1 (Fool's Mate)" $ do
       let Just board = parseFen "rnbqkbnr/pppp1ppp/8/4p3/6P1/5P2/PPPPP2P/RNBQKBNR b KQkq - 0 2"
       tt <- newTT 1
       bestMove <- search board tt (defaultLimits { limitDepth = Just 2 })
       uci bestMove `shouldBe` "d8h4"
