module CoreFenSpec where

import Test.Hspec
import Chess.Core.Fen
import Data.Maybe (isJust, fromJust)

spec :: Spec
spec = describe "Core FEN Parsing" $ do
  describe "ThreeCheck FEN" $ do
    it "parses standard FEN with default checks" $ do
      let fen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
      let res = parseThreeCheckFen fen
      res `shouldSatisfy` isJust
      let (_, _, (w, b)) = fromJust res
      w `shouldBe` 0
      b `shouldBe` 0

    it "parses FEN with explicit checks +0+0" $ do
      let fen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1 +0+0"
      let res = parseThreeCheckFen fen
      res `shouldSatisfy` isJust
      let (_, _, (w, b)) = fromJust res
      w `shouldBe` 0
      b `shouldBe` 0

    it "parses FEN with non-zero checks +2+1" $ do
      let fen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1 +2+1"
      let res = parseThreeCheckFen fen
      res `shouldSatisfy` isJust
      let (_, _, (w, b)) = fromJust res
      w `shouldBe` 2
      b `shouldBe` 1

    it "fails on malformed checks" $ do
      let fen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1 +a+b"
      let res = parseThreeCheckFen fen
      res `shouldSatisfy` (\r -> case r of
                                   Just _ -> False
                                   Nothing -> True)

    it "roundtrips ThreeCheck FEN" $ do
      let fenStr = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1 +2+3"
      let res = parseThreeCheckFen fenStr
      res `shouldSatisfy` isJust
      let (b, gs, checks) = fromJust res
      threeCheckFen b gs checks `shouldBe` fenStr
