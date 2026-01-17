module TypesSpec (spec) where

import Test.Hspec
import Chess.Types

spec :: Spec
spec = do
  describe "colorName" $ do
    it "returns white for White" $
      colorName White `shouldBe` "white"
    it "returns black for Black" $
      colorName Black `shouldBe` "black"

  describe "pieceSymbol" $ do
    it "returns symbol for Queen" $
      pieceSymbol Queen `shouldBe` 'Q'

  describe "unicodeSymbol" $ do
    it "returns correct unicode symbol for White King" $
      unicodeSymbol White King `shouldBe` '♔'
    it "returns correct unicode symbol for Black Queen" $
      unicodeSymbol Black Queen `shouldBe` '♛'

  describe "square parsing" $ do
    it "parses and shows a1" $
      fmap show (parseSquare "a1") `shouldBe` Just "a1"
    it "parses invalid" $
      parseSquare "i9" `shouldBe` Nothing

  describe "square utilities" $ do
    it "mirror of A1 is H8" $
      squareMirror A1 `shouldBe` H8
    it "distance from A1 to H8 is 7" $
      squareDistance A1 H8 `shouldBe` 7
    it "manhattan distance from A1 to C2 is 3" $
      squareManhattanDistance A1 C2 `shouldBe` 3
    it "knight distance B1 to C3 is 1" $
      squareKnightDistance B1 C3 `shouldBe` 1
