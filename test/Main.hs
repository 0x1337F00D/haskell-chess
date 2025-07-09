module Main (main) where

import Test.Hspec
import Chess.Types
import Chess.Bitboard

main :: IO ()
main = hspec $ do
  describe "colorName" $ do
    it "returns white for White" $
      colorName White `shouldBe` "white"
    it "returns black for Black" $
      colorName Black `shouldBe` "black"

  describe "pieceSymbol" $ do
    it "returns symbol for Queen" $
      pieceSymbol Queen `shouldBe` 'Q'

  describe "square parsing" $ do
    it "parses and shows a1" $
      fmap show (parseSquare "a1") `shouldBe` Just "a1"
    it "parses invalid" $
      parseSquare "i9" `shouldBe` Nothing

  describe "bitboard basics" $ do
    it "lsb of BB_E2 is square 12" $
      lsb BB_E2 `shouldBe` Just 12
    it "popcount of a file is 8" $
      popcount bbFileA `shouldBe` 8
    it "flipVertical A1 is A8" $
      flipVertical BB_A1 `shouldBe` BB_A8
    it "shiftUp from A2 gives A3" $
      shiftUp BB_A2 `shouldBe` BB_A3

