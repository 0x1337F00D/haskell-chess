module Main (main) where

import Test.Hspec
import Data.Bits
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

    describe "square utilities" $ do
      it "mirror of A1 is H8" $
        squareMirror A1 `shouldBe` H8
      it "distance from A1 to H8 is 7" $
        squareDistance A1 H8 `shouldBe` 7
      it "manhattan distance from A1 to C2 is 3" $
        squareManhattanDistance A1 C2 `shouldBe` 3
      it "knight distance B1 to C3 is 1" $
        squareKnightDistance B1 C3 `shouldBe` 1

    describe "attack tables" $ do
      it "knight attacks from B1 include A3 and C3" $
        let a = knightAttacks B1
        in a .&. (bbFromSquare A3 .|. bbFromSquare C3) `shouldBe`
             (bbFromSquare A3 .|. bbFromSquare C3)
      it "king attacks from E4 include F5" $
        kingAttacks E4 .&. bbFromSquare F5 `shouldBe` bbFromSquare F5
      it "white pawn attacks from E2 include D3" $
        pawnAttacks White E2 .&. bbFromSquare D3 `shouldBe` bbFromSquare D3
      it "black pawn attacks from E7 include D6" $
        pawnAttacks Black E7 .&. bbFromSquare D6 `shouldBe` bbFromSquare D6


