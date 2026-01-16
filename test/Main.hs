module Main (main) where

import Test.Hspec
import Data.Bits
import Chess.Types
import Chess.Bitboard
import qualified Chess.SquareSet as SS

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

    describe "SquareSet" $ do
      it "empty is empty" $
        SS.null SS.empty `shouldBe` True
      it "singleton contains element" $
        SS.member A1 (SS.singleton A1) `shouldBe` True
      it "insert adds element" $
        SS.member A1 (SS.insert A1 SS.empty) `shouldBe` True
      it "delete removes element" $
        SS.member A1 (SS.delete A1 (SS.singleton A1)) `shouldBe` False
      it "fromList/toList roundtrip" $
        SS.toList (SS.fromList [A1, C3, H8]) `shouldBe` [A1, C3, H8]
      it "union combines sets" $
        let s1 = SS.singleton A1
            s2 = SS.singleton H8
        in SS.toList (SS.union s1 s2) `shouldBe` [A1, H8]
      it "intersection finds common" $
        let s1 = SS.fromList [A1, B2]
            s2 = SS.fromList [B2, C3]
        in SS.toList (SS.intersection s1 s2) `shouldBe` [B2]
      it "difference removes elements" $
        let s1 = SS.fromList [A1, B2]
            s2 = SS.singleton B2
        in SS.toList (SS.difference s1 s2) `shouldBe` [A1]
      it "size is correct" $
        SS.size (SS.fromList [A1, B2, C3]) `shouldBe` 3
      it "subset check" $
        SS.isSubsetOf (SS.singleton A1) (SS.fromList [A1, B2]) `shouldBe` True

