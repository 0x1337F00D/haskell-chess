module BitboardSpec (spec) where

import Test.Hspec
import Data.Bits
import Chess.Types
import Chess.Bitboard

spec :: Spec
spec = do
  describe "bitboard basics" $ do
    it "lsb of BB_E2 is square 12" $
      lsb BB_E2 `shouldBe` Just 12
    it "popcount of a file is 8" $
      popcount bbFileA `shouldBe` 8
    it "flipVertical A1 is A8" $
      flipVertical BB_A1 `shouldBe` BB_A8
    it "shiftUp from A2 gives A3" $
      shiftUp BB_A2 `shouldBe` BB_A3

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

  describe "sliding attacks" $ do
    it "bishop attacks from D4 (empty board)" $ do
      let att = bishopAttacks D4 0
      let expected = foldr (.|.) 0 $ map bbFromSquare [E5, F6, G7, H8, C5, B6, A7, E3, F2, G1, C3, B2, A1]
      att `shouldBe` expected

    it "bishop attacks from D4 with blocker at E5" $ do
      let occ = bbFromSquare E5
      let att = bishopAttacks D4 occ
      let expected = foldr (.|.) 0 $ map bbFromSquare [E5, C5, B6, A7, E3, F2, G1, C3, B2, A1]
      att `shouldBe` expected

    it "rook attacks from D4 (empty board)" $ do
      let att = rookAttacks D4 0
      let expected = foldr (.|.) 0 $ map bbFromSquare
            ([D5, D6, D7, D8] ++ [D3, D2, D1] ++ [E4, F4, G4, H4] ++ [C4, B4, A4])
      att `shouldBe` expected

    it "rook attacks from D4 with blocker at D6" $ do
      let occ = bbFromSquare D6
      let att = rookAttacks D4 occ
      let expected = foldr (.|.) 0 $ map bbFromSquare
            ([D5, D6] ++ [D3, D2, D1] ++ [E4, F4, G4, H4] ++ [C4, B4, A4])
      att `shouldBe` expected
