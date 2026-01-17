module SquareSetSpec (spec) where

import Test.Hspec
import Chess.Types
import qualified Chess.SquareSet as SS

spec :: Spec
spec = do
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
