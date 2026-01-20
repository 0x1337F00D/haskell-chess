module Board.UciSpec (spec) where

import Test.Hspec
import Chess.Types
import qualified Chess.Board.Uci as Uci

spec :: Spec
spec = do
  describe "Chess.Board.Uci" $ do
    it "uci generates correct string" $ do
      let m = Move E2 E4 Nothing
      Uci.uci m `shouldBe` "e2e4"
    it "fromUci parses correct string" $ do
      let str = "e2e4"
      Uci.fromUci str `shouldBe` Just (Move E2 E4 Nothing)
    it "fromUci handles promotion" $ do
      let str = "a7a8q"
      Uci.fromUci str `shouldBe` Just (Move A7 A8 (Just Queen))
