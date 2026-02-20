module Board.GameStateSpec (spec) where

import Test.Hspec
import Chess.Types
import qualified Chess.Board.GameState as GS

spec :: Spec
spec = do
  describe "Board.GameState" $ do
    it "initial game state has correct defaults" $ do
      let gs = GS.initialGameState
      GS.turn gs `shouldBe` White
      GS.castlingRights gs `shouldBe` GS.allCastling
      GS.epSquare gs `shouldBe` NoSquare
      GS.halfmoveClock gs `shouldBe` 0
      GS.fullmoveNumber gs `shouldBe` 1

    it "canCastleStandardKingside/Queenside checks bitboard correctly" $ do
      let gs = GS.initialGameState
      GS.canCastleStandardKingside gs White `shouldBe` True
      GS.canCastleStandardQueenside gs White `shouldBe` True
      GS.canCastleStandardKingside gs Black `shouldBe` True
      GS.canCastleStandardQueenside gs Black `shouldBe` True

    it "removeColorCastlingRights removes both rights for a color" $ do
      let gs = GS.initialGameState
          gs' = GS.removeColorCastlingRights gs White
      GS.canCastleStandardKingside gs' White `shouldBe` False
      GS.canCastleStandardQueenside gs' White `shouldBe` False
      -- Black untouched
      GS.canCastleStandardKingside gs' Black `shouldBe` True
      GS.canCastleStandardQueenside gs' Black `shouldBe` True

    it "removeCastlingRight removes specific right" $ do
      let gs = GS.initialGameState
          gs' = GS.removeCastlingRight gs H1
      GS.canCastleStandardKingside gs' White `shouldBe` False
      GS.canCastleStandardQueenside gs' White `shouldBe` True
      GS.canCastleStandardKingside gs' Black `shouldBe` True
