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

    it "canCastleKingside/Queenside checks bitboard correctly" $ do
      let gs = GS.initialGameState
      GS.canCastleKingside gs White `shouldBe` True
      GS.canCastleQueenside gs White `shouldBe` True
      GS.canCastleKingside gs Black `shouldBe` True
      GS.canCastleQueenside gs Black `shouldBe` True

    it "removeColorCastlingRights removes both rights for a color" $ do
      let gs = GS.initialGameState
          gs' = GS.removeColorCastlingRights gs White
      GS.canCastleKingside gs' White `shouldBe` False
      GS.canCastleQueenside gs' White `shouldBe` False
      -- Black untouched
      GS.canCastleKingside gs' Black `shouldBe` True
      GS.canCastleQueenside gs' Black `shouldBe` True

    it "removeCastlingRight removes specific right" $ do
      let gs = GS.initialGameState
          gs' = GS.removeCastlingRight gs H1
      GS.canCastleKingside gs' White `shouldBe` False
      GS.canCastleQueenside gs' White `shouldBe` True
      GS.canCastleKingside gs' Black `shouldBe` True
