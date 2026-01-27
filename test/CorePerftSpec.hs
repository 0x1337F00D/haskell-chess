{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module CorePerftSpec where

import Test.Hspec
import Chess.Core.Board.Internal (KnownColor(..), SColor(..))
import Chess.Core.Game
import Chess.Core.Rules
import Chess.Core.Perft
import Chess.Core.Game.Internal (Game(..))
import Chess.Types (Depth)

runPerft :: forall v. ChessVariant v => Depth -> Game v 'Active -> Int
runPerft d (InProgressGame (ag :: ActiveGame v c s)) =
  case sColor @c of
    SWhite -> perft d ag
    SBlack -> perft d ag

spec :: Spec
spec = describe "Core Perft" $ do
  describe "Standard Initial Position" $ do
    it "Depth 1: 20 moves" $ do
      let game = initialGame
      runPerft 1 game `shouldBe` 20

    it "Depth 2: 400 moves" $ do
      let game = initialGame
      runPerft 2 game `shouldBe` 400

    it "Depth 3: 8902 moves" $ do
      let game = initialGame
      runPerft 3 game `shouldBe` 8902

  describe "Kiwipete Position" $ do
    -- Position 2 from CPW
    let fen = "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1"
    it "Depth 1: 48 moves" $ do
      case gameFromFEN fen of
        Just game@(InProgressGame _) -> runPerft 1 game `shouldBe` 48
        Nothing -> expectationFailure "Failed to parse Kiwipete FEN"

    it "Depth 2: 2039 moves" $ do
      case gameFromFEN fen of
        Just game@(InProgressGame _) -> runPerft 2 game `shouldBe` 2039
        Nothing -> expectationFailure "Failed to parse Kiwipete FEN"
