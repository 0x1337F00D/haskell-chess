{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GADTs #-}

module CoreSpec where

import Test.Hspec
import Chess.Core.Board
import Chess.Core.Game
import Chess.Core.Move
import Chess.Core.Rules
import qualified Data.Map as Map

spec :: Spec
spec = describe "Core Architecture" $ do
  describe "Board" $ do
    it "initialBoard has Kings on correct squares" $ do
      whiteKing initialBoard `shouldBe` Square FileE Rank1
      blackKing initialBoard `shouldBe` Square FileE Rank8

    it "initialBoard has correct number of Pawns" $ do
      Map.size (pawns initialBoard) `shouldBe` 16

  describe "Game Types" $ do
    it "Can construct a Setup Game" $ do
      let game = SetupGame initialBoard
      case game of
        SetupGame b -> b `shouldBe` initialBoard
