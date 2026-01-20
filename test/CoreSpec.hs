{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

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

    it "movePiece updates piece location" $ do
      let b = initialBoard
      let b' = movePiece (Square FileE Rank2) (Square FileE Rank4) b
      -- Check E2 is empty
      getPieceAt (Square FileE Rank2) b' `shouldBe` Nothing
      -- Check E4 has White Pawn
      case getPieceAt (Square FileE Rank4) b' of
        Just (SomePiece WPawn) -> return ()
        _ -> expectationFailure "Expected White Pawn at E4"

  describe "Rules" $ do
    it "isCheck detects check" $ do
      -- Setup a board with check
      -- White King E1, Black Rook E5.
      let b = initialBoard
            { pawns = Map.empty
            , whitePieces = Map.empty
            , blackPieces = Map.singleton (Square FileE Rank5) MRook
            }
      isCheck b White `shouldBe` True
      isCheck b Black `shouldBe` False

    it "applyMove performs a move and switches turn" $ do
      let ag :: ActiveGame 'White 'Safe
          ag = ActiveGame
               { gameBoard = initialBoard
               , castlingRights = CastlingRights True True True True
               , enPassantTarget = Nothing
               , halfMoveClock = 0
               , fullMoveNumber = 1
               }
      -- Move E2 to E4
      let move = StandardMove (Square FileE Rank2) (Square FileE Rank4)

      let res = applyMove move ag
      case res of
        Continue nextGame -> do
          -- We can verify the board in nextGame
          let b' = gameBoard nextGame
          case getPieceAt (Square FileE Rank4) b' of
             Just (SomePiece WPawn) -> return ()
             _ -> expectationFailure "Expected White Pawn at E4 in next game state"
        _ -> expectationFailure "Expected Continue"
