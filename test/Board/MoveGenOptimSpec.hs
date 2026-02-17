module Board.MoveGenOptimSpec (spec) where

import Test.Hspec
import Chess.Types
import qualified Chess.Board.Fen as Fen
import qualified Chess.Board.MoveGen as MoveGen
import qualified Chess.Board as Board
import qualified Data.Vector.Unboxed as U

spec :: Spec
spec = describe "Board.MoveGen.Optim" $ do
    it "filters pseudo-legal quiet moves for checks" $ do
      -- FEN: White to move. Ra1, Ke1. Black Kh8.
      -- Ra8+ is a quiet check.
      let fenStr = "7k/8/8/8/8/8/8/R3K3 w - - 0 1"
      let Just (b, gs) = Fen.parseFen fenStr

      let quietChecks = U.toList $ MoveGen.legalQuietChecks b gs
      let moves = map MoveGen.genMoveToMove quietChecks

      let checkMove = Move A1 A8 Nothing
      checkMove `elem` moves `shouldBe` True

    it "excludes captures from quiet checks" $ do
      -- FEN: White to move. Ra1, Ke1. Black Kh8, Pawn a8 (black).
      -- Ra1xa8+ is a capture check.
      let fenStr = "p6k/8/8/8/8/8/8/R3K3 w - - 0 1"
      let Just (b, gs) = Fen.parseFen fenStr

      let quietChecks = U.toList $ MoveGen.legalQuietChecks b gs
      let moves = map MoveGen.genMoveToMove quietChecks

      let checkMove = Move A1 A8 Nothing
      checkMove `elem` moves `shouldBe` False

    it "validated version via Board API works" $ do
      let fenStr = "7k/8/8/8/8/8/8/R3K3 w - - 0 1"
      let Just board = Board.parseFen fenStr
      let vBoard = Board.trustBoard board

      let quietChecks = Board.legalQuietChecksValidated vBoard
      let moves = map (\lm -> Move (Board.moveFrom lm) (Board.moveTo lm) (Board.movePromotion lm)) quietChecks

      let checkMove = Move A1 A8 Nothing
      checkMove `elem` moves `shouldBe` True
