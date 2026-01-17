module Board.BaseSpec (spec) where

import Test.Hspec
import Data.Bits
import Chess.Types
import Chess.Bitboard
import qualified Chess.Board.Base as Board

spec :: Spec
spec = do
  describe "Board.Base" $ do
    it "empty board is empty" $
      Board.occupied Board.empty `shouldBe` 0
    it "putPiece places a piece" $
      let b = Board.putPiece Board.empty E4 (Piece White Pawn)
      in Board.pieceAt b E4 `shouldBe` Just (Piece White Pawn)
    it "removePiece removes a piece" $
      let b = Board.putPiece Board.empty E4 (Piece White Pawn)
          b' = Board.removePieceAt b E4
      in Board.pieceAt b' E4 `shouldBe` Nothing
    it "occupiedBy returns correct bitboard" $
      let b = Board.putPiece Board.empty E4 (Piece White Pawn)
      in Board.occupiedBy b White `shouldBe` bbFromSquare E4

    describe "attacks" $ do
      it "rook attacks on empty board" $
         let b = Board.putPiece Board.empty E4 (Piece White Rook)
             att = Board.attacks b E4
             expected = (bbRank4 .|. bbFileE) `clearBit` (unSquare E4)
         in att `shouldBe` expected

      it "rook attacks blocked by own piece" $
         let b0 = Board.putPiece Board.empty A1 (Piece White Rook)
             b1 = Board.putPiece b0 A3 (Piece White Pawn)
             att = Board.attacks b1 A1
             -- A1=0. A3=16. Rank 1 is A1..H1. File A is A1..A8.
             -- Up (North): A2, A3 (blocked).
             -- Right (East): B1..H1.
             -- Expected: A2, A3, B1..H1.
             expected = bbFromSquare A2 .|. bbFromSquare A3 .|. (bbRank1 `clearBit` unSquare A1)
         in att `shouldBe` expected

      it "bishop attacks blocked by enemy piece" $
         let b0 = Board.putPiece Board.empty C1 (Piece White Bishop)
             b1 = Board.putPiece b0 E3 (Piece Black Pawn)
             att = Board.attacks b1 C1
             -- C1 is (2,0).
             -- Diag NE: (3,1)=D2, (4,2)=E3. Stop.
             -- Diag NW: (1,1)=B2, (0,2)=A3. Stop (edge).
             expected = bbFromSquare D2 .|. bbFromSquare E3 .|. bbFromSquare B2 .|. bbFromSquare A3
         in att `shouldBe` expected

      it "isAttackedBy checks attacks" $
         let b = Board.putPiece Board.empty E4 (Piece White Rook)
         in Board.isAttackedBy b White E8 `shouldBe` True
