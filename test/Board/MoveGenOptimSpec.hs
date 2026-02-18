module Board.MoveGenOptimSpec (spec) where

import Test.Hspec
import Chess.Types
import qualified Chess.Board.Fen as Fen
import qualified Chess.Board.MoveGen as MoveGen
import qualified Data.Vector.Unboxed as U

spec :: Spec
spec = do
  describe "Board.MoveGen.legalQuietChecks" $ do
    it "returns empty for starting position (no checks)" $ do
      let (Just (b, gs)) = Fen.parseFen startingFEN
      let checks = U.toList $ MoveGen.legalQuietChecks b gs
      checks `shouldBe` []

    it "identifies a pawn push check" $ do
      -- White King E1, Black King E8.
      -- White Pawn D6, Black King E8. Move D6-D7+ (attacks E8)
      -- FEN: 4k3/8/3P4/8/8/8/8/4K3 w - - 0 1
      let fenStr = "4k3/8/3P4/8/8/8/8/4K3 w - - 0 1"
      let (Just (b, gs)) = Fen.parseFen fenStr
      let checks = U.toList $ MoveGen.legalQuietChecks b gs
      let move = MoveGen.GenQuiet D6 D7 Pawn
      checks `shouldContain` [move]
      length checks `shouldBe` 1

    it "identifies a knight check" $ do
      -- White King E1, White Knight D4. Black King F8.
      -- Knight D4 to E6 (check).
      -- FEN: 5k2/8/8/8/3N4/8/8/4K3 w - - 0 1
      let fenStr = "5k2/8/8/8/3N4/8/8/4K3 w - - 0 1"
      let (Just (b, gs)) = Fen.parseFen fenStr
      let checks = U.toList $ MoveGen.legalQuietChecks b gs
      let move = MoveGen.GenQuiet D4 E6 Knight
      checks `shouldContain` [move]
      -- Verify no other checks
      length checks `shouldBe` 1

    it "identifies a castling check" $ do
      -- White can castle, and castling gives check.
      -- White King E1, Rook H1. Black King F8 (Rook ends on F1, checking F8).
      -- FEN: 5k2/8/8/8/8/8/8/4K2R w K - 0 1
      let fenStr = "5k2/8/8/8/8/8/8/4K2R w K - 0 1"
      let (Just (b, gs)) = Fen.parseFen fenStr
      let checks = U.toList $ MoveGen.legalQuietChecks b gs
      let castleMove = MoveGen.GenCastling E1 G1
      checks `shouldContain` [castleMove]

    it "identifies discovered check" $ do
      -- White Rook on E1, White Bishop on E2. Black King E8.
      -- Moving Bishop discovers check from Rook.
      -- FEN: 4k3/8/8/8/8/8/4B3/4R1K1 w - - 0 1
      let fenStr = "4k3/8/8/8/8/8/4B3/4R1K1 w - - 0 1"
      let (Just (b, gs)) = Fen.parseFen fenStr
      let checks = map MoveGen.genMoveToMove $ U.toList $ MoveGen.legalQuietChecks b gs
      -- Move E2-D3 (discovered check)
      let m1 = Move E2 D3 Nothing
      m1 `elem` checks `shouldBe` True
      -- Move E2-B5 (double check)
      let m2 = Move E2 B5 Nothing
      m2 `elem` checks `shouldBe` True

    it "does not include captures that give check" $ do
      -- White Rook A1, Black Pawn A8. Black King C8.
      -- White Rook A1 captures A8 (check).
      -- This is a capture check, should NOT be in quiet checks.
      -- FEN: 2k5/p7/8/8/8/8/8/R3K3 w - - 0 1
      let fenStr = "2k5/p7/8/8/8/8/8/R3K3 w - - 0 1"
      let (Just (b, gs)) = Fen.parseFen fenStr
      let checks = U.toList $ MoveGen.legalQuietChecks b gs
      -- Rook A1-A8 is capture.
      let move = MoveGen.GenCapture A1 A8 Rook Pawn
      move `elem` checks `shouldBe` False
      -- However, Rook A1-C1 is check (Quiet).
      let quietCheck = MoveGen.GenQuiet A1 C1 Rook
      quietCheck `elem` checks `shouldBe` True

    it "does not include promotions (even if check)" $ do
       -- White Pawn A7, Black King C8.
       -- Pawn A7-A8=Q+ (check).
       -- This is GenPromotion, not GenQuiet.
       -- FEN: 2k5/P7/8/8/8/8/8/4K3 w - - 0 1
       let fenStr = "2k5/P7/8/8/8/8/8/4K3 w - - 0 1"
       let (Just (b, gs)) = Fen.parseFen fenStr
       let checks = U.toList $ MoveGen.legalQuietChecks b gs
       checks `shouldBe` []
