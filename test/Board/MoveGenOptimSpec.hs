module Board.MoveGenOptimSpec (spec) where

import Test.Hspec
import Chess.Types
import qualified Chess.Board.Fen as Fen
import qualified Chess.Board.MoveGen as MoveGen
import qualified Data.Vector.Unboxed as U

spec :: Spec
spec = do
  describe "Board.MoveGen.legalQuietChecks" $ do
    it "finds no checks when none exist (Knight position)" $ do
       -- 8/8/8/8/4N3/8/8/4k2K w - - 0 1
       -- White Knight at E4 cannot check Black King at E1 immediately.
       let fen = "8/8/8/8/4N3/8/8/4k2K w - - 0 1"
       let (Just (b, gs)) = Fen.parseFen fen
       let checks = MoveGen.legalQuietChecks b gs
       U.length checks `shouldBe` 0

    it "finds check moves correctly (Rook to back rank)" $ do
       -- 4k3/8/8/8/8/8/8/R3K2R w K - 0 1
       -- Rooks at A1, H1. King at E1. Black King at E8.
       -- Checks: Ra8+, Rh8+.
       let fen = "4k3/8/8/8/8/8/8/R3K2R w K - 0 1"
       let (Just (b, gs)) = Fen.parseFen fen
       let checks = MoveGen.legalQuietChecks b gs
       U.length checks `shouldBe` 2

       let moves = map MoveGen.genMoveToMove $ U.toList checks
       let ra8 = Move A1 A8 Nothing
       let rh8 = Move H1 H8 Nothing

       ra8 `elem` moves `shouldBe` True
       rh8 `elem` moves `shouldBe` True

    it "does not include illegal checking moves (pinned piece)" $ do
       -- White King E1, White Rook E2 (pinned), Black Rook E8.
       -- White Rook cannot move.
       -- 4r3/8/8/8/8/8/4R3/4K3 w - - 0 1
       -- If White moves Rook E2 to say A2 or H2, it exposes King to check.
       -- But wait, Rook E2 checking Black King?
       -- I need a position where a pinned piece COULD give check if it moved, but is pinned.

       -- White King E1. White Bishop D2. Black Rook H2 (checking E2? No).
       -- White King E1. White Rook E2. Black Rook E8.
       -- If White Rook moves to say E7 (check), it exposes King E1 to Rook E8.
       -- So Re7+ is illegal.

       -- Position: 4r3/8/8/8/8/8/4R3/4K3 w - - 0 1
       -- White to move.
       -- Rook at E2 is pinned by Rook at E8.
       -- Rook at E2 could move to E7 giving check to King at E8?
       -- No, Rook at E2 captures Rook at E8 is legal (capture).
       -- But legalQuietChecks only returns quiet moves.
       -- Move Re2-e7 is quiet.
       -- Does Re2-e7 give check? Yes (direct check).
       -- Is Re2-e7 legal? No, King E1 is exposed to Rook E8.

       let fen = "4r3/8/8/8/8/8/4R3/4K3 w - - 0 1"
       let (Just (b, gs)) = Fen.parseFen fen
       let checks = MoveGen.legalQuietChecks b gs

       -- Re7 is (E2, E7).
       let re7 = Move E2 E7 Nothing
       let moves = map MoveGen.genMoveToMove $ U.toList checks

       re7 `elem` moves `shouldBe` False

    it "finds discovered check" $ do
       -- White Rook A1. White Bishop B1. Black King A8.
       -- 4k3/8/8/8/8/8/8/RB2K3 w - - 0 1
       -- If Bishop moves, Rook gives check.
       -- Bishop at B1. Rook at A1. King at E8 (too far).
       -- Let's put Black King at A8.
       -- k7/8/8/8/8/8/8/RB2K3 w - - 0 1
       -- Rook A1 attacks A file. Bishop B1 blocks it.
       -- Bishop moves to say C2.
       -- Rook A1 now checks A8.
       -- Move B1-C2 is quiet.

       let fen = "k7/8/8/8/8/8/8/RB2K3 w - - 0 1"
       let (Just (b, gs)) = Fen.parseFen fen
       let checks = MoveGen.legalQuietChecks b gs

       -- Bishop moves: C2, D3, E4, F5, G6, H7. All unblock check.
       -- Are they legal? Yes.
       let moves = map MoveGen.genMoveToMove $ U.toList checks

       let bc2 = Move B1 C2 Nothing
       bc2 `elem` moves `shouldBe` True
