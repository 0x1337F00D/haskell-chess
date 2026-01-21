module Board.MoveGenSpec (spec) where

import Test.Hspec
import Chess.Types
import qualified Chess.Board.Fen as Fen
import qualified Chess.Board.MoveGen as MoveGen

spec :: Spec
spec = do
  describe "Board.MoveGen" $ do
    it "generates 20 moves for starting position" $ do
      let (Just (b, gs)) = Fen.parseFen startingFEN
      let moves = MoveGen.pseudoLegalMoves b gs
      length moves `shouldBe` 20
      let legal = MoveGen.legalMoves b gs
      length legal `shouldBe` 20

    it "pseudoLegalMoves includes castling" $ do
       -- Position where white can castle kingside and queenside
       let fenStr = "r3k2r/8/8/8/8/8/8/R3K2R w KQkq - 0 1"
       let (Just (b, gs)) = Fen.parseFen fenStr
       let moves = map (\(MoveGen.GenMove m _ _) -> m) $ MoveGen.pseudoLegalMoves b gs
       -- Check if O-O and O-O-O are in moves
       let castlingK = Move E1 G1 Nothing
       let castlingQ = Move E1 C1 Nothing
       castlingK `elem` moves `shouldBe` True
       castlingQ `elem` moves `shouldBe` True

    it "legalMoves filters castling through check" $ do
       -- White king E1, Rooks A1, H1.
       -- Black Rook on E8 (attacks E1, so in check -> can't castle)
       -- Wait, standard castling rule: can't castle out of check.
       -- Let's test "through check".
       -- White King E1, Rook H1.
       -- Black Rook on F8 (attacks F1). F1 is passed through for O-O.
       -- This FEN is:
       -- r . . . . r k . (rank 8)
       -- ...
       -- R . . . K . . R (rank 1)
       -- I need to place a piece to attack F1.
       -- Let's place Black Rook on F8.
       let fenStr2 = "5r2/8/8/8/8/8/8/4K2R w K - 0 1"
       let (Just (b, gs)) = Fen.parseFen fenStr2
       let moves = MoveGen.legalMoves b gs
       let castlingK = Move E1 G1 Nothing
       castlingK `elem` moves `shouldBe` False

    it "legalMoves allows castling when legal" $ do
       let fenStr = "8/8/8/8/8/8/8/4K2R w K - 0 1"
       let (Just (b, gs)) = Fen.parseFen fenStr
       let moves = MoveGen.legalMoves b gs
       let castlingK = Move E1 G1 Nothing
       castlingK `elem` moves `shouldBe` True
