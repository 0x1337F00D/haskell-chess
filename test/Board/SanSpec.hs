module Board.SanSpec (spec) where

import Test.Hspec
import Chess.Types
import qualified Chess.Board.Base as Board
import qualified Chess.Board.GameState as GS
import qualified Chess.Board.Fen as Fen
import qualified Chess.Board.San as San

spec :: Spec
spec = do
  describe "Chess.Board.San" $ do
    it "generates simple pawn move" $ do
       let (Just (b, gs)) = Fen.parseFen startingFEN
       let m = Move E2 E4 Nothing
       San.san b gs m `shouldBe` "e4"

    it "generates knight move" $ do
       let (Just (b, gs)) = Fen.parseFen startingFEN
       let m = Move G1 F3 Nothing
       San.san b gs m `shouldBe` "Nf3"

    it "generates check" $ do
       -- Position: White Rook A1, Black King A8. Move Ra7+ doesn't make sense.
       -- White Rook A1. Black King A8. Move A1-A8 is capture.
       -- Let's use Fool's Mate final move.
       -- 1. f3 e5 2. g4 Qh4#
       -- State before Qh4#:
       -- rnbqkbnr/pppp1ppp/8/4p3/6P1/5P2/PPPPP2P/RNBQKBNR b KQkq - 0 1
       let fenStr = "rnbqkbnr/pppp1ppp/8/4p3/6P1/5P2/PPPPP2P/RNBQKBNR b KQkq - 0 1"
       let (Just (b, gs)) = Fen.parseFen fenStr
       -- Move: Qh4 (d8 to h4)
       let m = Move D8 H4 Nothing
       San.san b gs m `shouldBe` "Qh4#"

    it "generates castling O-O" $ do
       let fenStr = "rnbqk2r/pppp1ppp/5n2/2b1p3/4P3/5N2/PPPP1PPP/RNBQKB1R w KQkq - 4 4" -- Ruy Lopez
       -- Actually simple start position with pieces removed
       let fenStr2 = "r3k2r/8/8/8/8/8/8/R3K2R w KQkq - 0 1"
       let (Just (b, gs)) = Fen.parseFen fenStr2
       let m = Move E1 G1 Nothing
       San.san b gs m `shouldBe` "O-O"

    it "generates castling O-O-O" $ do
       let fenStr2 = "r3k2r/8/8/8/8/8/8/R3K2R w KQkq - 0 1"
       let (Just (b, gs)) = Fen.parseFen fenStr2
       let m = Move E1 C1 Nothing
       San.san b gs m `shouldBe` "O-O-O"

    it "generates capture" $ do
       -- e4 d5 exd5
       let fenStr = "rnbqkbnr/ppp1pppp/8/3p4/4P3/8/PPPP1PPP/RNBQKBNR w KQkq - 0 2"
       let (Just (b, gs)) = Fen.parseFen fenStr
       -- Move: exd5. E4 to D5.
       let m = Move E4 D5 Nothing
       San.san b gs m `shouldBe` "exd5"

    it "generates disambiguation (file)" $ do
       -- Two knights can move to d2. Nbd2 vs Nfd2.
       -- White Knights at B1, F1. Target D2.
       let fenStr = "rnbqkbnr/pppppppp/8/8/8/8/PPPNNPPP/R1BQKB1R w KQkq - 0 1" -- Wrong fen, knights on d2? No.
       -- Let's place Knights on B1 and F1. (Standard)
       -- Target D2 (Standard empty).
       -- Both can jump to D2.
       -- Move B1-D2 -> Nbd2.
       let (Just (b, gs)) = Fen.parseFen startingFEN
       let m = Move B1 C3 Nothing -- Standard opening Nc3. Only one knight can reach C3? No, nothing else.
       San.san b gs m `shouldBe` "Nc3"

       -- Setup ambiguity. Knights on B1 and D1. Target C3.
       let fenStr2 = "rnbqkbnr/pppppppp/8/8/8/8/PPP1PPPP/RN1QKBNR w KQkq - 0 1" -- Knights B1, G1 standard.
       -- Let's manually place Knight on D1.
       let b1 = Board.putPiece Board.empty B1 (Piece White Knight)
       let b2 = Board.putPiece b1 D1 (Piece White Knight)
       let gs = GS.initialGameState
       let m = Move B1 C3 Nothing
       -- MoveGen for C3? Legal moves? Kings must be present for check?
       -- If no Kings, isLegal fails or crashes?
       -- isLegal checks kingSquare. If Nothing, returns False.
       -- So I need Kings on board.
       let b3 = Board.putPiece b2 E1 (Piece White King)
       let b4 = Board.putPiece b3 E8 (Piece Black King)

       San.san b4 gs m `shouldBe` "Nbc3"

    it "parses san" $ do
       let (Just (b, gs)) = Fen.parseFen startingFEN
       San.parseSan b gs "e4" `shouldBe` Just (Move E2 E4 Nothing)
       San.parseSan b gs "Nf3" `shouldBe` Just (Move G1 F3 Nothing)

    it "parses san castling" $ do
       let fenStr = "r3k2r/8/8/8/8/8/8/R3K2R w KQkq - 0 1"
       let (Just (b, gs)) = Fen.parseFen fenStr
       San.parseSan b gs "O-O" `shouldBe` Just (Move E1 G1 Nothing)
       San.parseSan b gs "O-O-O" `shouldBe` Just (Move E1 C1 Nothing)
