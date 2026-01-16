module Main (main) where

import Test.Hspec
import Data.Bits
import Chess.Types
import Chess.Bitboard
import qualified Chess.SquareSet as SS
import qualified Chess.Board.Base as Board
import qualified Chess.Board.GameState as GS
import qualified Chess.Board.Fen as Fen

main :: IO ()
main = hspec $ do
  describe "colorName" $ do
    it "returns white for White" $
      colorName White `shouldBe` "white"
    it "returns black for Black" $
      colorName Black `shouldBe` "black"

  describe "pieceSymbol" $ do
    it "returns symbol for Queen" $
      pieceSymbol Queen `shouldBe` 'Q'

  describe "unicodeSymbol" $ do
    it "returns correct unicode symbol for White King" $
      unicodeSymbol White King `shouldBe` '♔'
    it "returns correct unicode symbol for Black Queen" $
      unicodeSymbol Black Queen `shouldBe` '♛'

  describe "square parsing" $ do
    it "parses and shows a1" $
      fmap show (parseSquare "a1") `shouldBe` Just "a1"
    it "parses invalid" $
      parseSquare "i9" `shouldBe` Nothing

    describe "bitboard basics" $ do
      it "lsb of BB_E2 is square 12" $
        lsb BB_E2 `shouldBe` Just 12
      it "popcount of a file is 8" $
        popcount bbFileA `shouldBe` 8
      it "flipVertical A1 is A8" $
        flipVertical BB_A1 `shouldBe` BB_A8
      it "shiftUp from A2 gives A3" $
        shiftUp BB_A2 `shouldBe` BB_A3

    describe "square utilities" $ do
      it "mirror of A1 is H8" $
        squareMirror A1 `shouldBe` H8
      it "distance from A1 to H8 is 7" $
        squareDistance A1 H8 `shouldBe` 7
      it "manhattan distance from A1 to C2 is 3" $
        squareManhattanDistance A1 C2 `shouldBe` 3
      it "knight distance B1 to C3 is 1" $
        squareKnightDistance B1 C3 `shouldBe` 1

    describe "attack tables" $ do
      it "knight attacks from B1 include A3 and C3" $
        let a = knightAttacks B1
        in a .&. (bbFromSquare A3 .|. bbFromSquare C3) `shouldBe`
             (bbFromSquare A3 .|. bbFromSquare C3)
      it "king attacks from E4 include F5" $
        kingAttacks E4 .&. bbFromSquare F5 `shouldBe` bbFromSquare F5
      it "white pawn attacks from E2 include D3" $
        pawnAttacks White E2 .&. bbFromSquare D3 `shouldBe` bbFromSquare D3
      it "black pawn attacks from E7 include D6" $
        pawnAttacks Black E7 .&. bbFromSquare D6 `shouldBe` bbFromSquare D6

    describe "SquareSet" $ do
      it "empty is empty" $
        SS.null SS.empty `shouldBe` True
      it "singleton contains element" $
        SS.member A1 (SS.singleton A1) `shouldBe` True
      it "insert adds element" $
        SS.member A1 (SS.insert A1 SS.empty) `shouldBe` True
      it "delete removes element" $
        SS.member A1 (SS.delete A1 (SS.singleton A1)) `shouldBe` False
      it "fromList/toList roundtrip" $
        SS.toList (SS.fromList [A1, C3, H8]) `shouldBe` [A1, C3, H8]
      it "union combines sets" $
        let s1 = SS.singleton A1
            s2 = SS.singleton H8
        in SS.toList (SS.union s1 s2) `shouldBe` [A1, H8]
      it "intersection finds common" $
        let s1 = SS.fromList [A1, B2]
            s2 = SS.fromList [B2, C3]
        in SS.toList (SS.intersection s1 s2) `shouldBe` [B2]
      it "difference removes elements" $
        let s1 = SS.fromList [A1, B2]
            s2 = SS.singleton B2
        in SS.toList (SS.difference s1 s2) `shouldBe` [A1]
      it "size is correct" $
        SS.size (SS.fromList [A1, B2, C3]) `shouldBe` 3
      it "subset check" $
        SS.isSubsetOf (SS.singleton A1) (SS.fromList [A1, B2]) `shouldBe` True

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

  describe "Board.GameState" $ do
    it "initial game state has correct defaults" $ do
      let gs = GS.initialGameState
      GS.turn gs `shouldBe` White
      GS.castlingRights gs `shouldBe` GS.allCastling
      GS.epSquare gs `shouldBe` Nothing
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

  describe "Board.Fen" $ do
    it "parses starting FEN correctly" $ do
      let res = Fen.parseFen startingFEN
      res `shouldNotBe` Nothing
      let Just (b, gs) = res
      -- Check a few pieces
      Board.pieceAt b E1 `shouldBe` Just (Piece White King)
      Board.pieceAt b E8 `shouldBe` Just (Piece Black King)
      Board.pieceAt b A1 `shouldBe` Just (Piece White Rook)
      -- Check game state
      GS.turn gs `shouldBe` White
      GS.castlingRights gs `shouldBe` GS.allCastling
      GS.epSquare gs `shouldBe` Nothing

    it "roundtrips starting FEN" $ do
      let fenStr = startingFEN
          Just (b, gs) = Fen.parseFen fenStr
          fenStr' = Fen.fen b gs
      fenStr' `shouldBe` fenStr

    it "parses custom FEN with EP and castling" $ do
      let fenStr = "rnbqkbnr/ppp1pppp/8/3pP3/8/8/PPPP1PPP/RNBQKBNR b KQkq e6 0 2"
      let Just (b, gs) = Fen.parseFen fenStr
      GS.turn gs `shouldBe` Black
      GS.epSquare gs `shouldBe` Just E6
      GS.fullmoveNumber gs `shouldBe` 2

    it "roundtrips custom FEN" $ do
      let fenStr = "rnbqkbnr/ppp1pppp/8/3pP3/8/8/PPPP1PPP/RNBQKBNR b KQkq e6 0 2"
      let Just (b, gs) = Fen.parseFen fenStr
      let fenStr' = Fen.fen b gs
      fenStr' `shouldBe` fenStr
