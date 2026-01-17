module Board.FenSpec (spec) where

import Test.Hspec
import Chess.Types
import qualified Chess.Board.Base as Board
import qualified Chess.Board.GameState as GS
import qualified Chess.Board.Fen as Fen

spec :: Spec
spec = do
  describe "Board.Fen" $ do
    it "parses starting FEN correctly" $ do
      let mbRes = Fen.parseFen startingFEN
      mbRes `shouldNotBe` Nothing
      let (Just (b, gs)) = mbRes
      -- Check piece placement (sampling)
      Board.pieceAt b E1 `shouldBe` Just (Piece White King)
      Board.pieceAt b E8 `shouldBe` Just (Piece Black King)
      Board.pieceAt b A1 `shouldBe` Just (Piece White Rook)
      -- Check state
      GS.turn gs `shouldBe` White
      GS.castlingRights gs `shouldBe` GS.allCastling
      GS.epSquare gs `shouldBe` Nothing
      GS.halfmoveClock gs `shouldBe` 0
      GS.fullmoveNumber gs `shouldBe` 1

    it "roundtrips starting FEN" $ do
      let mbRes = Fen.parseFen startingFEN
      let (Just (b, gs)) = mbRes
      Fen.fen b gs `shouldBe` startingFEN

    it "parses FEN with en passant and move counts" $ do
      let fenStr = "rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq e3 0 1"
      let (Just (b, gs)) = Fen.parseFen fenStr
      GS.turn gs `shouldBe` Black
      GS.epSquare gs `shouldBe` Just E3
      Board.pieceAt b E4 `shouldBe` Just (Piece White Pawn)

    it "handles empty castling" $ do
      let fenStr = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w - - 0 1"
      let (Just (_, gs)) = Fen.parseFen fenStr
      GS.castlingRights gs `shouldBe` GS.noCastling

    it "handles partial castling" $ do
      let fenStr = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w Kq - 0 1"
      let (Just (_, gs)) = Fen.parseFen fenStr
      GS.canCastleKingside gs White `shouldBe` True
      GS.canCastleQueenside gs White `shouldBe` False
      GS.canCastleKingside gs Black `shouldBe` False
      GS.canCastleQueenside gs Black `shouldBe` True
