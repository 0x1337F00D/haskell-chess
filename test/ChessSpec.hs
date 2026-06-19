module ChessSpec (spec) where

import Test.Hspec
import Chess
import qualified Chess.Board.GameState as GS
import qualified Chess.Board.Zobrist as Zobrist
import Data.Maybe (isJust, fromJust)

spec :: Spec
spec = describe "Chess (High-Level API)" $ do
  describe "Board" $ do
    it "initialBoard has correct FEN" $ do
      fen initialBoard `shouldBe` startingFEN

    it "parseFen roundtrips starting FEN" $ do
      let mb = parseFen startingFEN
      mb `shouldSatisfy` isJust
      let b = fromJust mb
      fen b `shouldBe` startingFEN

  describe "Moves" $ do
    it "generates legal moves" $ do
      let moves = legalMoves initialBoard
      length moves `shouldBe` 20

    it "applyMove updates board and state" $ do
      let b = initialBoard
      -- e2e4
      let m = fromJust $ parseSan b "e4"
      let b' = applyMove b m

      -- Check FEN parts
      -- Board: rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR
      -- Turn: b
      -- Castling: KQkq
      -- EP: e3
      -- Halfmove: 0 (pawn move)
      -- Fullmove: 1 (still 1 until black moves)
      fen b' `shouldBe` "rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq e3 0 1"

    it "applyMove updates fullmove number" $ do
      let b0 = initialBoard
      -- 1. e4
      let m1 = fromJust $ parseSan b0 "e4"
      let b1 = applyMove b0 m1
      -- 1... e5
      let m2 = fromJust $ parseSan b1 "e5"
      let b2 = applyMove b1 m2

      -- FEN should show fullmove 2
      let fenStr = fen b2
      last (words fenStr) `shouldBe` "2"

    it "applyMove handles null moves as an engine pass" $ do
      let b' = applyMove initialBoard NullMove
      fen b' `shouldBe` "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR b KQkq - 1 1"
      GS.zobristHash (state b') `shouldBe` Zobrist.computeHash (pieces b') (state b')

    it "applyMove clears en passant and advances fullmove on black null move" $ do
      let b0 = initialBoard
      let m1 = fromJust $ parseSan b0 "e4"
      let b1 = applyMove b0 m1
      let b2 = applyMove b1 NullMove
      fen b2 `shouldBe` "rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR w KQkq - 1 2"
      GS.zobristHash (state b2) `shouldBe` Zobrist.computeHash (pieces b2) (state b2)

    it "handles checkmate" $ do
      -- Fool's mate
      let b0 = initialBoard
      -- 1. f3
      let m1 = fromJust $ parseSan b0 "f3"
      let b1 = applyMove b0 m1
      -- 1... e5
      let m2 = fromJust $ parseSan b1 "e5"
      let b2 = applyMove b1 m2
      -- 2. g4
      let m3 = fromJust $ parseSan b2 "g4"
      let b3 = applyMove b2 m3
      -- 2... Qh4#
      let m4 = fromJust $ parseSan b3 "Qh4#"
      let b4 = applyMove b3 m4

      isCheckmate b4 `shouldBe` True
      outcome b4 `shouldBe` Just (Outcome Checkmate (Just Black))

  describe "Notation" $ do
    it "SAN parsing matches generation" $ do
       let b = initialBoard
       let mStr = "e4"
       let m = fromJust $ parseSan b mStr
       san b m `shouldBe` mStr
