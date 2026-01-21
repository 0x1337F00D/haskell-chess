module Board.ValidationSpec (spec) where

import Test.Hspec
import Chess.Types
import qualified Chess.Board.GameState as GS
import qualified Chess.Board.Fen as Fen
import qualified Chess.Board.Validation as Validation
import Data.Maybe (fromJust)

spec :: Spec
spec = do
  describe "Board.Validation" $ do
    it "detects check" $ do
      -- Fool's mate pattern: e4 g5, Nc3 f6, Qh5#
      -- But let's just construct a check position.
      -- White King E1, Black Rook E8.
      let fenStr = "4r3/8/8/8/8/8/8/4K3 w - - 0 1"
      let (b, gs) = fromJust $ Fen.parseFen fenStr
      Validation.isCheck b gs `shouldBe` True

    it "detects not check" $ do
      let (b, gs) = fromJust $ Fen.parseFen startingFEN
      Validation.isCheck b gs `shouldBe` False

    it "detects checkmate" $ do
      -- Fool's Mate: 1. f3 e5 2. g4 Qh4#
      -- Position: rnbqkbnr/pppp1ppp/8/4p3/6P1/5P2/PPPPP2P/RNBQKBNR b KQkq - 0 1 (White to move, this is wrong)
      -- White to move getting mated:
      -- r1bqkbnr/pppp1ppp/2n5/4p3/4P3/5P2/PPPP2PP/RNBQKBNR w KQkq - 1 2 (example)
      -- Actual Fool's Mate Final Position:
      -- rnb1kbnr/pppp1ppp/8/4p3/6Pq/5P2/PPPPP2P/RNBQKBNR w KQkq - 1 3
      let fenStr = "rnb1kbnr/pppp1ppp/8/4p3/6Pq/5P2/PPPPP2P/RNBQKBNR w KQkq - 1 3"
      let (b, gs) = fromJust $ Fen.parseFen fenStr
      Validation.isCheckmate b gs `shouldBe` True
      Validation.outcome b gs [] `shouldBe` Just (Outcome Checkmate (Just Black))

    it "detects stalemate" $ do
      -- Stalemate position: 8/8/8/8/8/7k/7p/7K w - - 0 1
      let fenStr = "8/8/8/8/8/7k/7p/7K w - - 0 1"
      let (b, gs) = fromJust $ Fen.parseFen fenStr
      Validation.isStalemate b gs `shouldBe` True
      Validation.isCheckmate b gs `shouldBe` False
      Validation.outcome b gs [] `shouldBe` Just (Outcome Stalemate Nothing)

    it "detects threefold repetition" $ do
      let (b, gs) = fromJust $ Fen.parseFen startingFEN
      let rep = Validation.PositionRep b (GS.turn gs) (GS.castlingRights gs) (GS.epSquare gs)
      -- 2 repetitions in history + 1 current = 3
      let history = [rep, rep]
      Validation.isThreefoldRepetition b gs history `shouldBe` True
      Validation.outcome b gs history `shouldBe` Just (Outcome ThreefoldRepetition Nothing)

    it "detects fifty moves" $ do
      let gs = GS.initialGameState { GS.halfmoveClock = 100 }
      Validation.isFiftyMoves gs `shouldBe` True
      -- Use starting board which has legal moves, so not stalemate
      let (b, _) = fromJust $ Fen.parseFen startingFEN
      Validation.outcome b gs [] `shouldBe` Just (Outcome FiftyMoves Nothing)

    it "detects insufficient material K vs K" $ do
      let fenStr = "8/8/8/8/8/8/3k4/4K3 w - - 0 1"
      let (b, gs) = fromJust $ Fen.parseFen fenStr
      Validation.hasInsufficientMaterial b `shouldBe` True
      Validation.outcome b gs [] `shouldBe` Just (Outcome InsufficientMaterial Nothing)

    it "detects insufficient material K+N vs K" $ do
      let fenStr = "8/8/8/8/8/5n2/3k4/4K3 w - - 0 1"
      let (b, _) = fromJust $ Fen.parseFen fenStr
      Validation.hasInsufficientMaterial b `shouldBe` True

    it "detects insufficient material K+B vs K" $ do
      let fenStr = "8/8/8/8/8/5b2/3k4/4K3 w - - 0 1"
      let (b, _) = fromJust $ Fen.parseFen fenStr
      Validation.hasInsufficientMaterial b `shouldBe` True

    it "detects insufficient material K+B vs K+B same color" $ do
      -- White B on C1 (Dark), Black B on F8 (Dark)
      let fenStr = "5b2/8/8/8/8/8/3k4/2B1K3 w - - 0 1"
      let (b, _) = fromJust $ Fen.parseFen fenStr
      Validation.hasInsufficientMaterial b `shouldBe` True

    it "detects SUFFICIENT material K+B vs K+B opposite color" $ do
      -- White B on C1 (Dark), Black B on E8 (Light - E8 is white? Wait. A1 is black? No A1 is dark usually? standard board: A1 black, H1 white? No.
      -- A1 is BLACK (0). H1 is WHITE.
      -- C1 is (2,0) -> 2+0=2 even -> Black(Dark).
      -- E8 is (4,7) -> 4+7=11 odd -> White(Light).
      -- So C1 and E8 are opposite colors.
      let fenStr = "4b3/8/8/8/8/8/3k4/2B1K3 w - - 0 1"
      let (b, _) = fromJust $ Fen.parseFen fenStr
      Validation.hasInsufficientMaterial b `shouldBe` False

    it "detects SUFFICIENT material K+P vs K" $ do
      let fenStr = "8/8/8/8/8/4P3/3k4/4K3 w - - 0 1"
      let (b, _) = fromJust $ Fen.parseFen fenStr
      Validation.hasInsufficientMaterial b `shouldBe` False

    it "detects insufficient material K+N vs K+B" $ do
      -- White Knight, Black Bishop
      let fenStr = "8/8/8/8/8/5b2/3k2N1/4K3 w - - 0 1"
      let (b, _) = fromJust $ Fen.parseFen fenStr
      Validation.hasInsufficientMaterial b `shouldBe` True

    it "detects insufficient material K+B vs K+N" $ do
      -- White Bishop, Black Knight
      let fenStr = "8/8/8/8/8/5n2/3k2B1/4K3 w - - 0 1"
      let (b, _) = fromJust $ Fen.parseFen fenStr
      Validation.hasInsufficientMaterial b `shouldBe` True

    it "detects SUFFICIENT material K+N vs K+N" $ do
      -- White Knight, Black Knight
      let fenStr = "8/8/8/8/8/5n2/3k2N1/4K3 w - - 0 1"
      let (b, _) = fromJust $ Fen.parseFen fenStr
      Validation.hasInsufficientMaterial b `shouldBe` False

    it "detects SUFFICIENT material K+N+N vs K" $ do
      -- 2 White Knights
      let fenStr = "8/8/8/8/8/5N2/3k2N1/4K3 w - - 0 1"
      let (b, _) = fromJust $ Fen.parseFen fenStr
      Validation.hasInsufficientMaterial b `shouldBe` False
