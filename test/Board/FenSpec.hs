module Board.FenSpec (spec) where

import Test.Hspec
import Chess.Types
import qualified Chess.Board.Base as Board
import qualified Chess.Board.GameState as GS
import qualified Chess.Board.Fen as Fen
import Data.List (dropWhileEnd)
import Data.Char (isSpace)

-- | Trim whitespace from both ends
trim :: String -> String
trim = dropWhile isSpace . dropWhileEnd isSpace

-- | Parse just the FEN part from EPD line (before the ';')
parseEpdFen :: String -> Maybe String
parseEpdFen line
    | null (trim line) = Nothing
    | head line == '#' = Nothing
    | otherwise =
        let (fenPart, _) = break (== ';') line
        in Just (trim fenPart)

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
      GS.epSquare gs `shouldBe` NoSquare
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
      GS.epSquare gs `shouldBe` E3
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

    it "preserves non-standard castling rights (CFcf)" $ do
      let fenStr = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w CFcf - 0 1"
      let parsed = Fen.parseFen fenStr
      parsed `shouldNotBe` Nothing
      let (Just (b, gs)) = parsed
      Fen.fen b gs `shouldBe` fenStr

    it "roundtrips all FENs from perftsuite.epd" $ do
        content <- readFile "test/gamefiles/perftsuite.epd"
        let fens = map parseEpdFen (lines content)
        mapM_ checkRoundtrip fens

    where
        checkRoundtrip Nothing = return ()
        checkRoundtrip (Just fenStr) = do
            let mbRes = Fen.parseFen fenStr
            case mbRes of
                Nothing -> expectationFailure $ "Failed to parse FEN: " ++ fenStr
                Just (b, gs) -> do
                    let fenStr2 = Fen.fen b gs
                    -- We check if they are equal.
                    -- Note: Some FEN generators might behave differently regarding whitespace or en passant dash.
                    -- But let's assume strict equality for now as pychess does.
                    if fenStr2 /= fenStr
                       then expectationFailure $ "Roundtrip failed.\nOriginal: " ++ fenStr ++ "\nGenerated: " ++ fenStr2
                       else return ()
