module Book.PolyglotSpec (spec) where

import Test.Hspec
import Data.Word (Word64)
import qualified Data.ByteString.Lazy as BL
import Data.Binary.Put (runPut, putWord64be, putWord16be, putWord32be)
import System.Directory (removeFile)
import Control.Exception (bracket)

import Chess.Book.Polyglot
import Chess.Board.Fen (parseFen)
import Chess.Board.Base (Board)
import Chess.Board.GameState (GameState)

spec :: Spec
spec = do
  describe "polyglotKey" $ do
    it "calculates correct key for starting position" $ do
      checkKey "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1" 0x463b96181691fc9c

    it "calculates correct key after e2e4" $ do
      checkKey "rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq e3 0 1" 0x823c9b50fd114196

    it "calculates correct key after e2e4 d7d5" $ do
      checkKey "rnbqkbnr/ppp1pppp/8/3p4/4P3/8/PPPP1PPP/RNBQKBNR w KQkq d6 0 2" 0x0756b94461c50fb0

    it "calculates correct key after e2e4 d7d5 e4e5" $ do
      checkKey "rnbqkbnr/ppp1pppp/8/3pP3/8/8/PPPP1PPP/RNBQKBNR b KQkq - 0 2" 0x662fafb965db29d4

    it "calculates correct key after e2e4 d7d5 e4e5 f7f5 (en passant available)" $ do
      checkKey "rnbqkbnr/ppp1p1pp/8/3pPp2/8/8/PPPP1PPP/RNBQKBNR w KQkq f6 0 3" 0x22a48b5a8e47ff78

    it "calculates correct key after e2e4 d7d5 e4e5 f7f5 e1e2" $ do
      checkKey "rnbqkbnr/ppp1p1pp/8/3pPp2/8/8/PPPPKPPP/RNBQ1BNR b kq - 0 3" 0x652a607ca3f242c1

    it "calculates correct key after e2e4 d7d5 e4e5 f7f5 e1e2 e8f7" $ do
      checkKey "rnbq1bnr/ppp1pkpp/8/3pPp2/8/8/PPPPKPPP/RNBQ1BNR w - - 0 4" 0x00fdd303c946bdd9

    it "calculates correct key after a2a4 b7b5 h2h4 b5b4 c2c4 (en passant available)" $ do
      checkKey "rnbqkbnr/p1pppppp/8/8/PpP4P/8/1P1PPPP1/RNBQKBNR b KQkq c3 0 3" 0x3c8123ea7b067637

    it "calculates correct key after a2a4 b7b5 h2h4 b5b4 c2c4 b4c3 a1a3" $ do
      checkKey "rnbqkbnr/p1pppppp/8/8/P6P/R1p5/1P1PPPP1/1NBQKBNR b Kkq - 0 4" 0x5c3f9b829b279560

  describe "readPolyglotBook" $ do
    it "finds entries in a book file" $ do
      let
        entries =
          [ BookEntry 0x1000 1 10 0
          , BookEntry 0x2000 2 20 0
          , BookEntry 0x2000 3 30 0 -- Duplicate key
          , BookEntry 0x3000 4 40 0
          ]

        encodeEntry :: BookEntry -> BL.ByteString
        encodeEntry (BookEntry k m w l) = runPut $ do
          putWord64be k
          putWord16be m
          putWord16be w
          putWord32be l

        fileContent = mconcat (map encodeEntry entries)
        path = "test_book.bin"

      bracket (BL.writeFile path fileContent) (\_ -> removeFile path) $ \_ -> do
        res1 <- readPolyglotBook path 0x1000
        res1 `shouldBe` [BookEntry 0x1000 1 10 0]

        res2 <- readPolyglotBook path 0x2000
        res2 `shouldBe` [BookEntry 0x2000 2 20 0, BookEntry 0x2000 3 30 0]

        res3 <- readPolyglotBook path 0x3000
        res3 `shouldBe` [BookEntry 0x3000 4 40 0]

        res4 <- readPolyglotBook path 0x1500 -- Not found
        res4 `shouldBe` []

checkKey :: String -> Word64 -> Expectation
checkKey fen expectedKey = do
  case parseFen fen of
    Nothing -> expectationFailure $ "Failed to parse FEN: " ++ fen
    Just (b, gs) -> polyglotKey b gs `shouldBe` expectedKey
