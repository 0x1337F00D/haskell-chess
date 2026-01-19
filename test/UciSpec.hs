module UciSpec (spec) where

import Test.Hspec
import Chess.Uci

spec :: Spec
spec = do
  describe "UCI Parser" $ do
    it "parses id command" $ do
        parseEngineLine "id name Stockfish 15" `shouldBe` Id "Stockfish 15" ""
        parseEngineLine "id author T. Romstad" `shouldBe` Id "" "T. Romstad"

    it "parses uciok/readyok" $ do
        parseEngineLine "uciok" `shouldBe` UciOk
        parseEngineLine "readyok" `shouldBe` ReadyOk

    it "parses bestmove" $ do
        parseEngineLine "bestmove e2e4" `shouldBe` BestMove "e2e4" Nothing
        parseEngineLine "bestmove e2e4 ponder e7e5" `shouldBe` BestMove "e2e4" (Just "e7e5")

    it "parses info simple" $ do
        let expected = InfoLine (defaultInfo { depth = Just 20, score = Just (Cp 50) })
        let parsed = parseEngineLine "info depth 20 score cp 50"
        parsed `shouldBe` expected

    it "parses info complex" $ do
        let expected = InfoLine (defaultInfo {
            depth = Just 30,
            seldepth = Just 40,
            time = Just 1000,
            nodes = Just 500000,
            nps = Just 500,
            score = Just (Mate 5),
            pv = ["e2e4", "c7c5", "g1f3"]
        })
        let parsed = parseEngineLine "info depth 30 seldepth 40 score mate 5 time 1000 nodes 500000 nps 500 pv e2e4 c7c5 g1f3"
        parsed `shouldBe` expected

    it "parses info string" $ do
        let expected = InfoLine (defaultInfo { infoString = Just "currmove e2e4 currmovenumber 1" })
        parseEngineLine "info string currmove e2e4 currmovenumber 1" `shouldBe` expected

    it "parses info with multipv" $ do
        let expected = InfoLine (defaultInfo { multicpv = Just 1, score = Just (Cp 10), pv = ["e2e4"] })
        parseEngineLine "info multicpv 1 score cp 10 pv e2e4" `shouldBe` expected
