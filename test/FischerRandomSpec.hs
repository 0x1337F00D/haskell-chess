{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}

module FischerRandomSpec where

import Test.Hspec
import Chess.Core.Game
import Chess.Core.Rules
import Chess.Core.Move
import Chess.Core.Board.Internal (Board(..), Square(..), File(..), Rank(..))
import Chess.Core.Game.Internal (Game(..), Variant(..), ActiveGame(..), CastlingRights(..))
import Chess.Core.Move.Internal (Move(..), MoveResult(..))
import qualified Data.Map as Map

spec :: Spec
spec = describe "Fischer Random Logic" $ do
    it "parses FEN with Shredder castling rights" $ do
        -- Rooks at A1, G1. King at E1.
        let fen = "r1b1k1r1/pppppppp/8/8/8/8/PPPPPPPP/R1B1K1R1 w AGag - 0 1"
        let game = fischerRandomGameFromFEN fen
        game `shouldSatisfy` \g -> case g of Just _ -> True; _ -> False

        case game of
          Just (InProgressGame ag) -> do
             let (wks, wqs, bks, bqs) = variantState ag
             -- King at E1.
             -- Rooks at A1 (Left), G1 (Right).
             -- KS Rook should be G1. QS Rook should be A1.
             wks `shouldBe` Just (Square FileG Rank1)
             wqs `shouldBe` Just (Square FileA Rank1)
             bks `shouldBe` Just (Square FileG Rank8)
             bqs `shouldBe` Just (Square FileA Rank8)

             let cr = castlingRights ag
             whiteKingSide cr `shouldBe` True
             whiteQueenSide cr `shouldBe` True

          _ -> expectationFailure "Failed to parse game"

    it "generates castling moves" $ do
        -- Setup: Rooks A1, H1. King E1. Standard setup but using Shredder rights.
        -- "HAha" -> H is KS, A is QS.
        let fen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w HAha - 0 1"
        case fischerRandomGameFromFEN fen of
           Just (InProgressGame ag) -> do
               let moves = generateMoves ag
               -- Should include Castling960Move E1 H1 (KS) and E1 A1 (QS)
               let ks = Castling960Move (Square FileE Rank1) (Square FileH Rank1)
               let qs = Castling960Move (Square FileE Rank1) (Square FileA Rank1)

               moves `shouldContain` [ks, qs]
           _ -> expectationFailure "Failed to parse"

    it "executes castling correctly (King to G1, Rook to F1)" $ do
        -- Standard setup.
        let fen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w HAha - 0 1"
        case fischerRandomGameFromFEN fen of
           Just (InProgressGame ag) -> do
               let move = Castling960Move (Square FileE Rank1) (Square FileH Rank1)
               let res = executeMove move ag

               case res of
                 Continue nextGame -> do
                    let vb = viewBoard nextGame
                    whiteKing vb `shouldBe` Just (Square FileG Rank1)
                    -- Check castling rights updated (lost)
                    let cr = castlingRights nextGame
                    whiteKingSide cr `shouldBe` False
                    whiteQueenSide cr `shouldBe` False
                 _ -> expectationFailure "Move failed"
           _ -> expectationFailure "Failed to parse"
