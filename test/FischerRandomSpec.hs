{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module FischerRandomSpec where

import Test.Hspec
import Chess.Core.Rules.FischerRandom
import Chess.Core.Rules.Class
import Chess.Core.Game.Internal
import Chess.Core.Move.Internal
import Chess.Core.Board.Internal hiding (Board)
import qualified Chess.Types as T
import qualified Chess.Board.Base as Base
import Data.Maybe (isJust)
import Data.Bits (popCount, testBit)

spec :: Spec
spec = describe "Fischer Random" $ do
  describe "FEN Parsing" $ do
    it "parses Shredder-FEN with letter castling rights" $ do
      -- Position 266 (BBRKRNQN) from random generator example logic
      -- "bbqnnrkr/pppppppp/8/8/8/8/PPPPPPPP/BBQNNRKR w FHfh - 0 1"
      -- White Rooks: F1 (5), H1 (7). King: G1 (6).
      -- Castling rights: FHfh (F-file and H-file rooks)
      let fen = "bbqnnrkr/pppppppp/8/8/8/8/PPPPPPPP/BBQNNRKR w FHfh - 0 1"
      let game = fischerRandomGameFromFEN fen
      game `shouldSatisfy` isJust
      case game of
        Just (InProgressGame ag) -> do
          let frState = variantState ag
          -- Verify Rooks in State
          let wRooks = whiteRookFiles frState
          let bRooks = blackRookFiles frState
          popCount wRooks `shouldBe` 2
          popCount bRooks `shouldBe` 2
          -- F1 and H1 should be set for White
          testBit wRooks (T.unSquare T.F1) `shouldBe` True
          testBit wRooks (T.unSquare T.H1) `shouldBe` True
        _ -> expectationFailure "Expected InProgressGame"

  describe "Move Generation" $ do
    it "generates castling moves correctly for clear path" $ do
      -- King E1, Rooks A1, H1. Empty rank 1 otherwise.
      -- FEN: 8/8/8/8/8/8/8/R3K2R w KQ - 0 1
      let fen = "8/8/8/8/8/8/8/R3K2R w KQ - 0 1"
      case fischerRandomGameFromFEN fen of
        Just (InProgressGame ag) -> do
          let moves = generateMoves ag
          let castlingMoves = [ m | m@(Castling960Move _ _) <- moves ]

          -- Expect Castling960Move E1 H1 (King-side)
          let hasK = any (\m -> case m of Castling960Move k r -> k == Square FileE Rank1 && r == Square FileH Rank1; _ -> False) castlingMoves
          -- Expect Castling960Move E1 A1 (Queen-side)
          let hasQ = any (\m -> case m of Castling960Move k r -> k == Square FileE Rank1 && r == Square FileA Rank1; _ -> False) castlingMoves

          hasK `shouldBe` True
          hasQ `shouldBe` True
        _ -> expectationFailure "Expected InProgressGame"

  describe "Move Execution" $ do
    it "executes King-side castling move correctly" $ do
      let fen = "bbqnnrkr/pppppppp/8/8/8/8/PPPPPPPP/BBQNNRKR w FHfh - 0 1"
      case fischerRandomGameFromFEN fen of
        Just (InProgressGame (ag :: ActiveGame 'FischerRandom c s)) ->
          case sColor @c of
            SWhite -> do
              -- Kingside Castling: King G1 -> G1, Rook H1 -> F1
              let m = Castling960Move (Square FileG Rank1) (Square FileH Rank1)
              let res = executeMove m ag

              case res of
                Continue nextAg -> do
                   let b = internalBoard nextAg
                   -- Check King at G1
                   Base.pieceAt b T.G1 `shouldBe` Just (T.Piece T.White T.King)
                   -- Check Rook at F1
                   Base.pieceAt b T.F1 `shouldBe` Just (T.Piece T.White T.Rook)
                   -- Check H1 is empty
                   Base.pieceAt b T.H1 `shouldBe` Nothing
                _ -> expectationFailure "Expected Continue"
            _ -> expectationFailure "Expected White turn"
        _ -> expectationFailure "Expected InProgressGame"

    it "executes Queen-side castling move correctly" $ do
      let fen = "bbqnnrkr/pppppppp/8/8/8/8/PPPPPPPP/BBQNNRKR w FHfh - 0 1"
      case fischerRandomGameFromFEN fen of
        Just (InProgressGame (ag :: ActiveGame 'FischerRandom c s)) ->
          case sColor @c of
            SWhite -> do
              -- Queenside Castling: King G1 -> C1, Rook F1 -> D1
              let m = Castling960Move (Square FileG Rank1) (Square FileF Rank1)
              let res = executeMove m ag

              case res of
                Continue nextAg -> do
                   let b = internalBoard nextAg
                   -- Check King at C1
                   Base.pieceAt b T.C1 `shouldBe` Just (T.Piece T.White T.King)
                   -- Check Rook at D1
                   Base.pieceAt b T.D1 `shouldBe` Just (T.Piece T.White T.Rook)
                   -- Check F1 is empty (Rook moved)
                   Base.pieceAt b T.F1 `shouldBe` Nothing
                   -- Check G1 is empty (King moved)
                   Base.pieceAt b T.G1 `shouldBe` Nothing
                _ -> expectationFailure "Expected Continue"
            _ -> expectationFailure "Expected White turn"
        _ -> expectationFailure "Expected InProgressGame"
