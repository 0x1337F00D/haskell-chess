{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module FischerRandomSpec (spec) where

import Test.Hspec
import Chess.Core.Rules
import Chess.Core.Game.Internal
import Chess.Core.Board.Internal
import Chess.Core.Move.Internal
import Chess.Core.Move (toUCI)
import qualified Chess.Types as T
import qualified Chess.Board.Base as Base
import Data.Maybe (isJust, fromJust)
import Data.List (find)

-- | Helper to refine color constraint.
withOpposite :: forall c r. KnownColor c => (KnownColor (Opposite c) => r) -> r
withOpposite f = case sColor @c of
  SWhite -> f
  SBlack -> f

spec :: Spec
spec = do
  describe "Fischer Random Chess (Chess960)" $ do
    it "parses standard start position as 960" $ do
      let fen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
      let game = fischerRandomGameFromFEN fen
      game `shouldSatisfy` isJust
      case game of
        Just (InProgressGame ag) -> do
          let moves = generateMoves ag
          -- Check for castling moves
          let castlingMoves = filter isCastling960 moves
          length castlingMoves `shouldBe` 0 -- blocked by pawns/pieces
        _ -> expectationFailure "Failed to parse game"

    it "generates castling moves in open position" $ do
      -- Remove pieces to allow castling
      -- Rooks at A1, H1. King at E1.
      -- FEN: "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/R3K2R w KQkq - 0 1"
      let fen = "r3k2r/pppppppp/8/8/8/8/PPPPPPPP/R3K2R w KQkq - 0 1"
      let game = fischerRandomGameFromFEN fen
      case game of
        Just (InProgressGame ag) -> do
          let moves = generateMoves ag
          let castlingMoves = filter isCastling960 moves
          length castlingMoves `shouldBe` 2

          -- Check E1-G1 (H1 rook)
          let kSide = find (\m -> cm960Rook m == Square FileH Rank1) castlingMoves
          kSide `shouldSatisfy` isJust

          -- Check E1-C1 (A1 rook)
          let qSide = find (\m -> cm960Rook m == Square FileA Rank1) castlingMoves
          qSide `shouldSatisfy` isJust
        _ -> expectationFailure "Failed to parse game"

    it "executes castling correctly (King-side)" $ do
      let fen = "r3k2r/pppppppp/8/8/8/8/PPPPPPPP/R3K2R w KQkq - 0 1"
      case fischerRandomGameFromFEN fen of
        Just (InProgressGame (ag :: ActiveGame 'FischerRandom turn status)) -> do
          let moves = generateMoves ag
          let Just move = find (\m -> case m of Castling960Move _ r -> r == Square FileH Rank1; _ -> False) moves

          let res = withOpposite @turn (executeMove move ag)
          case res of
            Continue nextAg -> do
               let b = viewBoard nextAg
               -- King should be at G1
               whiteKing b `shouldBe` Square FileG Rank1
               -- Rook should be at F1
               Base.pieceAt (internalBoard nextAg) (toSquare (Square FileF Rank1)) `shouldSatisfy` (\p -> fmap T.pieceType p == Just T.Rook)
               -- Old squares empty
               Base.pieceAt (internalBoard nextAg) (toSquare (Square FileE Rank1)) `shouldBe` Nothing
               Base.pieceAt (internalBoard nextAg) (toSquare (Square FileH Rank1)) `shouldBe` Nothing
            _ -> expectationFailure "Move execution failed"
        _ -> expectationFailure "Failed to parse game"

    it "executes castling correctly (Queen-side)" $ do
      let fen = "r3k2r/pppppppp/8/8/8/8/PPPPPPPP/R3K2R w KQkq - 0 1"
      case fischerRandomGameFromFEN fen of
        Just (InProgressGame (ag :: ActiveGame 'FischerRandom turn status)) -> do
          let moves = generateMoves ag
          let Just move = find (\m -> case m of Castling960Move _ r -> r == Square FileA Rank1; _ -> False) moves

          let res = withOpposite @turn (executeMove move ag)
          case res of
            Continue nextAg -> do
               let b = viewBoard nextAg
               -- King should be at C1
               whiteKing b `shouldBe` Square FileC Rank1
               -- Rook should be at D1
               Base.pieceAt (internalBoard nextAg) (toSquare (Square FileD Rank1)) `shouldSatisfy` (\p -> fmap T.pieceType p == Just T.Rook)
            _ -> expectationFailure "Move execution failed"
        _ -> expectationFailure "Failed to parse game"

    it "parses Shredder-FEN castling rights (HAha)" $ do
      -- Custom position: Rooks at B1, G1. King at E1.
      -- FEN: "rn1qk1nr/pppppppp/8/8/8/8/PPPPPPPP/1R2K1R1 w HAha - 0 1"
      -- H -> G1 (file 6/G is H-side relative to B1/E1? No, H means H-file rook. But here G1 is file G.)
      -- Shredder-FEN uses File letters. So GBgb?
      -- If rooks are at B1 and G1. Rights are B and G.
      let fen = "1r2k1r1/pppppppp/8/8/8/8/PPPPPPPP/1R2K1R1 w GBgb - 0 1"
      let game = fischerRandomGameFromFEN fen
      game `shouldSatisfy` isJust
      case game of
        Just (InProgressGame ag) -> do
           -- Verify VariantState has rooks at B1 and G1
           let (wK, wQ, bK, bQ) = variantState ag
           -- Sorted: B1 (Left/Queen), G1 (Right/King)
           wQ `shouldBe` Just (Square FileB Rank1)
           wK `shouldBe` Just (Square FileG Rank1)
        _ -> expectationFailure "Failed to parse"

isCastling960 :: Move c -> Bool
isCastling960 (Castling960Move _ _) = True
isCastling960 _ = False
