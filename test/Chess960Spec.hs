{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
module Chess960Spec where

import Test.Hspec
import Chess.Core.Game.Internal
import Chess.Core.Rules
import Chess.Core.Board.Internal
import Chess.Core.Move
import Chess.Core.Move.Internal (Move(..))
import qualified Chess.Types as T

spec :: Spec
spec = do
  describe "Chess 960 (Fischer Random)" $ do
    it "parses FEN with 960 castling rights" $ do
      let fen = "rkr5/8/8/8/8/8/8/RKR5 w CAca - 0 1"
          res = fischerRandomGameFromFEN fen

      case res of
        Just (InProgressGame ag) -> do
          let (wk, wq, bk, bq) = variantState ag
          whiteKing (gameBoard ag) `shouldBe` Square FileB Rank1
          wk `shouldBe` Just (Square FileC Rank1)
          wq `shouldBe` Just (Square FileA Rank1)
        Nothing -> expectationFailure "Parse failed"

    it "generates 960 castling moves" $ do
      let fen = "8/4k3/8/8/8/8/8/RKR5 w CAca - 0 1"

      case fischerRandomGameFromFEN fen of
        Just (InProgressGame ag) -> do
          let moves = generateMoves ag
              castlingMoves = [ m | m@Castling960Move{} <- moves ]
          -- Should have KS and QS castling
          -- KS: B1->G1. QS: B1->C1.
          length castlingMoves `shouldSatisfy` (>= 1)
        Nothing -> expectationFailure "Parse failed"
