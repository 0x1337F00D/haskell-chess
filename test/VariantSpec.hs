{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module VariantSpec where

import Test.Hspec
import Chess.Core.Board
import Chess.Core.Game
import Chess.Core.Move
import Chess.Core.Rules
import Chess.Core.Board.Internal hiding (movePiece)
import Chess.Core.Game.Internal (ActiveGame(..), Game(..), Variant(..))
import Chess.Core.Move.Internal (Move(..))
import qualified Data.Map as Map

spec :: Spec
spec = describe "New Variants" $ do
  describe "Antichess" $ do
    it "initialAntichessGame setup" $ do
      case initialAntichessGame of
        InProgressGame ag -> do
             whiteKing (gameBoard ag) `shouldBe` Just (Square FileE Rank1)

    it "Enforces mandatory capture" $ do
      -- Setup: White Rook at A1, Black Rook at A2 (capture available).
      -- Also White Rook at H1 (move H1-H2 available but not capture).
      let b = initialBoard
            { pawns = Map.empty
            , whitePieces = Map.fromList [(Square FileA Rank1, MRook), (Square FileH Rank1, MRook)]
            , blackPieces = Map.fromList [(Square FileA Rank2, MRook)]
            }

      let ag :: ActiveGame 'Antichess 'White 'Safe
          ag = ActiveGame
               { gameBoard = b
               , internalBoard = toBaseBoard b
               , castlingRights = CastlingRights False False False False
               , enPassantTarget = Nothing
               , halfMoveClock = 0
               , fullMoveNumber = 1
               , variantState = ()
               }

      let moves = generateLegalMoves ag
      let capture = StandardMove (Square FileA Rank1) (Square FileA Rank2)
      let quiet = StandardMove (Square FileH Rank1) (Square FileH Rank2)

      moves `shouldContain` [capture]
      moves `shouldNotContain` [quiet]

    it "Win by Stalemate (No moves available)" $ do
      -- Setup: White Pawn A2. Black Pawn A4.
      -- Black moves A4-A3, blocking White.
      -- Result should be Checkmate (Winner White) because White has no moves.

      let bPre = initialBoard
             { pawns = Map.fromList [((FileA, PRank2), White), ((FileA, PRank4), Black)]
             , whitePieces = Map.empty
             , blackPieces = Map.empty
             , whiteKing = Nothing
             , blackKing = Nothing
             }

      let agPre :: ActiveGame 'Antichess 'Black 'Safe
          agPre = ActiveGame
               { gameBoard = bPre
               , internalBoard = toBaseBoard bPre
               , castlingRights = CastlingRights False False False False
               , enPassantTarget = Nothing
               , halfMoveClock = 0
               , fullMoveNumber = 1
               , variantState = ()
               }

      let move = StandardMove (Square FileA Rank4) (Square FileA Rank3)
      let res = applyMove move agPre

      case res of
        Checkmate (Winner White) -> return ()
        _ -> expectationFailure $ "Expected White Win (Stalemate), got " ++ show res

  describe "Horde" $ do
    it "initialHordeGame setup" $ do
      case initialHordeGame of
        InProgressGame ag -> do
             whiteKing (gameBoard ag) `shouldBe` Nothing
             blackKing (gameBoard ag) `shouldBe` Just (Square FileE Rank8)
             -- Check Rank 1 Pawns
             case getPieceAt (Square FileE Rank1) (gameBoard ag) of
                Just (SomePiece WPawn) -> return ()
                _ -> expectationFailure "Expected White Pawn at E1"
             -- Check Rank 5 Pawn (e.g. F5)
             case getPieceAt (Square FileF Rank5) (gameBoard ag) of
                Just (SomePiece WPawn) -> return ()
                _ -> expectationFailure "Expected White Pawn at F5"

    it "White Pawns on Rank 1 can double push (unblocked)" $ do
      let b = initialBoard
            { pawns = Map.fromList [((FileE, PRank1), White)]
            , whitePieces = Map.empty
            , blackPieces = Map.empty
            , whiteKing = Nothing
            , blackKing = Just (Square FileA Rank8)
            }

      let ag :: ActiveGame 'Horde 'White 'Safe
          ag = ActiveGame
               { gameBoard = b
               , internalBoard = toBaseBoard b
               , castlingRights = CastlingRights False False False False
               , enPassantTarget = Nothing
               , halfMoveClock = 0
               , fullMoveNumber = 1
               , variantState = ()
               }

      let moves = generateLegalMoves ag
      let e1e3 = StandardMove (Square FileE Rank1) (Square FileE Rank3)
      moves `shouldContain` [e1e3]

    it "Black wins if White pieces = 0" $ do
      -- Setup: White Pawn A1. Black Rook A8.
      -- Black captures A1.
      let b = initialBoard
            { pawns = Map.fromList [((FileA, PRank1), White)]
            , whitePieces = Map.empty
            , blackPieces = Map.fromList [(Square FileA Rank8, MRook)]
            , whiteKing = Nothing
            }

      let ag :: ActiveGame 'Horde 'Black 'Safe
          ag = ActiveGame
               { gameBoard = b
               , internalBoard = toBaseBoard b
               , castlingRights = CastlingRights False False False False
               , enPassantTarget = Nothing
               , halfMoveClock = 0
               , fullMoveNumber = 1
               , variantState = ()
               }

      let move = StandardMove (Square FileA Rank8) (Square FileA Rank1)
      let res = applyMove move ag
      case res of
        Checkmate (Winner Black) -> return ()
        _ -> expectationFailure "Expected Black Win (All White pieces captured)"
