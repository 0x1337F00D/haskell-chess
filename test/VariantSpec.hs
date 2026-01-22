{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}

module VariantSpec where

import Test.Hspec
import Chess.Core.Board
import Chess.Core.Game
import Chess.Core.Move
import Chess.Core.Rules
import Chess.Core.Board.Internal (Board(..), PawnRank(..), MajorMinorPiece(..), Color(..), Square(..), File(..), Rank(..))
import Chess.Core.Game.Internal (ActiveGame(..), Game(..), Variant(..), CastlingRights(..), Outcome(..))
import Chess.Core.Move.Internal (Move(..), MoveResult(..))
import qualified Data.Map as Map

spec :: Spec
spec = describe "Variant Logic" $ do
  describe "Antichess" $ do
    it "enforces mandatory captures" $ do
      -- Setup: White Pawn E4, Black Pawn D5. White to move.
      -- Capture E4xD5 is mandatory. E4-E5 is illegal.
      let b = initialBoard
            { pawns = Map.fromList [((FileE, PRank4), White), ((FileD, PRank5), Black)]
            , whitePieces = Map.empty
            , blackPieces = Map.empty
            , whiteKing = Just (Square FileE Rank1)
            , blackKing = Just (Square FileE Rank8)
            }
      -- Note: Antichess kings are just pieces, but we place them to have valid board (popcount > 0 usually needed by base board logic? No, base board allows 0 kings. Core.Board allows 0 kings now.)

      let ag :: ActiveGame 'Antichess 'White 'Safe
          ag = ActiveGame
               { internalBoard = toBaseBoard b
               , castlingRights = CastlingRights False False False False
               , enPassantTarget = Nothing
               , halfMoveClock = 0
               , fullMoveNumber = 1
               , variantState = ()
               }

      let moves = generateLegalMoves ag
      let capture = StandardMove (Square FileE Rank4) (Square FileD Rank5)
      let push = StandardMove (Square FileE Rank4) (Square FileE Rank5)

      moves `shouldContain` [capture]
      moves `shouldNotContain` [push]
      length moves `shouldBe` 1

    it "winning by losing all pieces" $ do
       -- Setup: White has 1 piece. Black captures it. White wins.

       let b = initialBoard
             { pawns = Map.empty
             , whitePieces = Map.fromList [(Square FileD Rank5, MRook)]
             , blackPieces = Map.fromList [(Square FileD Rank6, MRook)]
             , whiteKing = Nothing
             , blackKing = Nothing
             }

       let ag :: ActiveGame 'Antichess 'Black 'Safe
           ag = ActiveGame
                { internalBoard = toBaseBoard b
                , castlingRights = CastlingRights False False False False
                , enPassantTarget = Nothing
                , halfMoveClock = 0
                , fullMoveNumber = 1
                , variantState = ()
                }

       -- Black captures White Rook: D6xD5.
       let move = StandardMove (Square FileD Rank6) (Square FileD Rank5)
       let res = applyMove move ag

       case res of
         Checkmate (Winner White) -> return ()
         _ -> expectationFailure $ "Expected White Win (lost all pieces), got: " ++ show res

    it "winning by stalemate (no moves)" $ do
       -- Setup: White Pawn A2. Black Pawn A3. White to move.
       -- White blocked. No moves. White wins.
       -- We simulate previous turn where Black moves A4-A3.

       let b0 = initialBoard
             { pawns = Map.fromList [((FileA, PRank2), White), ((FileA, PRank4), Black)]
             , whitePieces = Map.empty
             , blackPieces = Map.empty
             , whiteKing = Nothing
             , blackKing = Nothing
             }

       let ag :: ActiveGame 'Antichess 'Black 'Safe
           ag = ActiveGame
                { internalBoard = toBaseBoard b0
                , castlingRights = CastlingRights False False False False
                , enPassantTarget = Nothing
                , halfMoveClock = 0
                , fullMoveNumber = 1
                , variantState = ()
                }

       let move = StandardMove (Square FileA Rank4) (Square FileA Rank3)
       let res = applyMove move ag

       case res of
         Checkmate (Winner White) -> return () -- White has no moves -> White wins.
         _ -> expectationFailure $ "Expected White Win (Stalemate), got: " ++ show res

  describe "Horde" $ do
    it "White Pawns on Rank 1 can double push" $ do
       let b = initialBoard
             { pawns = Map.fromList [((FileE, PRank1), White)]
             , whitePieces = Map.empty
             , blackPieces = Map.empty
             , whiteKing = Nothing
             , blackKing = Just (Square FileA Rank8)
             }
       let ag :: ActiveGame 'Horde 'White 'Safe
           ag = ActiveGame
                { internalBoard = toBaseBoard b
                , castlingRights = CastlingRights False False False False
                , enPassantTarget = Nothing
                , halfMoveClock = 0
                , fullMoveNumber = 1
                , variantState = ()
                }

       let moves = generateLegalMoves ag
       let doublePush = StandardMove (Square FileE Rank1) (Square FileE Rank3)
       let singlePush = StandardMove (Square FileE Rank1) (Square FileE Rank2)

       moves `shouldContain` [doublePush]
       moves `shouldContain` [singlePush]

    it "White wins by Checkmate" $ do
       -- Setup: Black King A8. Black Pawns A7, B7 (Trapped). White Rook H1.
       -- Move H1-H8 -> Backrank Mate.

       let b = initialBoard
             { pawns = Map.fromList [((FileA, PRank7), Black), ((FileB, PRank7), Black)]
             , whitePieces = Map.fromList [(Square FileH Rank1, MRook)]
             , blackPieces = Map.empty
             , whiteKing = Nothing
             , blackKing = Just (Square FileA Rank8)
             }

       let ag :: ActiveGame 'Horde 'White 'Safe
           ag = ActiveGame
                { internalBoard = toBaseBoard b
                , castlingRights = CastlingRights False False False False
                , enPassantTarget = Nothing
                , halfMoveClock = 0
                , fullMoveNumber = 1
                , variantState = ()
                }

       let move = StandardMove (Square FileH Rank1) (Square FileH Rank8)
       let res = applyMove move ag

       case res of
         Checkmate (Winner White) -> return ()
         _ -> expectationFailure $ "Expected White Win (Checkmate), got: " ++ show res

    it "Black wins by capturing all White pieces" $ do
       -- Setup: White Pawn A2. Black Rook A8.
       -- Black captures A2. White pieces = 0.
       let b = initialBoard
             { pawns = Map.fromList [((FileA, PRank2), White)]
             , whitePieces = Map.empty
             , blackPieces = Map.fromList [(Square FileA Rank8, MRook)]
             , whiteKing = Nothing
             , blackKing = Just (Square FileH Rank8)
             }

       let ag :: ActiveGame 'Horde 'Black 'Safe
           ag = ActiveGame
                { internalBoard = toBaseBoard b
                , castlingRights = CastlingRights False False False False
                , enPassantTarget = Nothing
                , halfMoveClock = 0
                , fullMoveNumber = 1
                , variantState = ()
                }

       let move = StandardMove (Square FileA Rank8) (Square FileA Rank2)
       let res = applyMove move ag

       case res of
         Checkmate (Winner Black) -> return ()
         _ -> expectationFailure "Expected Black Win (All White pieces captured)"
