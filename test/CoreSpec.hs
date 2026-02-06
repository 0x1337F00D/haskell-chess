{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module CoreSpec where

import Test.Hspec
import Chess.Core.Board
import Chess.Core.Game
import Chess.Core.Move
import Chess.Core.Rules
import Chess.Core.Board.Internal (movePiece)
import Chess.Core.Game.Internal (ActiveGame(..), Game(..), CastlingRights(..), Pockets(..), CrazyhouseState(..), castlingWhiteKingSide, castlingWhiteQueenSide, castlingBlackKingSide, castlingBlackQueenSide, SCheckStatus(..))
import Chess.Core.Move.Internal (Move(..), GameTransition(..))
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import qualified Chess.Core.Board.Internal as CBI
import qualified Chess.Board.Base as Base
import qualified Chess.Board.GameState as GS
import qualified Chess.Bitboard as BB
import qualified Chess.Types as T
import Data.Bits ((.|.), (.&.))
import Data.Word (Word8)
import Data.Function ((&))

unsafeViewBoard :: Base.Board -> CBI.Board
unsafeViewBoard bb = fromJust (CBI.fromBaseBoard bb)

mkCastlingRights :: Bool -> Bool -> Bool -> Bool -> GS.CastlingRights
mkCastlingRights wk wq bk bq =
    (if wk then BB.BB_H1 else 0) .|.
    (if wq then BB.BB_A1 else 0) .|.
    (if bk then BB.BB_H8 else 0) .|.
    (if bq then BB.BB_A8 else 0)

spec :: Spec
spec = describe "Core Architecture" $ do
  describe "Board" $ do
    it "initialBoard has Kings on correct squares" $ do
      whiteKing initialBoard `shouldBe` Square FileE Rank1
      blackKing initialBoard `shouldBe` Square FileE Rank8

    it "initialBoard has correct number of Pawns" $ do
      Map.size (pawns initialBoard) `shouldBe` 16

    it "movePiece updates piece location" $ do
      let b = initialBoard
      let b' = movePiece (Square FileE Rank2) (Square FileE Rank4) b
      -- Check E2 is empty
      getPieceAt (Square FileE Rank2) b' `shouldBe` Nothing
      -- Check E4 has White Pawn
      case getPieceAt (Square FileE Rank4) b' of
        Just (SomePiece WPawn) -> return ()
        _ -> expectationFailure "Expected White Pawn at E4"

  describe "FEN" $ do
    it "fromFEN parses initial position correctly" $ do
      let fenStr = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
      case fromFEN fenStr of
        Just b -> b `shouldBe` initialBoard
        Nothing -> expectationFailure "Failed to parse initial FEN"

    it "fromFEN parses custom position correctly" $ do
      -- E4, D4, White King E1, Black King E8
      let fenStr = "4k3/8/8/8/3P4/4P3/8/4K3 w - - 0 1"
      case fromFEN fenStr of
        Just b -> do
          whiteKing b `shouldBe` Square FileE Rank1
          blackKing b `shouldBe` Square FileE Rank8
          -- Check Pawn at E3
          case getPieceAt (Square FileE Rank3) b of
             Just (SomePiece WPawn) -> return ()
             _ -> expectationFailure "Expected White Pawn at E3"
          -- Check Pawn at D4
          case getPieceAt (Square FileD Rank4) b of
             Just (SomePiece WPawn) -> return ()
             _ -> expectationFailure "Expected White Pawn at D4"
          -- Check Empty
          getPieceAt (Square FileA Rank1) b `shouldBe` Nothing
        Nothing -> expectationFailure "Failed to parse custom FEN"

    it "fromFEN returns Nothing for invalid FEN (missing King)" $ do
      let fenStr = "8/8/8/8/8/8/8/8 w - - 0 1"
      fromFEN fenStr `shouldBe` Nothing

  describe "Game Factory" $ do
    it "initialGame creates a valid game" $ do
      case initialGame of
        InProgressGame ag -> unsafeViewBoard (internalBoard ag) `shouldBe` initialBoard

    it "gameFromFEN parses initial FEN" $ do
      let fenStr = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
      case gameFromFEN fenStr of
        Just (InProgressGame ag) -> do
          unsafeViewBoard (internalBoard ag) `shouldBe` initialBoard
        Nothing -> expectationFailure "Failed to parse initial game FEN"

    it "gameFromFEN detects check" $ do
       -- White King E1, Black Rook E5. White to move.
       let fenStrCheck = "4k3/8/8/4r3/8/8/8/4K3 w - - 0 1"
       case gameFromFEN fenStrCheck of
         Just (InProgressGame ag) -> do
           let moves = generateLegalMoves ag
           length moves `shouldSatisfy` (> 0)
         Nothing -> expectationFailure "Failed to parse checked FEN"

  describe "Rules" $ do
    it "isCheck detects check" $ do
      -- Setup a board with check
      -- White King E1, Black Rook E5.
      let b = initialBoard
            { pawns = Map.empty
            , whitePieces = Map.empty
            , blackPieces = Map.singleton (Square FileE Rank5) MRook
            }
      isCheck b White `shouldBe` True
      isCheck b Black `shouldBe` False

    it "applyMove performs a move and switches turn" $ do
      let ag :: ActiveGame 'Standard 'White 'Safe
          ag = ActiveGame
               { internalBoard = toBaseBoard initialBoard
               , gameState = GS.initialGameState
               , variantState = ()
               , checkStatus = SSafe
               }
      -- Move E2 to E4
      let move = QuietMove (Square FileE Rank2) (Square FileE Rank4) Pawn

      let res = applyMove move ag
      case res of
        Transition nextGame -> do
          -- We can verify the board in nextGame
          let b' = unsafeViewBoard (internalBoard nextGame)
          case getPieceAt (Square FileE Rank4) b' of
             Just (SomePiece WPawn) -> return ()
             _ -> expectationFailure "Expected White Pawn at E4 in next game state"

    it "generateLegalMoves generates 20 moves for initial board" $ do
      let ag :: ActiveGame 'Standard 'White 'Safe
          ag = ActiveGame
               { internalBoard = toBaseBoard initialBoard
               , gameState = GS.initialGameState
               , variantState = ()
               , checkStatus = SSafe
               }
      let moves = generateLegalMoves ag
      length moves `shouldBe` 20
      -- Check E2-E4 is present
      let e2e4 = QuietMove (Square FileE Rank2) (Square FileE Rank4) Pawn
      moves `shouldContain` [e2e4]

    it "generateLegalMoves generates castling moves" $ do
      -- Setup board for White King Side Castling
      -- White King E1, Rook H1. Empty F1, G1.
      -- Black King E8.
      let b = initialBoard
            { pawns = Map.empty
            , whitePieces = Map.fromList [(Square FileH Rank1, MRook)]
            , blackPieces = Map.empty
            }
      -- Ensure E1 is King (it is in initialBoard.whiteKing)
      -- Ensure H1 is Rook.

      let ag :: ActiveGame 'Standard 'White 'Safe
          ag = ActiveGame
               { internalBoard = toBaseBoard b
               , gameState = GS.setCastlingRights (mkCastlingRights True False False False) GS.initialGameState
               , variantState = ()
               , checkStatus = SSafe
               }

      let moves = generateLegalMoves ag
      let castling = CastlingMove (Square FileE Rank1) (Square FileG Rank1)
      moves `shouldContain` [castling]

    it "generateLegalMoves generates en passant moves" $ do
      -- Setup En Passant: White Pawn E5, Black Pawn F5 (just moved F7-F5).
      -- En Passant Target: F6.
      -- White to move.
      let b = initialBoard
            { pawns = Map.fromList [((FileE, PRank5), White), ((FileF, PRank5), Black)]
            , whitePieces = Map.empty
            , blackPieces = Map.empty
            }

      let ag :: ActiveGame 'Standard 'White 'Safe
          ag = ActiveGame
               { internalBoard = toBaseBoard b
               , gameState = GS.initialGameState
                   & GS.setCastlingRights (mkCastlingRights False False False False)
                   & GS.setEpSquare (toSquare (Square FileF Rank6))
               , variantState = ()
               , checkStatus = SSafe
               }

      let moves = generateLegalMoves ag
      let epMove = EnPassantMove (Square FileE Rank5) (Square FileF Rank6)
      moves `shouldContain` [epMove]

    it "Atomic: capture triggers explosion" $ do
      -- Setup
      -- White Pawn E4, C4. Black Pawn D5. Black Knight C5.
      -- Move: E4 captures D5.
      -- Expected: D5 exploded (both pawns gone). C5 exploded (Knight gone). C4 survives (Pawn immune).
      let b = initialBoard
            { pawns = Map.fromList [((FileE, PRank4), White), ((FileC, PRank4), White), ((FileD, PRank5), Black)]
            , whitePieces = Map.empty
            , blackPieces = Map.fromList [(Square FileC Rank5, MKnight)]
            }

      let ag :: ActiveGame 'Atomic 'White 'Safe
          ag = ActiveGame
               { internalBoard = toBaseBoard b
               , gameState = GS.setCastlingRights (mkCastlingRights False False False False) GS.initialGameState
               , variantState = ()
               , checkStatus = SSafe
               }

      let move = CaptureMove (Square FileE Rank4) (Square FileD Rank5) Pawn Pawn

      let res = applyMove move ag
      case res of
        Transition nextGame -> do
           let b' = unsafeViewBoard (internalBoard nextGame)
           -- D5 empty (Exploded center)
           getPieceAt (Square FileD Rank5) b' `shouldBe` Nothing
           -- C5 empty (Exploded neighbor Knight)
           getPieceAt (Square FileC Rank5) b' `shouldBe` Nothing
           -- C4 occupied (Neighbor Pawn survives)
           case getPieceAt (Square FileC Rank4) b' of
             Just (SomePiece WPawn) -> return ()
             _ -> expectationFailure "Expected White Pawn at C4 to survive"

    it "KingOfTheHill: King to center wins" $ do
      -- Setup White King on E3. Move to E4 (Center).
      let b = initialBoard { whiteKing = Square FileE Rank3 }
      let ag :: ActiveGame 'KingOfTheHill 'White 'Safe
          ag = ActiveGame
               { internalBoard = toBaseBoard b
               , gameState = GS.setCastlingRights (mkCastlingRights False False False False) GS.initialGameState
               , variantState = ()
               , checkStatus = SSafe
               }
      let move = QuietMove (Square FileE Rank3) (Square FileE Rank4) King
      let res = executeMove move ag
      case res of
        Checkmate (Winner White) -> return ()
        _ -> expectationFailure "Expected White Win (King to Center)"

    it "RacingKings: Move giving check is illegal" $ do
        -- Setup: White Rook A1. Black King A8.
        -- Move A1-A2 attacks A file (gives check to A8). Should be illegal.
        let b = initialBoard
              { pawns = Map.empty
              , whitePieces = Map.fromList [(Square FileA Rank1, MRook)]
              , blackPieces = Map.empty
              , blackKing = Square FileA Rank8
              }
        let ag :: ActiveGame 'RacingKings 'White 'Safe
            ag = ActiveGame
               { internalBoard = toBaseBoard b
               , gameState = GS.setCastlingRights (mkCastlingRights False False False False) GS.initialGameState
               , variantState = ()
               , checkStatus = SSafe
               }

        let moves = generateLegalMoves ag
        let unsafeMove = QuietMove (Square FileA Rank1) (Square FileA Rank2) Rook
        moves `shouldNotContain` [unsafeMove]

        -- Safe move: Rook to B1
        let safeMove = QuietMove (Square FileA Rank1) (Square FileB Rank1) Rook
        moves `shouldContain` [safeMove]

    it "RacingKings: Reaching Rank 8" $ do
       -- Setup White King on E7. Move to E8.
       -- Ensure Black King is safe and not at E8 (to avoid capture)
       let b = initialBoard { whiteKing = Square FileE Rank7, blackKing = Square FileA Rank5 }
       let ag :: ActiveGame 'RacingKings 'White 'Safe
           ag = ActiveGame
               { internalBoard = toBaseBoard b
               , gameState = GS.setCastlingRights (mkCastlingRights False False False False) GS.initialGameState
               , variantState = ()
               , checkStatus = SSafe
               }
       let move = QuietMove (Square FileE Rank7) (Square FileE Rank8) King
       let res = executeMove move ag
       case res of
         Continue _ -> return () -- Game continues for Black's turn
         _ -> expectationFailure "Expected Continue (wait for Black)"

    it "ThreeCheck: Win by 3 checks" $ do
       -- Setup: White Rook on A1. Black King on A8.
       let b = initialBoard
             { pawns = Map.empty
             , whitePieces = Map.fromList [(Square FileA Rank1, MRook)]
             , blackPieces = Map.empty
             , blackKing = Square FileA Rank8
             }

       -- Case 1: 0 checks -> 1 check
       let ag :: ActiveGame 'ThreeCheck 'White 'Safe
           ag = ActiveGame
                { internalBoard = toBaseBoard b
                , gameState = GS.setCastlingRights (mkCastlingRights False False False False) GS.initialGameState
                , variantState = (0, 0)
                , checkStatus = SSafe
                }

       let m1 = QuietMove (Square FileA Rank1) (Square FileA Rank2) Rook -- Checks A8
       let res1 = applyMove m1 ag
       case res1 of
         Transition ag2 -> do
            let (wChecks, _) = variantState ag2
            wChecks `shouldBe` 1

       -- Case 2: 2 checks -> 3rd check (Win)
       let agTwo :: ActiveGame 'ThreeCheck 'White 'Safe
           agTwo = ActiveGame
                { internalBoard = toBaseBoard b
                , gameState = GS.setCastlingRights (mkCastlingRights False False False False) GS.initialGameState
                , variantState = (2, 0)
                , checkStatus = SSafe
                }

       let res2 = executeMove m1 agTwo -- A1-A2 again
       case res2 of
          Checkmate (Winner White) -> return ()
          _ -> expectationFailure "Expected White Win by 3rd Check"
