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
import Chess.Core.Game.Internal (ActiveGame(..))
import Chess.Core.Move.Internal (Move(..))
import qualified Data.Map as Map

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
               { gameBoard = initialBoard
               , castlingRights = CastlingRights True True True True
               , enPassantTarget = Nothing
               , halfMoveClock = 0
               , fullMoveNumber = 1
               , variantData = ()
               }
      -- Move E2 to E4
      let move = StandardMove (Square FileE Rank2) (Square FileE Rank4)

      let res = applyMove move ag
      case res of
        Continue nextGame -> do
          -- We can verify the board in nextGame
          let b' = gameBoard nextGame
          case getPieceAt (Square FileE Rank4) b' of
             Just (SomePiece WPawn) -> return ()
             _ -> expectationFailure "Expected White Pawn at E4 in next game state"
        _ -> expectationFailure "Expected Continue"

    it "generateLegalMoves generates 20 moves for initial board" $ do
      let ag :: ActiveGame 'Standard 'White 'Safe
          ag = ActiveGame
               { gameBoard = initialBoard
               , castlingRights = CastlingRights True True True True
               , enPassantTarget = Nothing
               , halfMoveClock = 0
               , fullMoveNumber = 1
               , variantData = ()
               }
      let moves = generateLegalMoves ag
      length moves `shouldBe` 20
      -- Check E2-E4 is present
      let e2e4 = StandardMove (Square FileE Rank2) (Square FileE Rank4)
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
               { gameBoard = b
               , castlingRights = CastlingRights True False False False
               , enPassantTarget = Nothing
               , halfMoveClock = 0
               , fullMoveNumber = 1
               , variantData = ()
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
               { gameBoard = b
               , castlingRights = CastlingRights False False False False
               , enPassantTarget = Just FileF
               , halfMoveClock = 0
               , fullMoveNumber = 1
               , variantData = ()
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
               { gameBoard = b
               , castlingRights = CastlingRights False False False False
               , enPassantTarget = Nothing
               , halfMoveClock = 0
               , fullMoveNumber = 1
               , variantData = ()
               }

      let move = StandardMove (Square FileE Rank4) (Square FileD Rank5)

      let res = applyMove move ag
      case res of
        Continue nextGame -> do
           let b' = gameBoard nextGame
           -- D5 empty (Exploded center)
           getPieceAt (Square FileD Rank5) b' `shouldBe` Nothing
           -- C5 empty (Exploded neighbor Knight)
           getPieceAt (Square FileC Rank5) b' `shouldBe` Nothing
           -- C4 occupied (Neighbor Pawn survives)
           case getPieceAt (Square FileC Rank4) b' of
             Just (SomePiece WPawn) -> return ()
             _ -> expectationFailure "Expected White Pawn at C4 to survive"
        _ -> expectationFailure "Expected Continue"

    it "KingOfTheHill: King to center wins" $ do
      -- Setup White King on E3. Move to E4 (Center).
      let b = initialBoard { whiteKing = Square FileE Rank3 }
      let ag :: ActiveGame 'KingOfTheHill 'White 'Safe
          ag = ActiveGame
               { gameBoard = b
               , castlingRights = CastlingRights False False False False
               , enPassantTarget = Nothing
               , halfMoveClock = 0
               , fullMoveNumber = 1
               , variantData = ()
               }
      let move = StandardMove (Square FileE Rank3) (Square FileE Rank4)
      let res = applyMove move ag
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
               { gameBoard = b
               , castlingRights = CastlingRights False False False False
               , enPassantTarget = Nothing
               , halfMoveClock = 0
               , fullMoveNumber = 1
               , variantData = ()
               }

        let moves = generateLegalMoves ag
        let unsafeMove = StandardMove (Square FileA Rank1) (Square FileA Rank2)
        moves `shouldNotContain` [unsafeMove]

        -- Safe move: Rook to B1
        let safeMove = StandardMove (Square FileA Rank1) (Square FileB Rank1)
        moves `shouldContain` [safeMove]

    it "RacingKings: Reaching Rank 8" $ do
       -- Setup White King on E7. Move to E8.
       let b = initialBoard { whiteKing = Square FileE Rank7 }
       let ag :: ActiveGame 'RacingKings 'White 'Safe
           ag = ActiveGame
               { gameBoard = b
               , castlingRights = CastlingRights False False False False
               , enPassantTarget = Nothing
               , halfMoveClock = 0
               , fullMoveNumber = 1
               , variantData = ()
               }
       let move = StandardMove (Square FileE Rank7) (Square FileE Rank8)
       let res = applyMove move ag
       case res of
         Continue _ -> return () -- Game continues for Black's turn
         _ -> expectationFailure "Expected Continue (wait for Black)"

    it "ThreeCheck: 3rd check wins" $ do
      -- Setup: White Rook on E1, King on E8.
      -- 2 checks already.
      -- Move Rook to E2 (check).
      let b = initialBoard
            { pawns = Map.empty
            , whitePieces = Map.singleton (Square FileE Rank1) MRook
            , blackPieces = Map.empty
            , blackKing = Square FileE Rank8
            }

      let ag :: ActiveGame 'ThreeCheck 'White 'Safe
          ag = ActiveGame
               { gameBoard = b
               , castlingRights = CastlingRights False False False False
               , enPassantTarget = Nothing
               , halfMoveClock = 0
               , fullMoveNumber = 1
               , variantData = (2, 0)
               }

      -- Move E1-E2
      let move = StandardMove (Square FileE Rank1) (Square FileE Rank2)
      let res = applyMove move ag
      case res of
        Checkmate (Winner White) -> return ()
        _ -> expectationFailure "Expected White Win (3rd Check)"
