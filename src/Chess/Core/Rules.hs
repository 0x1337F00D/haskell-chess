{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Chess.Core.Rules where

import Chess.Core.Board
import Chess.Core.Game
import Chess.Core.Move

import qualified Chess.Types as T
import qualified Chess.Board.Base as Base
import qualified Chess.Board.GameState as GS
import qualified Chess.Board.Validation as Val
import qualified Chess.Bitboard as BB
import Data.Bits (setBit, (.&.), (.|.), complement, testBit)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)

-- Type-level Opposite Color
type family Opposite (c :: Color) :: Color where
  Opposite 'White = 'Black
  Opposite 'Black = 'White

-- | Class to reify type-level Color to value-level Color
class KnownColor (c :: Color) where
  colorVal :: Color

instance KnownColor 'White where colorVal = White
instance KnownColor 'Black where colorVal = Black

-- | Convert Core Color to Engine Color
toColor :: Color -> T.Color
toColor White = T.White
toColor Black = T.Black

-- | Convert Core Square to Engine Square
toSquare :: Square -> T.Square
toSquare (Square f r) = T.Square (fromEnum r * 8 + fromEnum f)

-- | Convert Core PieceType to Engine PieceType
toPieceType :: PieceType -> T.PieceType
toPieceType King   = T.King
toPieceType Queen  = T.Queen
toPieceType Rook   = T.Rook
toPieceType Bishop = T.Bishop
toPieceType Knight = T.Knight
toPieceType Pawn   = T.Pawn

-- | Convert Core Board to Engine Board
toBaseBoard :: Board -> Base.Board
toBaseBoard b = Base.Board
  { Base.whitePawns   = wPawns
  , Base.blackPawns   = bPawns
  , Base.whiteKnights = wKnights
  , Base.blackKnights = bKnights
  , Base.whiteBishops = wBishops
  , Base.blackBishops = bBishops
  , Base.whiteRooks   = wRooks
  , Base.blackRooks   = bRooks
  , Base.whiteQueens  = wQueens
  , Base.blackQueens  = bQueens
  , Base.whiteKings   = wKings
  , Base.blackKings   = bKings
  , Base.occupiedWhite = wOcc
  , Base.occupiedBlack = bOcc
  , Base.occupiedTotal = wOcc .|. bOcc
  }
  where
    -- Helper to create bitboard from list of squares
    squaresToBB :: [Square] -> BB.Bitboard
    squaresToBB sqs = foldr (\s acc -> setBit acc (T.unSquare (toSquare s))) 0 sqs

    -- Extract squares for specific pieces
    -- Kings
    wKings = squaresToBB [whiteKing b]
    bKings = squaresToBB [blackKing b]

    -- Pawns
    wPawnSqs = [ Square f (toRank pr) | ((f, pr), c) <- Map.toList (pawns b), c == White ]
    bPawnSqs = [ Square f (toRank pr) | ((f, pr), c) <- Map.toList (pawns b), c == Black ]
    wPawns = squaresToBB wPawnSqs
    bPawns = squaresToBB bPawnSqs

    -- Major/Minor Pieces
    getSquaresWhite pt = [ sq | (sq, p) <- Map.toList (whitePieces b), pieceTypeMatches p pt ]
    getSquaresBlack pt = [ sq | (sq, p) <- Map.toList (blackPieces b), pieceTypeMatches p pt ]

    pieceTypeMatches :: MajorMinorPiece c -> PieceType -> Bool
    pieceTypeMatches MQueen Queen = True
    pieceTypeMatches MRook Rook = True
    pieceTypeMatches MBishop Bishop = True
    pieceTypeMatches MKnight Knight = True
    pieceTypeMatches _ _ = False

    wKnights = squaresToBB (getSquaresWhite Knight)
    bKnights = squaresToBB (getSquaresBlack Knight)
    wBishops = squaresToBB (getSquaresWhite Bishop)
    bBishops = squaresToBB (getSquaresBlack Bishop)
    wRooks   = squaresToBB (getSquaresWhite Rook)
    bRooks   = squaresToBB (getSquaresBlack Rook)
    wQueens  = squaresToBB (getSquaresWhite Queen)
    bQueens  = squaresToBB (getSquaresBlack Queen)

    wOcc = wPawns .|. wKnights .|. wBishops .|. wRooks .|. wQueens .|. wKings
    bOcc = bPawns .|. bKnights .|. bBishops .|. bRooks .|. bQueens .|. bKings

-- | Convert ActiveGame to Engine GameState
toGameState :: forall c s. KnownColor c => ActiveGame c s -> GS.GameState
toGameState ag = GS.GameState
  { GS.turn = toColor (colorVal @c)
  , GS.castlingRights = toCastlingRights (castlingRights ag)
  , GS.epSquare = case enPassantTarget ag of
                    Nothing -> Nothing
                    Just f -> Just (toSquare (Square f (epRank (colorVal @c))))
  , GS.halfmoveClock = halfMoveClock ag
  , GS.fullmoveNumber = fullMoveNumber ag
  }

epRank :: Color -> Rank
epRank White = Rank6 -- If White to move, EP target is Rank 6 (skipped over by Black)
epRank Black = Rank3 -- If Black to move, EP target is Rank 3 (skipped over by White)

toCastlingRights :: CastlingRights -> GS.CastlingRights
toCastlingRights cr =
  (if whiteKingSide cr then GS.allCastling .&. BB.BB_H1 else 0) .|.
  (if whiteQueenSide cr then GS.allCastling .&. BB.BB_A1 else 0) .|.
  (if blackKingSide cr then GS.allCastling .&. BB.BB_H8 else 0) .|.
  (if blackQueenSide cr then GS.allCastling .&. BB.BB_A8 else 0)

-- | Check if side `c` is in check.
isCheck :: Board -> Color -> Bool
isCheck b c = Val.isCheck (toBaseBoard b) (dummyGameState c)
  where
    dummyGameState col = GS.initialGameState { GS.turn = toColor col }

-- Generate Legal Moves
generateLegalMoves :: ActiveGame c s -> [Move c]
generateLegalMoves _ = [] -- Stub

-- Helpers for Apply Move
mkPiece :: Color -> PieceType -> SomePiece
mkPiece White King = SomePiece WKing
mkPiece White Queen = SomePiece WQueen
mkPiece White Rook = SomePiece WRook
mkPiece White Bishop = SomePiece WBishop
mkPiece White Knight = SomePiece WKnight
mkPiece White Pawn = SomePiece WPawn
mkPiece Black King = SomePiece BKing
mkPiece Black Queen = SomePiece BQueen
mkPiece Black Rook = SomePiece BRook
mkPiece Black Bishop = SomePiece BBishop
mkPiece Black Knight = SomePiece BKnight
mkPiece Black Pawn = SomePiece BPawn

getCastlingRookMove :: Square -> Square -> (Square, Square)
getCastlingRookMove (Square FileE Rank1) (Square FileG Rank1) = (Square FileH Rank1, Square FileF Rank1) -- White King Side
getCastlingRookMove (Square FileE Rank1) (Square FileC Rank1) = (Square FileA Rank1, Square FileD Rank1) -- White Queen Side
getCastlingRookMove (Square FileE Rank8) (Square FileG Rank8) = (Square FileH Rank8, Square FileF Rank8) -- Black King Side
getCastlingRookMove (Square FileE Rank8) (Square FileC Rank8) = (Square FileA Rank8, Square FileD Rank8) -- Black Queen Side
getCastlingRookMove f t = (f, t) -- Fallback

getEpCapturedSquare :: Square -> Square -> Square
getEpCapturedSquare (Square _ r1) (Square f2 _) = Square f2 r1

isDoublePush :: Square -> Square -> Bool
isDoublePush (Square _ Rank2) (Square _ Rank4) = True
isDoublePush (Square _ Rank7) (Square _ Rank5) = True
isDoublePush _ _ = False

getFile :: Square -> File
getFile (Square f _) = f

updateCastlingRights :: CastlingRights -> Square -> Square -> CastlingRights
updateCastlingRights cr from to =
  let
      -- Check if King or Rook moved (from)
      cr1 = case from of
              Square FileE Rank1 -> cr { whiteKingSide = False, whiteQueenSide = False }
              Square FileE Rank8 -> cr { blackKingSide = False, blackQueenSide = False }
              Square FileH Rank1 -> cr { whiteKingSide = False }
              Square FileA Rank1 -> cr { whiteQueenSide = False }
              Square FileH Rank8 -> cr { blackKingSide = False }
              Square FileA Rank8 -> cr { blackQueenSide = False }
              _ -> cr

      -- Check if Rook captured (to)
      cr2 = case to of
              Square FileH Rank1 -> cr1 { whiteKingSide = False }
              Square FileA Rank1 -> cr1 { whiteQueenSide = False }
              Square FileH Rank8 -> cr1 { blackKingSide = False }
              Square FileA Rank8 -> cr1 { blackQueenSide = False }
              _ -> cr1
  in cr2

-- Apply Move
-- We require KnownColor c to handle GameState conversion and Turn switching logic.
applyMove :: forall c s. (KnownColor c, KnownColor (Opposite c)) => Move c -> ActiveGame c s -> MoveResult (Opposite c)
applyMove m ag =
  let
      -- 1. Update Board
      b = gameBoard ag

      b' = case m of
             StandardMove f t -> movePiece f t b
             PromotionMove f t pt ->
               let b1 = removePieceAt f b -- Remove pawn
                   promoted = mkPiece (colorVal @c) pt
               in putPieceAt t promoted b1
             CastlingMove f t ->
               let b1 = movePiece f t b -- Move King
                   (rf, rt) = getCastlingRookMove f t
               in movePiece rf rt b1 -- Move Rook
             EnPassantMove f t ->
               let b1 = movePiece f t b -- Move Pawn
                   capSq = getEpCapturedSquare f t
               in removePieceAt capSq b1

      (from, to) = case m of
                     StandardMove f t -> (f, t)
                     PromotionMove f t _ -> (f, t)
                     CastlingMove f t -> (f, t)
                     EnPassantMove f t -> (f, t)

      -- 2. Update Game State

      -- Update Castling Rights
      newCR = updateCastlingRights (castlingRights ag) from to

      -- Update EP Target
      -- Check if pawn moved
      movedPiece = getPieceAt to b'
      isPawn = case movedPiece of
                 Just (SomePiece WPawn) -> True
                 Just (SomePiece BPawn) -> True
                 _ -> False

      newEP = case m of
                StandardMove f t -> if isPawn && isDoublePush f t then Just (getFile f) else Nothing
                _ -> Nothing

      -- Update Clocks
      newHMC = halfMoveClock ag + 1
      newFMN = fullMoveNumber ag + (if colorVal @c == Black then 1 else 0)

      -- 3. Validation
      baseBoard = toBaseBoard b'

      -- We construct the GameState for the NEXT player to check if THEY are in check/mate.
      nextTurnGS = GS.GameState
        { GS.turn = toColor (colorVal @(Opposite c))
        , GS.castlingRights = toCastlingRights newCR
        , GS.epSquare = case newEP of
                          Nothing -> Nothing
                          Just f -> Just (toSquare (Square f (epRank (colorVal @(Opposite c)))))
                          -- Note: EP Rank for next turn is based on WHO moved.
                          -- If White moved 2 squares, next turn is Black.
                          -- Black needs to know EP target is Rank 3.
                          -- 'epRank (colorVal @(Opposite c))' (Black) returns Rank 3. Correct.
        , GS.halfmoveClock = newHMC
        , GS.fullmoveNumber = newFMN
        }

      isChecked = Val.isCheck baseBoard nextTurnGS
      hasMoves = Val.hasLegalMoves baseBoard nextTurnGS

  in case (isChecked, hasMoves) of
       (True, False) -> Checkmate (Winner (colorVal @c))
       (False, False) -> Stalemate
       (True, True) -> Continue (ActiveGame
                                  { gameBoard = b'
                                  , castlingRights = newCR
                                  , enPassantTarget = newEP
                                  , halfMoveClock = newHMC
                                  , fullMoveNumber = newFMN
                                  } :: ActiveGame (Opposite c) 'Checked)
       (False, True) -> Continue (ActiveGame
                                  { gameBoard = b'
                                  , castlingRights = newCR
                                  , enPassantTarget = newEP
                                  , halfMoveClock = newHMC
                                  , fullMoveNumber = newFMN
                                  } :: ActiveGame (Opposite c) 'Safe)
