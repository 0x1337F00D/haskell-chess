{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Chess.Core.Rules.Common where

import Chess.Core.Board.Internal
import Chess.Core.Game.Internal
import Chess.Core.Move.Internal
import Chess.Core.Rules.Class (Opposite)

import qualified Chess.Types as T
import qualified Chess.Board.Base as Base
import qualified Chess.Board.GameState as GS
import qualified Chess.Board.MoveGen as MG
import qualified Chess.Board.Validation as Val
import qualified Chess.Bitboard as BB
import qualified Chess.Board.Fen as Fen
import Data.Bits (setBit, clearBit, (.&.), (.|.), testBit, countTrailingZeros, complement, xor)
import Data.Word (Word8)
import qualified Data.Map as Map

-- | Create the initial game state for Standard chess.
initialGame :: Game 'Standard 'Active
initialGame =
  let b = initialBoard
      cr = CastlingRights (castlingWhiteKingSide .|. castlingWhiteQueenSide .|. castlingBlackKingSide .|. castlingBlackQueenSide)
      ag = ActiveGame
           { internalBoard = toBaseBoard b
           , castlingRights = cr
           , enPassantTarget = Nothing
           , halfMoveClock = 0
           , fullMoveNumber = 1
           , variantState = ()
           } :: ActiveGame 'Standard 'White 'Safe
  in InProgressGame ag

-- | Create a game from FEN string (Standard variant).
gameFromFEN :: String -> Maybe (Game 'Standard 'Active)
gameFromFEN s = do
  (baseBoard, gs) <- Fen.parseFen s

  let c = case GS.turn gs of
            T.White -> White
            T.Black -> Black

      -- Map bitboard bits to CastlingRights bits
      crVal = (if testBit (GS.castlingRights gs) (countTrailingZeros BB.BB_H1) then castlingWhiteKingSide else 0) .|.
              (if testBit (GS.castlingRights gs) (countTrailingZeros BB.BB_A1) then castlingWhiteQueenSide else 0) .|.
              (if testBit (GS.castlingRights gs) (countTrailingZeros BB.BB_H8) then castlingBlackKingSide else 0) .|.
              (if testBit (GS.castlingRights gs) (countTrailingZeros BB.BB_A8) then castlingBlackQueenSide else 0)

      cr = CastlingRights crVal

      ep = case GS.epSquare gs of
             Nothing -> Nothing
             Just sq -> Just (getFile (fromBaseSquare sq))

      hmc = GS.halfmoveClock gs
      fmn = GS.fullmoveNumber gs

      checked = Val.isCheck baseBoard gs
      hasMoves = Val.hasLegalMoves baseBoard gs

  if hasMoves
    then case c of
      White -> if checked
               then return $ InProgressGame (ActiveGame baseBoard cr ep hmc fmn () :: ActiveGame 'Standard 'White 'Checked)
               else return $ InProgressGame (ActiveGame baseBoard cr ep hmc fmn () :: ActiveGame 'Standard 'White 'Safe)
      Black -> if checked
               then return $ InProgressGame (ActiveGame baseBoard cr ep hmc fmn () :: ActiveGame 'Standard 'Black 'Checked)
               else return $ InProgressGame (ActiveGame baseBoard cr ep hmc fmn () :: ActiveGame 'Standard 'Black 'Safe)
    else Nothing

-- | Convert Core Color to Engine Color
toColor :: Color -> T.Color
toColor White = T.White
toColor Black = T.Black

-- | Convert Core Square to Engine Square
toSquare :: Square -> T.Square
toSquare (Square f r) = T.Square (fromEnum r * 8 + fromEnum f)

-- | Convert Engine Square to Core Square
fromSquare :: T.Square -> Square
fromSquare (T.Square i) = Square (toEnum (i `mod` 8)) (toEnum (i `div` 8))

-- | Convert Core PieceType to Engine PieceType
toPieceType :: PieceType -> T.PieceType
toPieceType King   = T.King
toPieceType Queen  = T.Queen
toPieceType Rook   = T.Rook
toPieceType Bishop = T.Bishop
toPieceType Knight = T.Knight
toPieceType Pawn   = T.Pawn

-- | Convert Engine PieceType to Core PieceType
fromPieceType :: T.PieceType -> PieceType
fromPieceType T.King   = King
fromPieceType T.Queen  = Queen
fromPieceType T.Rook   = Rook
fromPieceType T.Bishop = Bishop
fromPieceType T.Knight = Knight
fromPieceType T.Pawn   = Pawn

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
toGameState :: forall v c s. KnownColor c => ActiveGame v c s -> GS.GameState
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
epRank White = Rank6
epRank Black = Rank3

toCastlingRights :: CastlingRights -> GS.CastlingRights
toCastlingRights (CastlingRights cr) =
  (if testBit cr 0 then BB.BB_H1 else 0) .|. -- White King Side (Bit 0) -> H1
  (if testBit cr 1 then BB.BB_A1 else 0) .|. -- White Queen Side (Bit 1) -> A1
  (if testBit cr 2 then BB.BB_H8 else 0) .|. -- Black King Side (Bit 2) -> H8
  (if testBit cr 3 then BB.BB_A8 else 0)     -- Black Queen Side (Bit 3) -> A8

-- | Check if side `c` is in check.
isCheck :: Board -> Color -> Bool
isCheck b c = Val.isCheck (toBaseBoard b) (dummyGameState c)
  where
    dummyGameState col = GS.initialGameState { GS.turn = toColor col }

toCoreMove :: Base.Board -> T.Move -> Move c
toCoreMove b (T.Move f t promo) =
  let fromSq = fromSquare f
      toSq = fromSquare t
      p = Base.pieceAt b f
  in case (p, promo) of
       (Just _, Just pt) ->
          PromotionMove fromSq toSq (fromPieceType pt)
       (Just piece, Nothing) ->
          if isCastlingMove piece fromSq toSq
          then CastlingMove fromSq toSq
          else if isEnPassantMove piece fromSq toSq b
               then EnPassantMove fromSq toSq
               else StandardMove fromSq toSq
       _ -> error "Invalid move generated"
toCoreMove _ (T.DropMove pt t) = DropMove (fromPieceType pt) (fromSquare t)
toCoreMove _ T.NullMove = error "Null move generated"

isCastlingMove :: T.Piece -> Square -> Square -> Bool
isCastlingMove p from to =
  T.pieceType p == T.King && abs (fromEnum (getFile from) - fromEnum (getFile to)) == 2

isEnPassantMove :: T.Piece -> Square -> Square -> Base.Board -> Bool
isEnPassantMove p from to b =
  T.pieceType p == T.Pawn &&
  getFile from /= getFile to &&
  case Base.pieceAt b (toSquare to) of
    Nothing -> True
    Just _ -> False

-- Helpers for Apply Move
getCastlingRookMove :: Square -> Square -> (Square, Square)
getCastlingRookMove (Square FileE Rank1) (Square FileG Rank1) = (Square FileH Rank1, Square FileF Rank1)
getCastlingRookMove (Square FileE Rank1) (Square FileC Rank1) = (Square FileA Rank1, Square FileD Rank1)
getCastlingRookMove (Square FileE Rank8) (Square FileG Rank8) = (Square FileH Rank8, Square FileF Rank8)
getCastlingRookMove (Square FileE Rank8) (Square FileC Rank8) = (Square FileA Rank8, Square FileD Rank8)
getCastlingRookMove f t = (f, t)

getEpCapturedSquare :: Square -> Square -> Square
getEpCapturedSquare (Square _ r1) (Square f2 _) = Square f2 r1

isDoublePush :: Square -> Square -> Bool
isDoublePush (Square _ Rank2) (Square _ Rank4) = True
isDoublePush (Square _ Rank7) (Square _ Rank5) = True
isDoublePush _ _ = False

getFile :: Square -> File
getFile (Square f _) = f

updateCastlingRights :: CastlingRights -> Square -> Square -> CastlingRights
updateCastlingRights (CastlingRights cr) from to =
  let
      -- Bitmasks for clearing rights
      -- WhiteKingSide = 1, WhiteQueenSide = 2, BlackKingSide = 4, BlackQueenSide = 8

      -- Clear White Rights (both) if White King Moves (E1)
      mask1 = case from of
                Square FileE Rank1 -> complement (castlingWhiteKingSide .|. castlingWhiteQueenSide)
                Square FileE Rank8 -> complement (castlingBlackKingSide .|. castlingBlackQueenSide)
                Square FileH Rank1 -> complement castlingWhiteKingSide
                Square FileA Rank1 -> complement castlingWhiteQueenSide
                Square FileH Rank8 -> complement castlingBlackKingSide
                Square FileA Rank8 -> complement castlingBlackQueenSide
                _ -> 0xFF -- No change

      cr1 = cr .&. mask1

      -- Check if Rook captured (to)
      mask2 = case to of
                Square FileH Rank1 -> complement castlingWhiteKingSide
                Square FileA Rank1 -> complement castlingWhiteQueenSide
                Square FileH Rank8 -> complement castlingBlackKingSide
                Square FileA Rank8 -> complement castlingBlackQueenSide
                _ -> 0xFF

      cr2 = cr1 .&. mask2
  in CastlingRights cr2

-- Apply Move Helper (Base Board update)
applyMoveBase :: forall c. KnownColor c => Move c -> Base.Board -> Base.Board
applyMoveBase m b =
    case m of
       StandardMove f t ->
          let p = Base.pieceAt b (toSquare f)
          in case p of
             Nothing -> b
             Just piece ->
                 let b1 = Base.removePieceAt b (toSquare f)
                 in Base.putPiece b1 (toSquare t) piece

       PromotionMove f t pt ->
          let b1 = Base.removePieceAt b (toSquare f)
              promoted = T.Piece (toColor (colorVal @c)) (toPieceType pt)
          in Base.putPiece b1 (toSquare t) promoted

       CastlingMove f t ->
          let p = Base.pieceAt b (toSquare f)
              b1 = case p of
                     Just piece -> Base.putPiece (Base.removePieceAt b (toSquare f)) (toSquare t) piece
                     Nothing -> b
              (rf, rt) = getCastlingRookMove f t
              rook = Base.pieceAt b (toSquare rf)
              b2 = case rook of
                     Just r -> Base.putPiece (Base.removePieceAt b1 (toSquare rf)) (toSquare rt) r
                     Nothing -> b1
          in b2

       EnPassantMove f t ->
          let p = Base.pieceAt b (toSquare f)
              b1 = case p of
                     Just piece -> Base.putPiece (Base.removePieceAt b (toSquare f)) (toSquare t) piece
                     Nothing -> b
              capSq = getEpCapturedSquare f t
          in Base.removePieceAt b1 (toSquare capSq)
       DropMove p t ->
          let promoted = T.Piece (toColor (colorVal @c)) (toPieceType p)
          in Base.putPiece b (toSquare t) promoted
