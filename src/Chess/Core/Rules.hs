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

import Chess.Core.Board.Internal
import Chess.Core.Game.Internal
import Chess.Core.Move.Internal

import qualified Chess.Types as T
import qualified Chess.Board.Base as Base
import qualified Chess.Board.GameState as GS
import qualified Chess.Board.MoveGen as MG
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

-- | Class for Chess Variants
class ChessVariant (v :: Variant) where
  generateMoves :: KnownColor c => ActiveGame v c s -> [Move c]
  executeMove :: (KnownColor c, KnownColor (Opposite c)) => Move c -> ActiveGame v c s -> MoveResult v (Opposite c)

-- Generate Legal Moves
generateLegalMoves :: forall v c s. (KnownColor c, ChessVariant v) => ActiveGame v c s -> [Move c]
generateLegalMoves = generateMoves

toCoreMove :: Board -> T.Move -> Move c
toCoreMove b (T.Move f t promo) =
  let fromSq = fromSquare f
      toSq = fromSquare t
      p = getPieceAt fromSq b
  in case (p, promo) of
       (Just (SomePiece piece), Just pt) ->
          PromotionMove fromSq toSq (fromPieceType pt)
       (Just (SomePiece piece), Nothing) ->
          if isCastlingMove piece fromSq toSq
          then CastlingMove fromSq toSq
          else if isEnPassantMove piece fromSq toSq b
               then EnPassantMove fromSq toSq
               else StandardMove fromSq toSq
       _ -> error "Invalid move generated" -- Should not happen if logic is consistent
toCoreMove _ T.NullMove = error "Null move generated"

isCastlingMove :: Piece c -> Square -> Square -> Bool
isCastlingMove p from to =
  pieceType p == King && abs (fromEnum (getFile from) - fromEnum (getFile to)) == 2

isEnPassantMove :: Piece c -> Square -> Square -> Board -> Bool
isEnPassantMove p from to b =
  pieceType p == Pawn &&
  getFile from /= getFile to &&
  case getPieceAt to b of
    Nothing -> True
    Just _ -> False

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
applyMove :: forall v c s. (KnownColor c, KnownColor (Opposite c), ChessVariant v) => Move c -> ActiveGame v c s -> MoveResult v (Opposite c)
applyMove = executeMove

instance ChessVariant 'Standard where
  generateMoves (ag :: ActiveGame 'Standard c s) =
    let b = gameBoard ag
        baseBoard = toBaseBoard b
        gs = toGameState ag
        baseMoves = MG.legalMoves baseBoard gs
    in map (toCoreMove b) baseMoves

  executeMove (m :: Move c) (ag :: ActiveGame 'Standard c s) =
    let
        -- 1. Update Board
        c = colorVal @c
        b = gameBoard ag

        b' = case m of
               StandardMove f t -> movePiece f t b
               PromotionMove f t pt ->
                 let b1 = removePieceAt f b -- Remove pawn
                     promoted = mkPiece c pt
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
        newFMN = fullMoveNumber ag + (if c == Black then 1 else 0)

        -- 3. Validation
        baseBoard = toBaseBoard b'

        -- We construct the GameState for the NEXT player to check if THEY are in check/mate.
        nextTurnGS = GS.GameState
          { GS.turn = toColor (colorVal @(Opposite c))
          , GS.castlingRights = toCastlingRights newCR
          , GS.epSquare = case newEP of
                            Nothing -> Nothing
                            Just f -> Just (toSquare (Square f (epRank (colorVal @(Opposite c)))))
          , GS.halfmoveClock = newHMC
          , GS.fullmoveNumber = newFMN
          }

        isChecked = Val.isCheck baseBoard nextTurnGS
        hasMoves = Val.hasLegalMoves baseBoard nextTurnGS

    in case (isChecked, hasMoves) of
         (True, False) -> Checkmate (Winner c)
         (False, False) -> Stalemate
         (True, True) -> Continue (ActiveGame
                                    { gameBoard = b'
                                    , castlingRights = newCR
                                    , enPassantTarget = newEP
                                    , halfMoveClock = newHMC
                                    , fullMoveNumber = newFMN
                                    } :: ActiveGame 'Standard (Opposite c) 'Checked)
         (False, True) -> Continue (ActiveGame
                                    { gameBoard = b'
                                    , castlingRights = newCR
                                    , enPassantTarget = newEP
                                    , halfMoveClock = newHMC
                                    , fullMoveNumber = newFMN
                                    } :: ActiveGame 'Standard (Opposite c) 'Safe)

instance ChessVariant 'Atomic where
  generateMoves (ag :: ActiveGame 'Atomic c s) =
    let b = gameBoard ag
        baseBoard = toBaseBoard b
        gs = toGameState ag
        c = colorVal @c

        pseudos = MG.pseudoLegalMoves baseBoard gs

        -- Filter King Captures: King cannot capture
        isKingCapture :: T.Move -> Bool
        isKingCapture (T.Move f t _) =
           let p = Base.pieceAt baseBoard f
           in fmap T.pieceType p == Just T.King && Base.pieceAt baseBoard t /= Nothing

        -- Filter Self Explosions: Capturing something adjacent to own King
        isSelfExplosion :: T.Move -> Bool
        isSelfExplosion (T.Move f t _) =
           let isCap = Base.pieceAt baseBoard t /= Nothing || isEpCapture
               isEpCapture = case GS.epSquare gs of
                               Just ep -> t == ep && fmap T.pieceType (Base.pieceAt baseBoard f) == Just T.Pawn
                               Nothing -> False
               ownKingSq = MG.kingSquare baseBoard (toColor c)
           in isCap && case ownKingSq of
                         Just k -> chebyshevDist t k <= 1
                         Nothing -> False

        chebyshevDist :: T.Square -> T.Square -> Int
        chebyshevDist (T.Square i1) (T.Square i2) =
           let r1 = i1 `div` 8
               c1 = i1 `mod` 8
               r2 = i2 `div` 8
               c2 = i2 `mod` 8
           in max (abs (r1 - r2)) (abs (c1 - c2))

        atomicMoves = filter (\m -> not (isKingCapture m) && not (isSelfExplosion m)) pseudos

        -- Apply standard check filtering (approximation)
        validMoves = filter (MG.isLegal baseBoard gs) atomicMoves

    in map (toCoreMove b) validMoves

  executeMove (m :: Move c) (ag :: ActiveGame 'Atomic c s) =
    let c = colorVal @c
        oppC = colorVal @(Opposite c)
        b = gameBoard ag

        -- 1. Apply Move Basic (Move piece, handle EP/Castling movement)
        bBasic = case m of
               StandardMove f t -> movePiece f t b
               PromotionMove f t pt ->
                 let b1 = removePieceAt f b
                     promoted = mkPiece c pt
                 in putPieceAt t promoted b1
               CastlingMove f t ->
                 let b1 = movePiece f t b
                     (rf, rt) = getCastlingRookMove f t
                 in movePiece rf rt b1
               EnPassantMove f t ->
                 let b1 = movePiece f t b
                     capSq = getEpCapturedSquare f t
                 in removePieceAt capSq b1

        (from, to) = case m of
                       StandardMove f t -> (f, t)
                       PromotionMove f t _ -> (f, t)
                       CastlingMove f t -> (f, t)
                       EnPassantMove f t -> (f, t)

        -- Check if capture
        isCapture = case m of
                      StandardMove _ t -> getPieceAt t b /= Nothing
                      PromotionMove _ t _ -> getPieceAt t b /= Nothing
                      EnPassantMove _ _ -> True
                      _ -> False

        -- Explosion Logic
        (bFinal, enemyKingExploded) = if isCapture
          then
            let center = to
                -- Capturing piece explodes (remove at center)
                b1 = removePieceAt center bBasic

                -- Surrounding Squares
                surrounds = getAdjacentSquares center

                -- Explode surrounding
                explode sq (board, kingDead) =
                  if sq == (if c == White then blackKing board else whiteKing board) -- Enemy King
                  then (board, True)
                  else
                    case getPieceAt sq board of
                       Just (SomePiece p) ->
                         if pieceType p == Pawn
                         then (board, kingDead)
                         else (removePieceAt sq board, kingDead)
                       Nothing -> (board, kingDead)

                (b2, kDead) = foldr explode (b1, False) surrounds
            in (b2, kDead)
          else (bBasic, False)

        -- State Updates
        newCR = updateCastlingRights (castlingRights ag) from to

        -- EP
        movedPiece = getPieceAt to bBasic -- Note: use bBasic to check piece type before explosion
        isPawn = case movedPiece of
                   Just (SomePiece WPawn) -> True
                   Just (SomePiece BPawn) -> True
                   _ -> False

        newEP = case m of
                  StandardMove f t -> if isPawn && isDoublePush f t then Just (getFile f) else Nothing
                  _ -> Nothing

        newHMC = halfMoveClock ag + 1
        newFMN = fullMoveNumber ag + (if c == Black then 1 else 0)

        nextTurnGS = GS.GameState
          { GS.turn = toColor oppC
          , GS.castlingRights = toCastlingRights newCR
          , GS.epSquare = case newEP of
                            Nothing -> Nothing
                            Just f -> Just (toSquare (Square f (epRank oppC)))
          , GS.halfmoveClock = newHMC
          , GS.fullmoveNumber = newFMN
          }

        baseBoard = toBaseBoard bFinal
        isChecked = Val.isCheck baseBoard nextTurnGS
        hasMoves = Val.hasLegalMoves baseBoard nextTurnGS

    in if enemyKingExploded
       then Checkmate (Winner c)
       else case (isChecked, hasMoves) of
         (True, False) -> Checkmate (Winner c)
         (False, False) -> Stalemate
         (True, True) -> Continue (ActiveGame
                                    { gameBoard = bFinal
                                    , castlingRights = newCR
                                    , enPassantTarget = newEP
                                    , halfMoveClock = newHMC
                                    , fullMoveNumber = newFMN
                                    } :: ActiveGame 'Atomic (Opposite c) 'Checked)
         (False, True) -> Continue (ActiveGame
                                    { gameBoard = bFinal
                                    , castlingRights = newCR
                                    , enPassantTarget = newEP
                                    , halfMoveClock = newHMC
                                    , fullMoveNumber = newFMN
                                    } :: ActiveGame 'Atomic (Opposite c) 'Safe)

getAdjacentSquares :: Square -> [Square]
getAdjacentSquares (Square f r) =
  let fIdx = fromEnum f
      rIdx = fromEnum r
      adjs = [ (f', r') | f' <- [fIdx-1 .. fIdx+1], r' <- [rIdx-1 .. rIdx+1], (f', r') /= (fIdx, rIdx) ]
      valid (fx, rx) = fx >= 0 && fx <= 7 && rx >= 0 && rx <= 7
  in [ Square (toEnum fx) (toEnum rx) | (fx, rx) <- adjs, valid (fx, rx) ]
