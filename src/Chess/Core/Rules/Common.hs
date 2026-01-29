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

module Chess.Core.Rules.Common where

import Chess.Core.Rules.Class
import Chess.Core.Board.Internal
import Chess.Core.Game.Internal
import Chess.Core.Move.Internal

import qualified Chess.Types as T
import qualified Chess.Board.Base as Base
import qualified Chess.Board.GameState as GS
import qualified Chess.Board.MoveGen as MG
import qualified Chess.Board.Validation as Val
import qualified Chess.Bitboard as BB
import Data.Bits (setBit, clearBit, (.&.), (.|.), testBit, complement)
import Data.Word (Word8)
import qualified Data.Map as Map
import qualified Data.Vector as V

-- | Convert Core Color to Engine Color
toColor :: Color -> T.Color
toColor White = T.White
toColor Black = T.Black

-- | Convert Core Square to Engine Square
{-# INLINE toSquare #-}
toSquare :: Square -> T.Square
toSquare (Square f r) = T.Square (fromEnum r * 8 + fromEnum f)

-- | Convert Engine Square to Core Square
squaresVector :: V.Vector Square
squaresVector = V.generate 64 (\i -> Square (toEnum (i `mod` 8)) (toEnum (i `div` 8)))
{-# NOINLINE squaresVector #-}

fromSquare :: T.Square -> Square
fromSquare (T.Square i) = V.unsafeIndex squaresVector i

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
  , GS.zobristHash = 0
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

-- Generate Legal Moves
generateLegalMoves :: forall v c s. (KnownColor c, ChessVariant v) => ActiveGame v c s -> [Move c]
generateLegalMoves = generateMoves

toCoreMove :: MG.GenMove -> Move c
toCoreMove gm =
  case gm of
    MG.GenQuiet f t pt ->
        QuietMove (fromSquare f) (fromSquare t) (fromPieceType pt)
    MG.GenCapture f t pt cap ->
        CaptureMove (fromSquare f) (fromSquare t) (fromPieceType pt) (fromPieceType cap)
    MG.GenEnPassant f t ->
        EnPassantMove (fromSquare f) (fromSquare t)
    MG.GenCastling f t ->
        CastlingMove (fromSquare f) (fromSquare t)
    MG.GenPromotion f t ppt ->
        PromotionMove (fromSquare f) (fromSquare t) (fromPieceType ppt)
    MG.GenPromotionCapture f t ppt cap ->
        PromotionCaptureMove (fromSquare f) (fromSquare t) (fromPieceType ppt) (fromPieceType cap)

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
       QuietMove f t pt ->
          let c = toColor (colorVal @c)
          in Base.unsafeMovePiece b (toSquare f) (toSquare t) c (toPieceType pt)

       CaptureMove f t pt cap ->
          let c = toColor (colorVal @c)
              oppC = Base.oppositeColor c
              b1 = Base.unsafeRemovePiece b (toSquare t) oppC (toPieceType cap)
          in Base.unsafeMovePiece b1 (toSquare f) (toSquare t) c (toPieceType pt)

       PromotionMove f t promo ->
          let c = toColor (colorVal @c)
              b1 = Base.unsafeRemovePiece b (toSquare f) c T.Pawn
              promoted = T.Piece c (toPieceType promo)
          in Base.unsafePutPiece b1 (toSquare t) promoted

       PromotionCaptureMove f t promo cap ->
          let c = toColor (colorVal @c)
              oppC = Base.oppositeColor c
              b1 = Base.unsafeRemovePiece b (toSquare f) c T.Pawn
              b2 = Base.unsafeRemovePiece b1 (toSquare t) oppC (toPieceType cap)
              promoted = T.Piece c (toPieceType promo)
          in Base.unsafePutPiece b2 (toSquare t) promoted

       CastlingMove f t ->
          let c = toColor (colorVal @c)
              b1 = Base.unsafeMovePiece b (toSquare f) (toSquare t) c T.King
              (rf, rt) = getCastlingRookMove f t
          in Base.unsafeMovePiece b1 (toSquare rf) (toSquare rt) c T.Rook

       EnPassantMove f t ->
          let c = toColor (colorVal @c)
              oppC = Base.oppositeColor c
              capSq = getEpCapturedSquare f t
              b1 = Base.unsafeRemovePiece b (toSquare capSq) oppC T.Pawn
          in Base.unsafeMovePiece b1 (toSquare f) (toSquare t) c T.Pawn

       DropMove p t ->
          let promoted = T.Piece (toColor (colorVal @c)) (toPieceType p)
          in Base.putPiece b (toSquare t) promoted

       Castling960Move k r ->
          let
              (Square kf kr) = k
              (Square rf _) = r
              isKingSide = fromEnum rf > fromEnum kf
              rank = kr

              -- Target Files
              -- King Side: King -> G (FileG = 6), Rook -> F (FileF = 5)
              -- Queen Side: King -> C (FileC = 2), Rook -> D (FileD = 3)
              kTarget = if isKingSide then Square FileG rank else Square FileC rank
              rTarget = if isKingSide then Square FileF rank else Square FileD rank

              kPiece = Base.pieceAt b (toSquare k)
              rPiece = Base.pieceAt b (toSquare r)

              b1 = Base.removePieceAt b (toSquare k)
              b2 = Base.removePieceAt b1 (toSquare r)

              b3 = case kPiece of
                     Just p -> Base.putPiece b2 (toSquare kTarget) p
                     Nothing -> b2
              b4 = case rPiece of
                     Just p -> Base.putPiece b3 (toSquare rTarget) p
                     Nothing -> b3
          in b4

-- Apply Move
-- Apply Move Helpers

setStatus :: SCheckStatus new -> ActiveGame v c old -> ActiveGame v c new
setStatus s ag = ActiveGame
  { internalBoard = internalBoard ag
  , castlingRights = castlingRights ag
  , enPassantTarget = enPassantTarget ag
  , halfMoveClock = halfMoveClock ag
  , fullMoveNumber = fullMoveNumber ag
  , variantState = variantState ag
  , checkStatus = s
  }

genericApplyMove :: forall v c s. (KnownColor c, KnownColor (Opposite c), ChessVariant v) => Move c -> ActiveGame v c s -> GameTransition v (Opposite c)
genericApplyMove m ag =
    let
        c = colorVal @c
        internalB = internalBoard ag
        internalB' = applyMoveBase m internalB
        (from, to) = case m of
                       QuietMove f t _ -> (f, t)
                       CaptureMove f t _ _ -> (f, t)
                       PromotionMove f t _ -> (f, t)
                       PromotionCaptureMove f t _ _ -> (f, t)
                       CastlingMove f t -> (f, t)
                       EnPassantMove f t -> (f, t)
                       DropMove _ t -> (t, t)
                       Castling960Move _ _ -> error "Castling960Move not supported in genericApplyMove"

        newCR = updateCastlingRights (castlingRights ag) from to

        isPawn = case m of
                   QuietMove _ _ pt -> pt == Pawn
                   CaptureMove _ _ pt _ -> pt == Pawn
                   EnPassantMove _ _ -> True
                   PromotionMove _ _ _ -> True
                   PromotionCaptureMove _ _ _ _ -> True
                   DropMove pt _ -> pt == Pawn
                   _ -> False

        newEP = case m of
                  QuietMove f t _ -> if isPawn && isDoublePush f t then Just (getFile f) else Nothing
                  _ -> Nothing

        isCapture = case m of
                      CaptureMove {} -> True
                      PromotionCaptureMove {} -> True
                      EnPassantMove _ _ -> True
                      _ -> False

        newHMC = if isPawn || isCapture then 0 else halfMoveClock ag + 1
        newFMN = fullMoveNumber ag + (if c == Black then 1 else 0)

        nextAg = ActiveGame
          { internalBoard = internalB'
          , castlingRights = newCR
          , enPassantTarget = newEP
          , halfMoveClock = newHMC
          , fullMoveNumber = newFMN
          , variantState = variantState ag
          , checkStatus = SUnchecked
          }
    in Transition nextAg

genericExecuteMove :: forall v c s. (KnownColor c, KnownColor (Opposite c), ChessVariant v) => Move c -> ActiveGame v c s -> MoveResult v (Opposite c)
genericExecuteMove m ag =
  case applyMove m ag of
    Transition nextAg ->
      let
         checked = Val.isCheck (internalBoard nextAg) (toGameState nextAg)

         (hasMoves, nextAgChecked) = if checked
            then
               let agChecked = setStatus SChecked nextAg
               in (not (null (generateMoves agChecked)), Right agChecked)
            else
               let agSafe = setStatus SSafe nextAg
               in (not (null (generateMoves agSafe)), Left agSafe)

      in if checked
         then if hasMoves
              then case nextAgChecked of Right finalAg -> Continue finalAg; Left _ -> error "Impossible"
              else Checkmate (Winner (colorVal @c))
         else if hasMoves
              then case nextAgChecked of Left finalAg -> Continue finalAg; Right _ -> error "Impossible"
              else Stalemate

getAdjacentSquares :: Square -> [Square]
getAdjacentSquares (Square f r) =
  let fIdx = fromEnum f
      rIdx = fromEnum r
      adjs = [ (f', r') | f' <- [fIdx-1 .. fIdx+1], r' <- [rIdx-1 .. rIdx+1], (f', r') /= (fIdx, rIdx) ]
      valid (fx, rx) = fx >= 0 && fx <= 7 && rx >= 0 && rx <= 7
  in [ Square (toEnum fx) (toEnum rx) | (fx, rx) <- adjs, valid (fx, rx) ]
