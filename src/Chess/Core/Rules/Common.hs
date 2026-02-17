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
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
import Control.Monad (forM_)

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
  , Base.whiteDiagonal = wDiagonal
  , Base.whiteOrthogonal = wOrthogonal
  , Base.blackDiagonal = bDiagonal
  , Base.blackOrthogonal = bOrthogonal
  , Base.mailbox = mb
  }
  where
    mb = U.create $ do
        v <- UM.replicate 64 0

        -- White King
        let wkIdx = T.unSquare (toSquare (whiteKing b))
        UM.unsafeWrite v wkIdx (Base.pieceToWord8 (T.Piece T.White T.King))

        -- Black King
        let bkIdx = T.unSquare (toSquare (blackKing b))
        UM.unsafeWrite v bkIdx (Base.pieceToWord8 (T.Piece T.Black T.King))

        -- Pawns
        forM_ (Map.toList (pawns b)) $ \((f, pr), c) -> do
             let sq = Square f (toRank pr)
             let idx = T.unSquare (toSquare sq)
             let pc = T.Piece (toColor c) T.Pawn
             UM.unsafeWrite v idx (Base.pieceToWord8 pc)

        -- White Pieces
        forM_ (Map.toList (whitePieces b)) $ \(sq, mp) -> do
             let idx = T.unSquare (toSquare sq)
             let pt = toPieceType (mmToPieceType mp)
             let pc = T.Piece T.White pt
             UM.unsafeWrite v idx (Base.pieceToWord8 pc)

        -- Black Pieces
        forM_ (Map.toList (blackPieces b)) $ \(sq, mp) -> do
             let idx = T.unSquare (toSquare sq)
             let pt = toPieceType (mmToPieceType mp)
             let pc = T.Piece T.Black pt
             UM.unsafeWrite v idx (Base.pieceToWord8 pc)

        return v

    mmToPieceType :: MajorMinorPiece c -> PieceType
    mmToPieceType MQueen = Queen
    mmToPieceType MRook = Rook
    mmToPieceType MBishop = Bishop
    mmToPieceType MKnight = Knight

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

    wDiagonal = wBishops .|. wQueens
    wOrthogonal = wRooks .|. wQueens
    bDiagonal = bBishops .|. bQueens
    bOrthogonal = bRooks .|. bQueens

    wOcc = wPawns .|. wKnights .|. wBishops .|. wRooks .|. wQueens .|. wKings
    bOcc = bPawns .|. bKnights .|. bBishops .|. bRooks .|. bQueens .|. bKings

-- | Convert ActiveGame to Engine GameState
toGameState :: forall v c s. KnownColor c => ActiveGame v c s -> GS.GameState
toGameState ag = gameState ag

epRank :: Color -> Rank
epRank White = Rank6
epRank Black = Rank3

-- | Check if side `c` is in check.
isCheck :: Board -> Color -> Bool
isCheck b c = Val.isCheck (toBaseBoard b) (dummyGameState c)
  where
    dummyGameState col = GS.initialGameState { GS.turn = toColor col }

-- Generate Legal Moves
generateLegalMoves :: forall v c s. (KnownColor c, ChessVariant v) => ActiveGame v c s -> [Move c]
generateLegalMoves = generateMoves

toCoreMove :: MG.GenMove -> Move c
toCoreMove = Move

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

updateCastlingRights :: forall c. KnownColor c => GS.GameState -> Move c -> GS.GameState
updateCastlingRights gs m =
  let c = colorVal @c
      (from, to) = case m of
                     QuietMove f t _ -> (f, t)
                     CaptureMove f t _ _ -> (f, t)
                     PromotionMove f t _ -> (f, t)
                     PromotionCaptureMove f t _ _ -> (f, t)
                     CastlingMove f t -> (f, t)
                     EnPassantMove f t -> (f, t)
                     DropMove _ t -> (t, t)
                     Castling960Move k r -> (k, r)

      -- 1. Remove rights if 'from' or 'to' was a rook (or captured rook)
      gs1 = GS.removeCastlingRight gs (toSquare from)
      gs2 = GS.removeCastlingRight gs1 (toSquare to)

      -- 2. Remove color rights if King moved
      isKing = case m of
                 QuietMove _ _ pt -> pt == King
                 CaptureMove _ _ pt _ -> pt == King
                 CastlingMove _ _ -> True
                 Castling960Move _ _ -> True
                 _ -> False

      mask = if c == White then complement BB.bbRank1 else complement BB.bbRank8
  in if isKing
     then gs2 { GS.castlingRights = GS.castlingRights gs2 .&. mask }
     else gs2

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
  , gameState = gameState ag
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

        gs = gameState ag
        gs3 = updateCastlingRights gs m

        isPawn = case m of
                   QuietMove _ _ pt -> pt == Pawn
                   CaptureMove _ _ pt _ -> pt == Pawn
                   EnPassantMove _ _ -> True
                   PromotionMove _ _ _ -> True
                   PromotionCaptureMove _ _ _ _ -> True
                   DropMove pt _ -> pt == Pawn
                   _ -> False

        newEP = case m of
                  QuietMove f t _ ->
                    if isPawn && isDoublePush f t
                    then toSquare (Square (getFile f) (epRank (colorVal @(Opposite c))))
                    else T.NoSquare
                  _ -> T.NoSquare

        isCapture = case m of
                      CaptureMove {} -> True
                      PromotionCaptureMove {} -> True
                      EnPassantMove _ _ -> True
                      _ -> False

        newHMC = if isPawn || isCapture then 0 else GS.halfmoveClock gs + 1
        newFMN = GS.fullmoveNumber gs + (if c == Black then 1 else 0)

        newGS = gs3
          { GS.turn = toColor (colorVal @(Opposite c))
          , GS.epSquare = newEP
          , GS.halfmoveClock = newHMC
          , GS.fullmoveNumber = newFMN
          , GS.zobristHash = 0 -- Reset hash as we don't track it incrementally yet
          }

        nextAg = ActiveGame
          { internalBoard = internalB'
          , gameState = newGS
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
