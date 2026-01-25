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
           , checkStatus = SSafe
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
               then return $ InProgressGame (ActiveGame baseBoard cr ep hmc fmn () SChecked :: ActiveGame 'Standard 'White 'Checked)
               else return $ InProgressGame (ActiveGame baseBoard cr ep hmc fmn () SSafe :: ActiveGame 'Standard 'White 'Safe)
      Black -> if checked
               then return $ InProgressGame (ActiveGame baseBoard cr ep hmc fmn () SChecked :: ActiveGame 'Standard 'Black 'Checked)
               else return $ InProgressGame (ActiveGame baseBoard cr ep hmc fmn () SSafe :: ActiveGame 'Standard 'Black 'Safe)
    else Nothing

-- | Create a game from FEN string (Crazyhouse variant).
crazyhouseGameFromFEN :: String -> Maybe (Game 'Crazyhouse 'Active)
crazyhouseGameFromFEN s = do
  (baseBoard, gs, extra) <- Fen.parseFenRest s

  let c = case GS.turn gs of
            T.White -> White
            T.Black -> Black

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

      -- Parse pockets
      pocketStr = case filter (\x -> not (null x) && head x == '[') extra of
                    (p:_) -> p
                    [] -> "[]"

      -- Helper to add to pocket
      addToPockets p pt = case pt of
          Pawn   -> p { pocketPawns   = pocketPawns p + 1 }
          Knight -> p { pocketKnights = pocketKnights p + 1 }
          Bishop -> p { pocketBishops = pocketBishops p + 1 }
          Rook   -> p { pocketRooks   = pocketRooks p + 1 }
          Queen  -> p { pocketQueens  = pocketQueens p + 1 }
          King   -> p -- Should not happen

      (wPocket, bPocket) = foldr dist (emptyPockets, emptyPockets) (filter (`elem` "PNBRQKpnbrqk") pocketStr)
        where
          dist char (wm, bm) =
             case T.fromSymbol char of
                Just (T.Piece T.White pt) -> (addToPockets wm (fromPieceType pt), bm)
                Just (T.Piece T.Black pt) -> (wm, addToPockets bm (fromPieceType pt))
                Nothing -> (wm, bm)

      vs = CrazyhouseState wPocket bPocket 0

      checked = Val.isCheck baseBoard gs

      -- Check if any moves available (including drops)
      generateDrops :: forall col. KnownColor col => [Move col]
      generateDrops =
           let col = colorVal @col
               pocket = if col == White then wPocket else bPocket
               emptySqs = [ Square f r | f <- [FileA .. FileH], r <- [Rank1 .. Rank8], Base.pieceAt baseBoard (toSquare (Square f r)) == Nothing ]

               genDrops pt count =
                  if count <= 0 then []
                  else
                     let validSquares = if pt == Pawn
                                        then filter (\(Square _ r) -> r /= Rank1 && r /= Rank8) emptySqs
                                        else emptySqs
                     in map (DropMove pt) validSquares

               drops = concat
                   [ genDrops Pawn (pocketPawns pocket)
                   , genDrops Knight (pocketKnights pocket)
                   , genDrops Bishop (pocketBishops pocket)
                   , genDrops Rook (pocketRooks pocket)
                   , genDrops Queen (pocketQueens pocket)
                   ]

               safe m = not (Val.isCheck (applyMoveBase m baseBoard) gs)
           in filter safe drops

      hasMoves :: forall col. KnownColor col => Bool
      hasMoves = Val.hasLegalMoves baseBoard gs || not (null (generateDrops @col))

  case c of
      White -> if hasMoves @'White
               then if checked
                    then return $ InProgressGame (ActiveGame baseBoard cr ep hmc fmn vs SChecked :: ActiveGame 'Crazyhouse 'White 'Checked)
                    else return $ InProgressGame (ActiveGame baseBoard cr ep hmc fmn vs SSafe :: ActiveGame 'Crazyhouse 'White 'Safe)
               else Nothing
      Black -> if hasMoves @'Black
               then if checked
                    then return $ InProgressGame (ActiveGame baseBoard cr ep hmc fmn vs SChecked :: ActiveGame 'Crazyhouse 'Black 'Checked)
                    else return $ InProgressGame (ActiveGame baseBoard cr ep hmc fmn vs SSafe :: ActiveGame 'Crazyhouse 'Black 'Safe)
               else Nothing

-- Type-level Opposite Color
type family Opposite (c :: Color) :: Color where
  Opposite 'White = 'Black
  Opposite 'Black = 'White

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

-- | Convert ActiveGame to Engine GameState for Move Generation
-- Applies optimization: if Checked, mask castling rights to avoid generating castling moves.
toMoveGenGameState :: forall v c s. KnownColor c => ActiveGame v c s -> GS.GameState
toMoveGenGameState ag =
  let gs = toGameState ag
  in case checkStatus ag of
       SChecked -> gs { GS.castlingRights = 0 }
       SSafe    -> gs

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

-- | Class for Chess Variants
class ChessVariant (v :: Variant) where
  generateMoves :: KnownColor c => ActiveGame v c s -> [Move c]
  executeMove :: (KnownColor c, KnownColor (Opposite c)) => Move c -> ActiveGame v c s -> MoveResult v (Opposite c)

-- Generate Legal Moves
generateLegalMoves :: forall v c s. (KnownColor c, ChessVariant v) => ActiveGame v c s -> [Move c]
generateLegalMoves = generateMoves

toCoreMove :: MG.GenMove -> Move c
toCoreMove (MG.GenMove (T.Move f t promo) pt captured) =
  let fromSq = fromSquare f
      toSq = fromSquare t
  in case promo of
       Just ppt ->
          PromotionMove fromSq toSq (fromPieceType pt)
       Nothing ->
          if pt == T.King && abs (T.unSquare f - T.unSquare t) == 2
          then CastlingMove fromSq toSq
          else if pt == T.Pawn && captured == Nothing && T.squareFile f /= T.squareFile t
               then EnPassantMove fromSq toSq
               else StandardMove fromSq toSq
toCoreMove (MG.GenMove (T.DropMove _ _) _ _) = error "DropMove in GenMove"
toCoreMove (MG.GenMove T.NullMove _ _) = error "NullMove in GenMove"

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

-- Apply Move
applyMove :: forall v c s. (KnownColor c, KnownColor (Opposite c), ChessVariant v) => Move c -> ActiveGame v c s -> MoveResult v (Opposite c)
applyMove = executeMove

instance ChessVariant 'Standard where
  generateMoves (ag :: ActiveGame 'Standard c s) =
    let baseBoard = internalBoard ag
        gs = toMoveGenGameState ag
        baseMoves = MG.legalGenMoves baseBoard gs
    in map toCoreMove baseMoves

  executeMove (m :: Move c) (ag :: ActiveGame 'Standard c s) =
    let
        c = colorVal @c
        internalB = internalBoard ag
        internalB' = applyMoveBase m internalB
        (from, to) = case m of
                       StandardMove f t -> (f, t)
                       PromotionMove f t _ -> (f, t)
                       CastlingMove f t -> (f, t)
                       EnPassantMove f t -> (f, t)
                       DropMove _ t -> (t, t)

        newCR = updateCastlingRights (castlingRights ag) from to

        movedPiece = Base.pieceAt internalB' (toSquare to)
        isPawn = case movedPiece of
                   Just p -> T.pieceType p == T.Pawn
                   _ -> False

        newEP = case m of
                  StandardMove f t -> if isPawn && isDoublePush f t then Just (getFile f) else Nothing
                  _ -> Nothing

        newHMC = halfMoveClock ag + 1
        newFMN = fullMoveNumber ag + (if c == Black then 1 else 0)

        baseBoard = internalB'
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
         (True, True) -> Continue (ActiveGame internalB' newCR newEP newHMC newFMN () SChecked :: ActiveGame 'Standard (Opposite c) 'Checked)
         (False, True) -> Continue (ActiveGame internalB' newCR newEP newHMC newFMN () SSafe :: ActiveGame 'Standard (Opposite c) 'Safe)

instance ChessVariant 'ThreeCheck where
  generateMoves (ag :: ActiveGame 'ThreeCheck c s) =
    let baseBoard = internalBoard ag
        gs = toMoveGenGameState ag
        baseMoves = MG.legalGenMoves baseBoard gs
    in map toCoreMove baseMoves

  executeMove (m :: Move c) (ag :: ActiveGame 'ThreeCheck c s) =
    let
        c = colorVal @c
        oppC = colorVal @(Opposite c)
        internalB = internalBoard ag
        internalB' = applyMoveBase m internalB
        (from, to) = case m of
                       StandardMove f t -> (f, t)
                       PromotionMove f t _ -> (f, t)
                       CastlingMove f t -> (f, t)
                       EnPassantMove f t -> (f, t)
                       DropMove _ t -> (t, t)

        newCR = updateCastlingRights (castlingRights ag) from to
        movedPiece = Base.pieceAt internalB' (toSquare to)
        isPawn = case movedPiece of
                   Just p -> T.pieceType p == T.Pawn
                   _ -> False

        newEP = case m of
                  StandardMove f t -> if isPawn && isDoublePush f t then Just (getFile f) else Nothing
                  _ -> Nothing

        newHMC = halfMoveClock ag + 1
        newFMN = fullMoveNumber ag + (if c == Black then 1 else 0)

        baseBoard = internalB'
        nextTurnGS = GS.GameState
          { GS.turn = toColor oppC
          , GS.castlingRights = toCastlingRights newCR
          , GS.epSquare = case newEP of
                            Nothing -> Nothing
                            Just f -> Just (toSquare (Square f (epRank oppC)))
          , GS.halfmoveClock = newHMC
          , GS.fullmoveNumber = newFMN
          }

        isChecked = Val.isCheck baseBoard nextTurnGS
        hasMoves = Val.hasLegalMoves baseBoard nextTurnGS

        (wChecks, bChecks) = variantState ag
        (wChecks', bChecks') = if isChecked
                               then if c == White then (wChecks + 1, bChecks) else (wChecks, bChecks + 1)
                               else (wChecks, bChecks)

        newVariantState = (wChecks', bChecks')
        winByCheck = (if c == White then wChecks' else bChecks') >= 3

    in if winByCheck
       then Checkmate (Winner c)
       else case (isChecked, hasMoves) of
         (True, False) -> Checkmate (Winner c)
         (False, False) -> Stalemate
         (True, True) -> Continue (ActiveGame internalB' newCR newEP newHMC newFMN newVariantState SChecked :: ActiveGame 'ThreeCheck (Opposite c) 'Checked)
         (False, True) -> Continue (ActiveGame internalB' newCR newEP newHMC newFMN newVariantState SSafe :: ActiveGame 'ThreeCheck (Opposite c) 'Safe)

instance ChessVariant 'Atomic where
  generateMoves (ag :: ActiveGame 'Atomic c s) =
    let baseBoard = internalBoard ag
        gs = toMoveGenGameState ag
        c = colorVal @c

        pseudos = MG.pseudoLegalMoves baseBoard gs

        isKingCapture :: T.Move -> Bool
        isKingCapture (T.Move f t _) =
           let p = Base.pieceAt baseBoard f
           in fmap T.pieceType p == Just T.King && Base.pieceAt baseBoard t /= Nothing
        isKingCapture T.NullMove = False
        isKingCapture (T.DropMove _ _) = False

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
        isSelfExplosion T.NullMove = False
        isSelfExplosion (T.DropMove _ _) = False

        chebyshevDist :: T.Square -> T.Square -> Int
        chebyshevDist (T.Square i1) (T.Square i2) =
           let r1 = i1 `div` 8
               c1 = i1 `mod` 8
               r2 = i2 `div` 8
               c2 = i2 `mod` 8
           in max (abs (r1 - r2)) (abs (c1 - c2))

        atomicMoves = filter (\(MG.GenMove m _ _) -> not (isKingCapture m) && not (isSelfExplosion m)) pseudos
        validMoves = filter (MG.isLegal baseBoard gs) atomicMoves

    in map toCoreMove validMoves

  executeMove (m :: Move c) (ag :: ActiveGame 'Atomic c s) =
    let c = colorVal @c
        oppC = colorVal @(Opposite c)
        internalB = internalBoard ag

        bBasic = applyMoveBase m internalB
        (from, to) = case m of
                       StandardMove f t -> (f, t)
                       PromotionMove f t _ -> (f, t)
                       CastlingMove f t -> (f, t)
                       EnPassantMove f t -> (f, t)
                       DropMove _ t -> (t, t)

        isCapture = case m of
                      StandardMove _ t -> Base.pieceAt internalB (toSquare t) /= Nothing
                      PromotionMove _ t _ -> Base.pieceAt internalB (toSquare t) /= Nothing
                      EnPassantMove _ _ -> True
                      _ -> False

        (bFinal, enemyKingExploded) = if isCapture
          then
            let center = to
                b1 = Base.removePieceAt bBasic (toSquare center)
                surrounds = getAdjacentSquares center
                enemyKingSq = MG.kingSquare bBasic (toColor oppC)
                explode sq (board, kingDead) =
                  if Just (toSquare sq) == enemyKingSq
                  then (board, True)
                  else
                    case Base.pieceAt board (toSquare sq) of
                       Just p ->
                         if T.pieceType p == T.Pawn
                         then (board, kingDead)
                         else (Base.removePieceAt board (toSquare sq), kingDead)
                       Nothing -> (board, kingDead)
                (b2, kDead) = foldr explode (b1, False) surrounds
            in (b2, kDead)
          else (bBasic, False)

        newCR = updateCastlingRights (castlingRights ag) from to
        movedPiece = Base.pieceAt bBasic (toSquare to)
        isPawn = case movedPiece of
                   Just p -> T.pieceType p == T.Pawn
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

        baseBoard = bFinal
        isChecked = Val.isCheck baseBoard nextTurnGS
        hasMoves = Val.hasLegalMoves baseBoard nextTurnGS

    in if enemyKingExploded
       then Checkmate (Winner c)
       else case (isChecked, hasMoves) of
         (True, False) -> Checkmate (Winner c)
         (False, False) -> Stalemate
         (True, True) -> Continue (ActiveGame bFinal newCR newEP newHMC newFMN () SChecked :: ActiveGame 'Atomic (Opposite c) 'Checked)
         (False, True) -> Continue (ActiveGame bFinal newCR newEP newHMC newFMN () SSafe :: ActiveGame 'Atomic (Opposite c) 'Safe)

instance ChessVariant 'KingOfTheHill where
  generateMoves (ag :: ActiveGame 'KingOfTheHill c s) =
    let baseBoard = internalBoard ag
        gs = toMoveGenGameState ag
        baseMoves = MG.legalGenMoves baseBoard gs
    in map toCoreMove baseMoves

  executeMove (m :: Move c) (ag :: ActiveGame 'KingOfTheHill c s) =
    let
        c = colorVal @c
        internalB = internalBoard ag
        internalB' = applyMoveBase m internalB
        (from, to) = case m of
                       StandardMove f t -> (f, t)
                       PromotionMove f t _ -> (f, t)
                       CastlingMove f t -> (f, t)
                       EnPassantMove f t -> (f, t)
                       DropMove _ t -> (t, t)

        newCR = updateCastlingRights (castlingRights ag) from to
        movedPiece = Base.pieceAt internalB' (toSquare to)
        isPawn = case movedPiece of
                   Just p -> T.pieceType p == T.Pawn
                   _ -> False

        isKing = case movedPiece of
                   Just p -> T.pieceType p == T.King
                   _ -> False

        newEP = case m of
                  StandardMove f t -> if isPawn && isDoublePush f t then Just (getFile f) else Nothing
                  _ -> Nothing

        newHMC = halfMoveClock ag + 1
        newFMN = fullMoveNumber ag + (if c == Black then 1 else 0)

        baseBoard = internalB'
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

        kingInCenter = isKing && to `elem` centerSquares
        centerSquares = [ Square FileE Rank4, Square FileD Rank4
                        , Square FileE Rank5, Square FileD Rank5 ]

    in if kingInCenter
       then Checkmate (Winner c)
       else case (isChecked, hasMoves) of
         (True, False) -> Checkmate (Winner c)
         (False, False) -> Stalemate
         (True, True) -> Continue (ActiveGame internalB' newCR newEP newHMC newFMN () SChecked :: ActiveGame 'KingOfTheHill (Opposite c) 'Checked)
         (False, True) -> Continue (ActiveGame internalB' newCR newEP newHMC newFMN () SSafe :: ActiveGame 'KingOfTheHill (Opposite c) 'Safe)

instance ChessVariant 'RacingKings where
  generateMoves (ag :: ActiveGame 'RacingKings c s) =
    let baseBoard = internalBoard ag
        gs = toMoveGenGameState ag
        baseMoves = MG.legalGenMoves baseBoard gs
        coreMoves = map toCoreMove baseMoves
        c = colorVal @c
        oppC = opposite c

        noGiveCheck m =
            let baseNext = applyMoveBase m baseBoard
            in not (Val.isCheck baseNext (dummyGameState oppC))
          where
            dummyGameState col = GS.initialGameState { GS.turn = toColor col }

    in filter noGiveCheck coreMoves

  executeMove (m :: Move c) (ag :: ActiveGame 'RacingKings c s) =
    let c = colorVal @c
        internalB = internalBoard ag
        internalB' = applyMoveBase m internalB
        (from, to) = case m of
                       StandardMove f t -> (f, t)
                       PromotionMove f t _ -> (f, t)
                       CastlingMove f t -> (f, t)
                       EnPassantMove f t -> (f, t)
                       DropMove _ t -> (t, t)

        newCR = updateCastlingRights (castlingRights ag) from to
        movedPiece = Base.pieceAt internalB' (toSquare to)
        isPawn = case movedPiece of
                   Just p -> T.pieceType p == T.Pawn
                   _ -> False
        newEP = case m of
                  StandardMove f t -> if isPawn && isDoublePush f t then Just (getFile f) else Nothing
                  _ -> Nothing
        newHMC = halfMoveClock ag + 1
        newFMN = fullMoveNumber ag + (if c == Black then 1 else 0)

        nextGameCandidate :: ActiveGame 'RacingKings (Opposite c) 'Safe
        nextGameCandidate = ActiveGame internalB' newCR newEP newHMC newFMN () SSafe

        nextMoves = generateMoves nextGameCandidate
        realHasMoves = not (null nextMoves)

        wKingSq = MG.kingSquare internalB' T.White
        bKingSq = MG.kingSquare internalB' T.Black
        wInGoal = case wKingSq of Just sq -> T.squareRank sq == 7; _ -> False
        bInGoal = case bKingSq of Just sq -> T.squareRank sq == 7; _ -> False

        result =
             if c == White
             then if wInGoal
                  then if realHasMoves
                       then Continue nextGameCandidate
                       else Checkmate (Winner White)
                  else
                       if realHasMoves then Continue nextGameCandidate else Stalemate
             else
                  if bInGoal && wInGoal then Checkmate Draw else
                  if wInGoal then Checkmate (Winner White) else
                  if bInGoal then Checkmate (Winner Black) else
                  if realHasMoves then Continue nextGameCandidate else Stalemate

    in result

instance ChessVariant 'Crazyhouse where
  generateMoves (ag :: ActiveGame 'Crazyhouse c s) =
    let baseBoard = internalBoard ag
        gs = toMoveGenGameState ag
        c = colorVal @c

        baseMoves = MG.legalGenMoves baseBoard gs
        standardMoves = map toCoreMove baseMoves

        (CrazyhouseState wPocket bPocket _) = variantState ag
        pocket = if c == White then wPocket else bPocket

        emptySquares = [ Square f r | f <- [FileA .. FileH], r <- [Rank1 .. Rank8], Base.pieceAt baseBoard (toSquare (Square f r)) == Nothing ]

        genDrops pt count =
          if count <= 0 then []
          else
             let validSquares = if pt == Pawn
                                then filter (\(Square _ r) -> r /= Rank1 && r /= Rank8) emptySquares
                                else emptySquares
             in map (DropMove pt) validSquares

        dropMoves = concat
           [ genDrops Pawn (pocketPawns pocket)
           , genDrops Knight (pocketKnights pocket)
           , genDrops Bishop (pocketBishops pocket)
           , genDrops Rook (pocketRooks pocket)
           , genDrops Queen (pocketQueens pocket)
           ]

        isSafeDrop m =
           let nextBase = applyMoveBase m baseBoard
           in not (Val.isCheck nextBase gs)

        validDropMoves = filter isSafeDrop dropMoves

    in standardMoves ++ validDropMoves

  executeMove (m :: Move c) (ag :: ActiveGame 'Crazyhouse c s) =
    let
        c = colorVal @c
        oppC = colorVal @(Opposite c)
        internalB = internalBoard ag
        internalB' = applyMoveBase m internalB

        (CrazyhouseState wPocket bPocket promoted) = variantState ag

        -- Helper to modify pockets
        updatePockets p pt f = case pt of
          Pawn   -> p { pocketPawns   = f (pocketPawns p) }
          Knight -> p { pocketKnights = f (pocketKnights p) }
          Bishop -> p { pocketBishops = f (pocketBishops p) }
          Rook   -> p { pocketRooks   = f (pocketRooks p) }
          Queen  -> p { pocketQueens  = f (pocketQueens p) }
          King   -> p

        (from, to) = case m of
                       StandardMove f t -> (f, t)
                       PromotionMove f t _ -> (f, t)
                       CastlingMove f t -> (f, t)
                       EnPassantMove f t -> (f, t)
                       DropMove _ t -> (t, t)

        ((wPocket', bPocket'), promoted') = case m of
           DropMove p _ ->
              let pockets = if c == White
                            then (updatePockets wPocket p (\x -> x - 1), bPocket)
                            else (wPocket, updatePockets bPocket p (\x -> x - 1))
              in (pockets, promoted)
           _ ->
              let capture = case m of
                              StandardMove _ t -> Base.pieceAt internalB (toSquare t)
                              PromotionMove _ t _ -> Base.pieceAt internalB (toSquare t)
                              EnPassantMove _ _ -> Just (T.Piece (toColor oppC) T.Pawn)
                              _ -> Nothing

                  capturedSquare = case m of
                                     EnPassantMove f t -> Just (getEpCapturedSquare f t)
                                     _ -> if capture /= Nothing then Just to else Nothing

                  pockets' = case capture of
                     Just (T.Piece _ pt) ->
                        let capturedType = fromPieceType pt
                            isPromoted = case capturedSquare of
                                           Just sq -> testBit promoted (T.unSquare (toSquare sq))
                                           Nothing -> False
                            addToPocket = if isPromoted then Pawn else capturedType
                            (wm, bm) = (wPocket, bPocket)
                        in if c == White
                           then (updatePockets wm addToPocket (+1), bm)
                           else (wm, updatePockets bm addToPocket (+1))
                     Nothing -> (wPocket, bPocket)

                  -- Update Promoted Bitboard
                  p1 = case capturedSquare of
                          Just sq -> clearBit promoted (T.unSquare (toSquare sq))
                          Nothing -> promoted

                  isMovingPromoted = testBit p1 (T.unSquare (toSquare from))
                  p2 = if isMovingPromoted
                       then setBit (clearBit p1 (T.unSquare (toSquare from))) (T.unSquare (toSquare to))
                       else p1

                  p3 = case m of
                          PromotionMove _ _ _ -> setBit p2 (T.unSquare (toSquare to))
                          _ -> p2

              in (pockets', p3)

        newCR = case m of
                  DropMove _ _ -> castlingRights ag
                  _ -> updateCastlingRights (castlingRights ag) from to

        movedPiece = Base.pieceAt internalB' (toSquare to)
        isPawn = case movedPiece of
                   Just p -> T.pieceType p == T.Pawn
                   _ -> False
        newEP = case m of
                  StandardMove f t -> if isPawn && isDoublePush f t then Just (getFile f) else Nothing
                  _ -> Nothing

        isCapture = case m of
                      StandardMove _ t -> Base.pieceAt internalB (toSquare t) /= Nothing
                      PromotionMove _ t _ -> Base.pieceAt internalB (toSquare t) /= Nothing
                      EnPassantMove _ _ -> True
                      _ -> False

        resetClock = isPawn || isCapture
        newHMC = if resetClock then 0 else halfMoveClock ag + 1
        newFMN = fullMoveNumber ag + (if c == Black then 1 else 0)

        baseBoard = internalB'
        nextTurnGS = GS.GameState
          { GS.turn = toColor oppC
          , GS.castlingRights = toCastlingRights newCR
          , GS.epSquare = case newEP of
                            Nothing -> Nothing
                            Just f -> Just (toSquare (Square f (epRank oppC)))
          , GS.halfmoveClock = newHMC
          , GS.fullmoveNumber = newFMN
          }

        newState = CrazyhouseState wPocket' bPocket' promoted'

        genDrops :: forall col. KnownColor col => PieceType -> Int -> [Move col]
        genDrops pt count =
          if count <= 0 then []
          else
             let validSquares = if pt == Pawn
                                then filter (\(Square _ r) -> r /= Rank1 && r /= Rank8) emptySquares
                                else emptySquares
             in map (DropMove pt) validSquares

        dropMoves :: [Move (Opposite c)]
        dropMoves = concat
           [ genDrops @(Opposite c) Pawn (pocketPawns (if oppC == White then wPocket' else bPocket'))
           , genDrops @(Opposite c) Knight (pocketKnights (if oppC == White then wPocket' else bPocket'))
           , genDrops @(Opposite c) Bishop (pocketBishops (if oppC == White then wPocket' else bPocket'))
           , genDrops @(Opposite c) Rook (pocketRooks (if oppC == White then wPocket' else bPocket'))
           , genDrops @(Opposite c) Queen (pocketQueens (if oppC == White then wPocket' else bPocket'))
           ]

        emptySquares = [ Square f r | f <- [FileA .. FileH], r <- [Rank1 .. Rank8], Base.pieceAt internalB' (toSquare (Square f r)) == Nothing ]

        isSafeDrop :: Move (Opposite c) -> Bool
        isSafeDrop m =
           let nextBase = applyMoveBase @(Opposite c) m internalB'
           in not (Val.isCheck nextBase nextTurnGS)

        canDrop = not (null (filter isSafeDrop dropMoves))

        isChecked = Val.isCheck baseBoard nextTurnGS
        hasMoves = Val.hasLegalMoves baseBoard nextTurnGS || canDrop

    in case (isChecked, hasMoves) of
         (True, False) -> Checkmate (Winner c)
         (False, False) -> Stalemate
         (True, True) -> Continue (ActiveGame internalB' newCR newEP newHMC newFMN newState SChecked :: ActiveGame 'Crazyhouse (Opposite c) 'Checked)
         (False, True) -> Continue (ActiveGame internalB' newCR newEP newHMC newFMN newState SSafe :: ActiveGame 'Crazyhouse (Opposite c) 'Safe)

getAdjacentSquares :: Square -> [Square]
getAdjacentSquares (Square f r) =
  let fIdx = fromEnum f
      rIdx = fromEnum r
      adjs = [ (f', r') | f' <- [fIdx-1 .. fIdx+1], r' <- [rIdx-1 .. rIdx+1], (f', r') /= (fIdx, rIdx) ]
      valid (fx, rx) = fx >= 0 && fx <= 7 && rx >= 0 && rx <= 7
  in [ Square (toEnum fx) (toEnum rx) | (fx, rx) <- adjs, valid (fx, rx) ]
