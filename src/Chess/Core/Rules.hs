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
import Data.Bits (setBit, (.&.), (.|.), testBit, countTrailingZeros, clearBit)
import qualified Data.Map as Map
import qualified Data.Set as Set

-- | Create the initial game state for Standard chess.
initialGame :: Game 'Standard 'Active
initialGame =
  let b = initialBoard
      ag = ActiveGame
           { internalBoard = toBaseBoard b
           , castlingRights = CastlingRights True True True True
           , enPassantTarget = Nothing
           , halfMoveClock = 0
           , fullMoveNumber = 1
           , variantState = ()
           } :: ActiveGame 'Standard 'White 'Safe
  in InProgressGame ag

-- | Create a game from FEN string (Standard variant).
gameFromFEN :: String -> Maybe (Game 'Standard 'Active)
gameFromFEN = genericGameFromFEN

-- | Create a game from FEN string (Atomic variant).
atomicGameFromFEN :: String -> Maybe (Game 'Atomic 'Active)
atomicGameFromFEN = genericGameFromFEN

-- | Create a game from FEN string (King of the Hill variant).
kingOfTheHillGameFromFEN :: String -> Maybe (Game 'KingOfTheHill 'Active)
kingOfTheHillGameFromFEN = genericGameFromFEN

-- | Create a game from FEN string (Racing Kings variant).
racingKingsGameFromFEN :: String -> Maybe (Game 'RacingKings 'Active)
racingKingsGameFromFEN = genericGameFromFEN

-- | Create a game from FEN string (ThreeCheck variant).
threeCheckGameFromFEN :: String -> Maybe (Game 'ThreeCheck 'Active)
threeCheckGameFromFEN s = do
  (baseBoard, gs, checks) <- CoreFen.parseThreeCheckFen s
  board <- fromBaseBoard baseBoard

  let c = case GS.turn gs of
            T.White -> White
            T.Black -> Black

      cr = CastlingRights
           { whiteKingSide = testBit (GS.castlingRights gs) (countTrailingZeros BB.BB_H1)
           , whiteQueenSide = testBit (GS.castlingRights gs) (countTrailingZeros BB.BB_A1)
           , blackKingSide = testBit (GS.castlingRights gs) (countTrailingZeros BB.BB_H8)
           , blackQueenSide = testBit (GS.castlingRights gs) (countTrailingZeros BB.BB_A8)
           }

      ep = case GS.epSquare gs of
             Nothing -> Nothing
             Just sq -> Just (getFile (fromBaseSquare sq))

      hmc = GS.halfmoveClock gs
      fmn = GS.fullmoveNumber gs

      -- Check if current player is in check
      checked = Val.isCheck baseBoard gs

      hasMovesFor :: forall col. (KnownColor col, KnownColor (Opposite col)) => Bool
      hasMovesFor = not $ null $ generateMoves (ActiveGame board baseBoard cr ep hmc fmn checks :: ActiveGame 'ThreeCheck col 'Safe)

      hasMoves = case c of
        White -> hasMovesFor @'White
        Black -> hasMovesFor @'Black

  if hasMoves
    then case c of
      White -> if checked
               then return $ InProgressGame (ActiveGame board baseBoard cr ep hmc fmn checks :: ActiveGame 'ThreeCheck 'White 'Checked)
               else return $ InProgressGame (ActiveGame board baseBoard cr ep hmc fmn checks :: ActiveGame 'ThreeCheck 'White 'Safe)
      Black -> if checked
               then return $ InProgressGame (ActiveGame board baseBoard cr ep hmc fmn checks :: ActiveGame 'ThreeCheck 'Black 'Checked)
               else return $ InProgressGame (ActiveGame board baseBoard cr ep hmc fmn checks :: ActiveGame 'ThreeCheck 'Black 'Safe)
    else Nothing

-- | Generic helper for variants with unit state
genericGameFromFEN :: forall v. (VariantState v ~ (), ChessVariant v) => String -> Maybe (Game v 'Active)
genericGameFromFEN s = do
  (baseBoard, gs) <- Fen.parseFen s
  board <- fromBaseBoard baseBoard

  let c = case GS.turn gs of
            T.White -> White
            T.Black -> Black

      cr = CastlingRights
           { whiteKingSide = testBit (GS.castlingRights gs) (countTrailingZeros BB.BB_H1)
           , whiteQueenSide = testBit (GS.castlingRights gs) (countTrailingZeros BB.BB_A1)
           , blackKingSide = testBit (GS.castlingRights gs) (countTrailingZeros BB.BB_H8)
           , blackQueenSide = testBit (GS.castlingRights gs) (countTrailingZeros BB.BB_A8)
           }

      ep = case GS.epSquare gs of
             Nothing -> Nothing
             Just sq -> Just (getFile (fromBaseSquare sq))

      hmc = GS.halfmoveClock gs
      fmn = GS.fullmoveNumber gs

      -- Check if current player is in check (using standard logic for now)
      checked = Val.isCheck baseBoard gs

      hasMovesFor :: forall col. (KnownColor col, KnownColor (Opposite col)) => Bool
      hasMovesFor = not $ null $ generateMoves (ActiveGame board baseBoard cr ep hmc fmn () :: ActiveGame v col 'Safe)

      hasMoves = case c of
        White -> hasMovesFor @'White
        Black -> hasMovesFor @'Black

  if hasMoves
    then case c of
      White -> if checked
               then return $ InProgressGame (ActiveGame baseBoard cr ep hmc fmn () :: ActiveGame 'Standard 'White 'Checked)
               else return $ InProgressGame (ActiveGame baseBoard cr ep hmc fmn () :: ActiveGame 'Standard 'White 'Safe)
      Black -> if checked
               then return $ InProgressGame (ActiveGame baseBoard cr ep hmc fmn () :: ActiveGame 'Standard 'Black 'Checked)
               else return $ InProgressGame (ActiveGame baseBoard cr ep hmc fmn () :: ActiveGame 'Standard 'Black 'Safe)
    else
      -- If no moves, game is finished. But this function returns Game 'Active.
      -- So we return Nothing? Or should we allow constructing FinishedGame?
      -- The type signature says Maybe (Game 'Standard 'Active).
      -- So we MUST return Nothing if the game is finished.
      Nothing

-- | Create a game from FEN string (Crazyhouse variant).
crazyhouseGameFromFEN :: String -> Maybe (Game 'Crazyhouse 'Active)
crazyhouseGameFromFEN s = do
  (baseBoard, gs, extra) <- Fen.parseFenRest s
  _ <- fromBaseBoard baseBoard

  let c = case GS.turn gs of
            T.White -> White
            T.Black -> Black

      cr = CastlingRights
           { whiteKingSide = testBit (GS.castlingRights gs) (countTrailingZeros BB.BB_H1)
           , whiteQueenSide = testBit (GS.castlingRights gs) (countTrailingZeros BB.BB_A1)
           , blackKingSide = testBit (GS.castlingRights gs) (countTrailingZeros BB.BB_H8)
           , blackQueenSide = testBit (GS.castlingRights gs) (countTrailingZeros BB.BB_A8)
           }

      ep = case GS.epSquare gs of
             Nothing -> Nothing
             Just sq -> Just (getFile (fromBaseSquare sq))

      hmc = GS.halfmoveClock gs
      fmn = GS.fullmoveNumber gs

      -- Parse pockets
      pocketStr = case filter (\x -> not (null x) && head x == '[') extra of
                    (p:_) -> p
                    [] -> "[]"

      (wPocket, bPocket) = foldr dist (Map.empty, Map.empty) (filter (`elem` "PNBRQKpnbrqk") pocketStr)
        where
          dist char (wm, bm) =
             case T.fromSymbol char of
                Just (T.Piece T.White pt) -> (Map.insertWith (+) (fromPieceType pt) 1 wm, bm)
                Just (T.Piece T.Black pt) -> (wm, Map.insertWith (+) (fromPieceType pt) 1 bm)
                Nothing -> (wm, bm)

      vs = (wPocket, bPocket, Set.empty)

      checked = Val.isCheck baseBoard gs

      -- Helper to generate drops
      generateDrops :: forall c. KnownColor c => [Move c]
      generateDrops =
           let col = colorVal @c
               pocket = if col == White then wPocket else bPocket
               emptySqs = [ Square f r | f <- [FileA .. FileH], r <- [Rank1 .. Rank8], Base.pieceAt baseBoard (toSquare (Square f r)) == Nothing ]
               gen (pt, cnt) | cnt <= 0 = []
                             | otherwise =
                                 let valid = if pt == Pawn then filter (\(Square _ r) -> r /= Rank1 && r /= Rank8) emptySqs else emptySqs
                                 in map (DropMove pt) valid
               drops = concatMap gen (Map.toList pocket)
               safe m = not (Val.isCheck (applyMoveBase m baseBoard) gs)
           in filter safe drops

      hasMoves :: forall c. KnownColor c => Bool
      hasMoves = Val.hasLegalMoves baseBoard gs || not (null (generateDrops @c))

  case c of
      White -> if hasMoves @'White
               then if checked
                    then return $ InProgressGame (ActiveGame baseBoard cr ep hmc fmn vs :: ActiveGame 'Crazyhouse 'White 'Checked)
                    else return $ InProgressGame (ActiveGame baseBoard cr ep hmc fmn vs :: ActiveGame 'Crazyhouse 'White 'Safe)
               else Nothing
      Black -> if hasMoves @'Black
               then if checked
                    then return $ InProgressGame (ActiveGame baseBoard cr ep hmc fmn vs :: ActiveGame 'Crazyhouse 'Black 'Checked)
                    else return $ InProgressGame (ActiveGame baseBoard cr ep hmc fmn vs :: ActiveGame 'Crazyhouse 'Black 'Safe)
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

-- | Helper to get the piece type that moved.
-- Assumes the move is valid and the board is in the state *before* the move.
getMovedPieceType :: forall c. KnownColor c => Move c -> Base.Board -> PieceType
getMovedPieceType m b = case m of
  DropMove p _ -> p
  PromotionMove _ _ p -> p
  StandardMove f _ -> fromPieceType (Base.findPieceType b (toColor (colorVal @c)) (toSquare f))
  CastlingMove _ _ -> King
  EnPassantMove _ _ -> Pawn

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

toCoreMove :: Base.Board -> T.Move -> Move c
toCoreMove b (T.Move f t promo) =
  let fromSq = fromSquare f
      toSq = fromSquare t
      -- Determine piece type at source
      isWhite = testBit (Base.occupiedWhite b) (T.unSquare f)
      tC = if isWhite then T.White else T.Black
      pt = fromPieceType (Base.findPieceType b tC f)
  in case promo of
       Just ppt ->
          PromotionMove fromSq toSq (fromPieceType ppt)
       Nothing ->
          if isCastlingMove pt fromSq toSq
          then CastlingMove fromSq toSq
          else if isEnPassantMove pt fromSq toSq b
               then EnPassantMove fromSq toSq
               else StandardMove fromSq toSq
toCoreMove _ (T.DropMove pt t) = DropMove (fromPieceType pt) (fromSquare t)
toCoreMove _ T.NullMove = error "Null move generated"

isCastlingMove :: PieceType -> Square -> Square -> Bool
isCastlingMove pt from to =
  pt == King && abs (fromEnum (getFile from) - fromEnum (getFile to)) == 2

isEnPassantMove :: PieceType -> Square -> Square -> Base.Board -> Bool
isEnPassantMove pt from to b =
  pt == Pawn &&
  getFile from /= getFile to &&
  case Base.pieceAt b (toSquare to) of
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


-- Apply Move Helper (Base Board update)
applyMoveBase :: forall c. KnownColor c => Move c -> Base.Board -> Base.Board
applyMoveBase m b =
    case m of
       StandardMove f t ->
          let p = Base.pieceAt b (toSquare f)
          in case p of
             Nothing -> b -- Should not happen for legal moves
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
       Castling960Move f t kDst rDst ->
          let b1 = Base.removePieceAt b (toSquare f)
              b2 = Base.removePieceAt b1 (toSquare t)
              kPiece = T.Piece (toColor (colorVal @c)) T.King
              rPiece = T.Piece (toColor (colorVal @c)) T.Rook
              b3 = Base.putPiece b2 (toSquare kDst) kPiece
              b4 = Base.putPiece b3 (toSquare rDst) rPiece
          in b4

-- Apply Move
applyMove :: forall v c s. (KnownColor c, KnownColor (Opposite c), ChessVariant v) => Move c -> ActiveGame v c s -> MoveResult v (Opposite c)
applyMove = executeMove

instance ChessVariant 'Standard where
  generateMoves (ag :: ActiveGame 'Standard c s) =
    let baseBoard = internalBoard ag
        gs = toGameState ag
        baseMoves = MG.legalMoves baseBoard gs
    in map (toCoreMove baseBoard) baseMoves

  executeMove (m :: Move c) (ag :: ActiveGame 'Standard c s) =
    let
        -- 1. Update Board
        c = colorVal @c
        -- Update Base Board
        internalB = internalBoard ag
        internalB' = applyMoveBase m internalB

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
        movedType = getMovedPieceType m internalB
        isPawn = movedType == Pawn

        newEP = case m of
                  StandardMove f t -> if isPawn && isDoublePush f t then Just (getFile f) else Nothing
                  _ -> Nothing

        -- Update Clocks
        newHMC = halfMoveClock ag + 1
        newFMN = fullMoveNumber ag + (if c == Black then 1 else 0)

        -- 3. Validation
        baseBoard = internalB'

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
                                    { internalBoard = internalB'
                                    , castlingRights = newCR
                                    , enPassantTarget = newEP
                                    , halfMoveClock = newHMC
                                    , fullMoveNumber = newFMN
                                    , variantState = ()
                                    } :: ActiveGame 'Standard (Opposite c) 'Checked)
         (False, True) -> Continue (ActiveGame
                                    { internalBoard = internalB'
                                    , castlingRights = newCR
                                    , enPassantTarget = newEP
                                    , halfMoveClock = newHMC
                                    , fullMoveNumber = newFMN
                                    , variantState = ()
                                    } :: ActiveGame 'Standard (Opposite c) 'Safe)

instance ChessVariant 'ThreeCheck where
  generateMoves (ag :: ActiveGame 'ThreeCheck c s) =
    let baseBoard = internalBoard ag
        gs = toGameState ag
        baseMoves = MG.legalMoves baseBoard gs
    in map (toCoreMove baseBoard) baseMoves

  executeMove (m :: Move c) (ag :: ActiveGame 'ThreeCheck c s) =
    let
        -- 1. Update Board
        c = colorVal @c
        oppC = colorVal @(Opposite c)

        -- Update Base Board
        internalB = internalBoard ag
        internalB' = applyMoveBase m internalB

        (from, to) = case m of
                       StandardMove f t -> (f, t)
                       PromotionMove f t _ -> (f, t)
                       CastlingMove f t -> (f, t)
                       EnPassantMove f t -> (f, t)

        -- 2. Update Game State
        newCR = updateCastlingRights (castlingRights ag) from to

        movedType = getMovedPieceType m internalB
        isPawn = movedType == Pawn

        newEP = case m of
                  StandardMove f t -> if isPawn && isDoublePush f t then Just (getFile f) else Nothing
                  _ -> Nothing

        newHMC = halfMoveClock ag + 1
        newFMN = fullMoveNumber ag + (if c == Black then 1 else 0)

        baseBoard = internalB'

        -- Check if this move GIVES check to the opponent
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

        -- Update Check Counters
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
         (True, True) -> Continue (ActiveGame
                                    { internalBoard = internalB'
                                    , castlingRights = newCR
                                    , enPassantTarget = newEP
                                    , halfMoveClock = newHMC
                                    , fullMoveNumber = newFMN
                                    , variantState = newVariantState
                                    } :: ActiveGame 'ThreeCheck (Opposite c) 'Checked)
         (False, True) -> Continue (ActiveGame
                                    { internalBoard = internalB'
                                    , castlingRights = newCR
                                    , enPassantTarget = newEP
                                    , halfMoveClock = newHMC
                                    , fullMoveNumber = newFMN
                                    , variantState = newVariantState
                                    } :: ActiveGame 'ThreeCheck (Opposite c) 'Safe)

instance ChessVariant 'Atomic where
  generateMoves (ag :: ActiveGame 'Atomic c s) =
    let baseBoard = internalBoard ag
        gs = toGameState ag
        c = colorVal @c

        pseudos = MG.pseudoLegalMoves baseBoard gs

        -- Filter King Captures: King cannot capture
        isKingCapture :: T.Move -> Bool
        isKingCapture (T.Move f t _) =
           let p = Base.pieceAt baseBoard f
           in fmap T.pieceType p == Just T.King && Base.pieceAt baseBoard t /= Nothing
        isKingCapture T.NullMove = False

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
        isSelfExplosion T.NullMove = False

        chebyshevDist :: T.Square -> T.Square -> Int
        chebyshevDist (T.Square i1) (T.Square i2) =
           let r1 = i1 `div` 8
               c1 = i1 `mod` 8
               r2 = i2 `div` 8
               c2 = i2 `mod` 8
           in max (abs (r1 - r2)) (abs (c1 - c2))

        atomicMoves = filter (\(MG.GenMove m _ _) -> not (isKingCapture m) && not (isSelfExplosion m)) pseudos

        -- Apply standard check filtering (approximation)
        validMoves = filter (MG.isLegal baseBoard gs) atomicMoves

    in map (toCoreMove baseBoard . (\(MG.GenMove m _ _) -> m)) validMoves

  executeMove (m :: Move c) (ag :: ActiveGame 'Atomic c s) =
    let c = colorVal @c
        oppC = colorVal @(Opposite c)

        -- 1. Apply Move Basic
        bBasicBase = applyMoveBase m (internalBoard ag)

        (from, to) = case m of
                       StandardMove f t -> (f, t)
                       PromotionMove f t _ -> (f, t)
                       CastlingMove f t -> (f, t)
                       EnPassantMove f t -> (f, t)

        -- Check if capture
        isCapture = case m of
                      StandardMove _ t -> Base.pieceAt (internalBoard ag) (toSquare t) /= Nothing
                      PromotionMove _ t _ -> Base.pieceAt (internalBoard ag) (toSquare t) /= Nothing
                      EnPassantMove _ _ -> True
                      _ -> False

        -- Explosion Logic
        (bFinalBase, enemyKingExploded) = if isCapture
          then
            let center = toSquare to
                -- Capturing piece explodes (remove at center)
                b1 = Base.removePieceAt bBasicBase center

                -- Surrounding Squares
                surrounds = getAdjacentSquares to

                -- Explode surrounding
                explode sq (board, kingDead) =
                  let sqBase = toSquare sq
                  in case Base.pieceAt board sqBase of
                       Nothing -> (board, kingDead)
                       Just (T.Piece _ pt) ->
                          if pt == T.Pawn then (board, kingDead) -- Pawns survive
                          else if pt == T.King then (board, True) -- King dies
                          else (Base.removePieceAt board sqBase, kingDead)

                (b2, kDead) = foldr explode (b1, False) surrounds
            in (b2, kDead)
          else (bBasicBase, False)

        -- State Updates
        newCR = updateCastlingRights (castlingRights ag) from to

        -- EP
        movedType = getMovedPieceType m (internalBoard ag) -- Look at board before move/explosion to determine moved piece
        isPawn = movedType == Pawn

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

        baseBoard = bFinalBase
        isChecked = Val.isCheck baseBoard nextTurnGS
        hasMoves = Val.hasLegalMoves baseBoard nextTurnGS

    in if enemyKingExploded
       then Checkmate (Winner c)
       else case (isChecked, hasMoves) of
         (True, False) -> Checkmate (Winner c)
         (False, False) -> Stalemate
         (True, True) -> Continue (ActiveGame
                                    { internalBoard = baseBoard
                                    , castlingRights = newCR
                                    , enPassantTarget = newEP
                                    , halfMoveClock = newHMC
                                    , fullMoveNumber = newFMN
                                    , variantState = ()
                                    } :: ActiveGame 'Atomic (Opposite c) 'Checked)
         (False, True) -> Continue (ActiveGame
                                    { internalBoard = baseBoard
                                    , castlingRights = newCR
                                    , enPassantTarget = newEP
                                    , halfMoveClock = newHMC
                                    , fullMoveNumber = newFMN
                                    , variantState = ()
                                    } :: ActiveGame 'Atomic (Opposite c) 'Safe)

instance ChessVariant 'KingOfTheHill where
  generateMoves (ag :: ActiveGame 'KingOfTheHill c s) =
    let baseBoard = internalBoard ag
        gs = toGameState ag
        baseMoves = MG.legalMoves baseBoard gs
    in map (toCoreMove baseBoard) baseMoves

  executeMove (m :: Move c) (ag :: ActiveGame 'KingOfTheHill c s) =
    let
        -- 1. Update Board
        c = colorVal @c
        -- Update Base Board
        internalB = internalBoard ag
        internalB' = applyMoveBase m internalB

        (from, to) = case m of
                       StandardMove f t -> (f, t)
                       PromotionMove f t _ -> (f, t)
                       CastlingMove f t -> (f, t)
                       EnPassantMove f t -> (f, t)

        -- 2. Update Game State
        newCR = updateCastlingRights (castlingRights ag) from to

        movedType = getMovedPieceType m internalB
        isPawn = movedType == Pawn
        isKing = movedType == King

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

        -- KOTH Condition: King in center (e4, d4, e5, d5)
        kingInCenter = isKing && to `elem` centerSquares
        centerSquares = [ Square FileE Rank4, Square FileD Rank4
                        , Square FileE Rank5, Square FileD Rank5 ]

    in if kingInCenter
       then Checkmate (Winner c)
       else case (isChecked, hasMoves) of
         (True, False) -> Checkmate (Winner c)
         (False, False) -> Stalemate
         (True, True) -> Continue (ActiveGame
                                    { internalBoard = internalB'
                                    , castlingRights = newCR
                                    , enPassantTarget = newEP
                                    , halfMoveClock = newHMC
                                    , fullMoveNumber = newFMN
                                    , variantState = ()
                                    } :: ActiveGame 'KingOfTheHill (Opposite c) 'Checked)
         (False, True) -> Continue (ActiveGame
                                    { internalBoard = internalB'
                                    , castlingRights = newCR
                                    , enPassantTarget = newEP
                                    , halfMoveClock = newHMC
                                    , fullMoveNumber = newFMN
                                    , variantState = ()
                                    } :: ActiveGame 'KingOfTheHill (Opposite c) 'Safe)

instance ChessVariant 'RacingKings where
  generateMoves (ag :: ActiveGame 'RacingKings c s) =
    let baseBoard = internalBoard ag
        gs = toGameState ag

        -- Get Standard moves (handles own check safety)
        baseMoves = MG.legalMoves baseBoard gs
        coreMoves = map (toCoreMove baseBoard) baseMoves

        c = colorVal @c
        oppC = opposite c

        -- Filter moves that give check to opponent
        noGiveCheck m =
            let baseNext = applyMoveBase m baseBoard
            in not (Val.isCheck baseNext (dummyGameState oppC))
          where
            dummyGameState col = GS.initialGameState { GS.turn = toColor col }

    in filter noGiveCheck coreMoves

  executeMove (m :: Move c) (ag :: ActiveGame 'RacingKings c s) =
    let c = colorVal @c

        -- Update Base Board
        internalB = internalBoard ag
        internalB' = applyMoveBase m internalB

        (from, to) = case m of
                       StandardMove f t -> (f, t)
                       PromotionMove f t _ -> (f, t)
                       CastlingMove f t -> (f, t)
                       EnPassantMove f t -> (f, t)

        newCR = updateCastlingRights (castlingRights ag) from to

        movedType = getMovedPieceType m internalB
        isPawn = movedType == Pawn

        newEP = case m of
                  StandardMove f t -> if isPawn && isDoublePush f t then Just (getFile f) else Nothing
                  _ -> Nothing

        newHMC = halfMoveClock ag + 1
        newFMN = fullMoveNumber ag + (if c == Black then 1 else 0)

        -- To properly check if opponent has moves, I need to call `generateMoves` for the next state.
        -- But `generateMoves` requires `ActiveGame`. I can construct one.
        nextGameCandidate :: ActiveGame 'RacingKings (Opposite c) 'Safe
        nextGameCandidate = ActiveGame
                                    { internalBoard = internalB'
                                    , castlingRights = newCR
                                    , enPassantTarget = newEP
                                    , halfMoveClock = newHMC
                                    , fullMoveNumber = newFMN
                                    , variantState = ()
                                    }

        -- Check if next player has moves using OUR generateMoves
        nextMoves = generateMoves nextGameCandidate
        realHasMoves = not (null nextMoves)

        -- Win Condition
        -- White King Rank
        wInGoal = (Base.whiteKings internalB') .&. BB.bbRank8 /= 0
        bInGoal = (Base.blackKings internalB') .&. BB.bbRank8 /= 0

        result =
             if c == White
             then if wInGoal
                  then if realHasMoves
                       then Continue nextGameCandidate
                       else Checkmate (Winner White) -- Black has no moves, cannot draw
                  else
                       if realHasMoves then Continue nextGameCandidate else Stalemate
             else -- c == Black
                  if bInGoal && wInGoal then Checkmate Draw else -- Both in goal -> Draw
                  if wInGoal then Checkmate (Winner White) else -- White in goal, Black failed -> White wins
                  if bInGoal then Checkmate (Winner Black) else -- Black in goal, White not -> Black wins (unexpected)
                  if realHasMoves then Continue nextGameCandidate else Stalemate

    in result

instance ChessVariant 'Crazyhouse where
  generateMoves (ag :: ActiveGame 'Crazyhouse c s) =
    let baseBoard = internalBoard ag
        gs = toGameState ag
        c = colorVal @c

        baseMoves = MG.legalMoves baseBoard gs
        standardMoves = map (toCoreMove baseBoard) baseMoves

        (wPocket, bPocket, _) = variantState ag
        pocket = if c == White then wPocket else bPocket

        emptySquares = [ Square f r | f <- [FileA .. FileH], r <- [Rank1 .. Rank8], Base.pieceAt baseBoard (toSquare (Square f r)) == Nothing ]

        dropMoves = concatMap genDrops (Map.toList pocket)

        genDrops (pt, count)
          | count <= 0 = []
          | otherwise =
              let validSquares = if pt == Pawn
                                 then filter (\(Square _ r) -> r /= Rank1 && r /= Rank8) emptySquares
                                 else emptySquares
              in map (DropMove pt) validSquares

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

        (wPocket, bPocket, promotedSet) = variantState ag

        (from, to) = case m of
                       StandardMove f t -> (f, t)
                       PromotionMove f t _ -> (f, t)
                       CastlingMove f t -> (f, t)
                       EnPassantMove f t -> (f, t)
                       DropMove _ t -> (t, t) -- Dummy from

        -- Handle Captures and Drops
        ((wPocket', bPocket'), promotedSet') = case m of
           DropMove p _ ->
              let pockets = if c == White
                            then (Map.adjust (\x -> x - 1) p wPocket, bPocket)
                            else (wPocket, Map.adjust (\x -> x - 1) p bPocket)
              in (pockets, promotedSet)
           _ ->
              -- Check capture
              let (capturedSquare, capturedType) = case m of
                     EnPassantMove f t -> (Just (getEpCapturedSquare f t), Just Pawn)
                     _ -> let tSq = toSquare to
                              p = Base.pieceAt internalB tSq
                          in case p of
                               Just (T.Piece _ pt) -> (Just to, Just (fromPieceType pt))
                               Nothing -> (Nothing, Nothing)

                  -- Update Pockets
                  pockets' = case capturedType of
                     Just pt ->
                        let isPromoted = case capturedSquare of
                                           Just sq -> Set.member sq promotedSet
                                           Nothing -> False
                            addToPocket = if isPromoted then Pawn else pt
                            (wm, bm) = (wPocket, bPocket)
                        in if c == White
                           then (Map.insertWith (+) addToPocket 1 wm, bm)
                           else (wm, Map.insertWith (+) addToPocket 1 bm)
                     Nothing -> (wPocket, bPocket)

                  -- Update Promoted Set
                  ps1 = case capturedSquare of
                          Just sq -> Set.delete sq promotedSet
                          Nothing -> promotedSet

                  isMovingPromoted = Set.member from ps1
                  ps2 = if isMovingPromoted then Set.insert to (Set.delete from ps1) else ps1

                  ps3 = case m of
                          PromotionMove _ t _ -> Set.insert to ps2
                          _ -> ps2

              in (pockets', ps3)

        -- State Updates
        newCR = case m of
                  DropMove _ _ -> castlingRights ag
                  _ -> updateCastlingRights (castlingRights ag) from to

        movedType = getMovedPieceType m internalB
        isPawn = movedType == Pawn

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

        newState = (wPocket', bPocket', promotedSet')

        generateDrops :: forall col. KnownColor col => Base.Board -> GS.GameState -> (Map.Map PieceType Int, Map.Map PieceType Int, Set.Set Square) -> [Move col]
        generateDrops board gs (wm, bm, _) =
           let cVal = colorVal @col
               pocket = if cVal == White then wm else bm
               emptySqs = [ Square f r | f <- [FileA .. FileH], r <- [Rank1 .. Rank8], Base.pieceAt board (toSquare (Square f r)) == Nothing ]
               gen (pt, cnt) | cnt <= 0 = []
                             | otherwise =
                                 let valid = if pt == Pawn then filter (\(Square _ r) -> r /= Rank1 && r /= Rank8) emptySqs else emptySqs
                                 in map (DropMove pt) valid
               drops = concatMap gen (Map.toList pocket)
               safe m = not (Val.isCheck (applyMoveBase m board) gs)
           in filter safe drops

        isChecked = Val.isCheck baseBoard nextTurnGS
        hasMoves = Val.hasLegalMoves baseBoard nextTurnGS || not (null (generateDrops @(Opposite c) baseBoard nextTurnGS newState))

    in case (isChecked, hasMoves) of
         (True, False) -> Checkmate (Winner c)
         (False, False) -> Stalemate
         (True, True) -> Continue (ActiveGame
                                    { internalBoard = internalB'
                                    , castlingRights = newCR
                                    , enPassantTarget = newEP
                                    , halfMoveClock = newHMC
                                    , fullMoveNumber = newFMN
                                    , variantState = newState
                                    } :: ActiveGame 'Crazyhouse (Opposite c) 'Checked)
         (False, True) -> Continue (ActiveGame
                                    { internalBoard = internalB'
                                    , castlingRights = newCR
                                    , enPassantTarget = newEP
                                    , halfMoveClock = newHMC
                                    , fullMoveNumber = newFMN
                                    , variantState = newState
                                    } :: ActiveGame 'Crazyhouse (Opposite c) 'Safe)

getAdjacentSquares :: Square -> [Square]
getAdjacentSquares (Square f r) =
  let fIdx = fromEnum f
      rIdx = fromEnum r
      adjs = [ (f', r') | f' <- [fIdx-1 .. fIdx+1], r' <- [rIdx-1 .. rIdx+1], (f', r') /= (fIdx, rIdx) ]
      valid (fx, rx) = fx >= 0 && fx <= 7 && rx >= 0 && rx <= 7
  in [ Square (toEnum fx) (toEnum rx) | (fx, rx) <- adjs, valid (fx, rx) ]

-- | Create a game from FEN string (FischerRandom variant).
fischerRandomGameFromFEN :: String -> Maybe (Game 'FischerRandom 'Active)
fischerRandomGameFromFEN s = do
  (baseBoard, gs) <- Fen.parseFen s
  board <- fromBaseBoard baseBoard

  let c = case GS.turn gs of
            T.White -> White
            T.Black -> Black

      -- Find rooks based on CastlingRights bitboard
      findRooks col =
          let king = if col == White then whiteKing board else blackKing board
              rank = if col == White then Rank1 else Rank8
              kFile = getFile king
              cr = GS.castlingRights gs
              -- Bits on the rank
              bits = BB.mapBitboard fromSquare cr
              onRank (Square _ r) = r == rank
              relevant = filter onRank bits

              -- Split by file relative to King
              (qs, ks) = span (\(Square f _) -> f < kFile) relevant
              -- ks includes file > kFile? No, span puts matching prefix in fst.
              -- relevant is sorted by File (A->H).
              -- So qs are files < kFile. ks are files >= kFile.
              -- But Rook cannot be on King square for castling (must be distinct).
              ks' = filter (\(Square f _) -> f > kFile) ks

              kRook = if null ks' then Nothing else Just (last ks') -- Outermost right
              qRook = if null qs then Nothing else Just (head qs) -- Outermost left
          in (kRook, qRook)

      (wk, wq) = findRooks White
      (bk, bq) = findRooks Black

      vs = (wk, wq, bk, bq)

      cr = CastlingRights
           { whiteKingSide = wk /= Nothing
           , whiteQueenSide = wq /= Nothing
           , blackKingSide = bk /= Nothing
           , blackQueenSide = bq /= Nothing
           }

      ep = case GS.epSquare gs of
             Nothing -> Nothing
             Just sq -> Just (getFile (fromBaseSquare sq))

      hmc = GS.halfmoveClock gs
      fmn = GS.fullmoveNumber gs

      -- Check if current player is in check
      checked = Val.isCheck baseBoard gs

      -- Create ActiveGame to check moves
      -- dummy Status

      hasMoves = case c of
        White -> not (null (generateMoves (ActiveGame board baseBoard cr ep hmc fmn vs :: ActiveGame 'FischerRandom 'White 'Safe)))
        Black -> not (null (generateMoves (ActiveGame board baseBoard cr ep hmc fmn vs :: ActiveGame 'FischerRandom 'Black 'Safe)))

  if hasMoves
    then case c of
      White -> if checked
               then return $ InProgressGame (ActiveGame board baseBoard cr ep hmc fmn vs :: ActiveGame 'FischerRandom 'White 'Checked)
               else return $ InProgressGame (ActiveGame board baseBoard cr ep hmc fmn vs :: ActiveGame 'FischerRandom 'White 'Safe)
      Black -> if checked
               then return $ InProgressGame (ActiveGame board baseBoard cr ep hmc fmn vs :: ActiveGame 'FischerRandom 'Black 'Checked)
               else return $ InProgressGame (ActiveGame board baseBoard cr ep hmc fmn vs :: ActiveGame 'FischerRandom 'Black 'Safe)
    else Nothing

instance ChessVariant 'FischerRandom where
  generateMoves (ag :: ActiveGame 'FischerRandom c s) =
    let b = gameBoard ag
        baseBoard = internalBoard ag
        gs = toGameState ag
        c = colorVal @c

        -- 1. Standard Moves (NO Castling)
        gsNoCastling = gs { GS.castlingRights = GS.noCastling }
        baseMoves = MG.legalMoves baseBoard gsNoCastling
        coreMoves = map (toCoreMove b) baseMoves

        -- 2. 960 Castling
        (wk, wq, bk, bq) = variantState ag
        (kRook, qRook) = if c == White then (wk, wq) else (bk, bq)

        kingSq = if c == White then whiteKing b else blackKing b
        rank = if c == White then Rank1 else Rank8

        tryCastle Nothing _ _ = []
        tryCastle (Just rookSq) kDst rDst =
           let
               -- Path 1: Between King and Rook (excluding King and Rook)
               path1 = BB.between (toSquare kingSq) (toSquare rookSq)

               -- Path 2: Between King and KingDest (excluding King)
               path2 = BB.between (toSquare kingSq) (toSquare kDst)

               -- Path 3: Between Rook and RookDest (excluding Rook)
               path3 = BB.between (toSquare rookSq) (toSquare rDst)

               -- Combined empty requirement
               requiredEmpty = path1 .|. path2 .|. path3

               -- Check if occupied
               occ = Base.occupiedTotal baseBoard
               -- We ignore King and Rook current positions for path checking
               occPath = occ `clearBit` (T.unSquare (toSquare kingSq)) `clearBit` (T.unSquare (toSquare rookSq))

               checkDestEmpty sq =
                   sq == kingSq || sq == rookSq || not (testBit occ (T.unSquare (toSquare sq)))

               destsEmpty = checkDestEmpty kDst && checkDestEmpty rDst

               pathClear = (requiredEmpty .&. occPath) == 0 && destsEmpty

               -- Check Safety
               notInCheck = not (Val.isCheck baseBoard gs) -- Current state

               -- King path not attacked.
               pathSquares = BB.scanForward (path2 .|. BB.bbFromSquare (toSquare kDst))
               oppC = Base.oppositeColor (toColor c)

               isAttacked sq = Base.isAttackedBy baseBoard oppC sq

               safePath = notInCheck && all (not . isAttacked . T.Square) pathSquares

           in if pathClear && safePath
              then [Castling960Move kingSq rookSq kDst rDst]
              else []

        canCastleKS = if c == White then whiteKingSide (castlingRights ag) else blackKingSide (castlingRights ag)
        canCastleQS = if c == White then whiteQueenSide (castlingRights ag) else blackQueenSide (castlingRights ag)

        ksMoves = if canCastleKS then tryCastle kRook (Square FileG rank) (Square FileF rank) else []
        qsMoves = if canCastleQS then tryCastle qRook (Square FileC rank) (Square FileD rank) else []

    in coreMoves ++ ksMoves ++ qsMoves

  executeMove (m :: Move c) (ag :: ActiveGame 'FischerRandom c s) =
    let
        c = colorVal @c
        oppC = colorVal @(Opposite c)
        b = gameBoard ag
        b' = applyMoveBoard b m
        internalB = internalBoard ag
        internalB' = applyMoveBase m internalB

        (from, to) = case m of
                       StandardMove f t -> (f, t)
                       PromotionMove f t _ -> (f, t)
                       CastlingMove f t -> (f, t)
                       EnPassantMove f t -> (f, t)
                       Castling960Move f t _ _ -> (f, t) -- f=King, t=Rook
                       _ -> (whiteKing b, whiteKing b)

        -- Castling Rights Update
        (wk, wq, bk, bq) = variantState ag
        cr = castlingRights ag

        isRookSq sq (Just r) = sq == r
        isRookSq _ Nothing = False

        wKingSq = whiteKing b
        bKingSq = blackKing b

        cr1 = if from == wKingSq then cr { whiteKingSide = False, whiteQueenSide = False }
              else if from == bKingSq then cr { blackKingSide = False, blackQueenSide = False }
              else cr

        checkRook rights sq =
           let r1 = if isRookSq sq wk then rights { whiteKingSide = False } else rights
               r2 = if isRookSq sq wq then r1 { whiteQueenSide = False } else r1
               r3 = if isRookSq sq bk then r2 { blackKingSide = False } else r2
               r4 = if isRookSq sq bq then r3 { blackQueenSide = False } else r3
           in r4

        newCR = checkRook (checkRook cr1 from) to

        movedPiece = getPieceAt (case m of Castling960Move _ _ kd _ -> kd; _ -> to) b'
        isPawn = case movedPiece of
                   Just (SomePiece WPawn) -> True
                   Just (SomePiece BPawn) -> True
                   _ -> False

        newEP = case m of
                  StandardMove f t -> if isPawn && isDoublePush f t then Just (getFile f) else Nothing
                  _ -> Nothing

        isCapture = case m of
             StandardMove _ t -> getPieceAt t b /= Nothing
             PromotionMove _ t _ -> getPieceAt t b /= Nothing
             EnPassantMove _ _ -> True
             Castling960Move _ _ _ _ -> False
             _ -> False

        resetClock = isPawn || isCapture
        newHMC = if resetClock then 0 else halfMoveClock ag + 1
        newFMN = fullMoveNumber ag + (if c == Black then 1 else 0)

        baseBoard = internalB'

        toCastlingRights960 rights =
             (if whiteKingSide rights then maybe 0 (BB.bbFromSquare . toSquare) wk else 0) .|.
             (if whiteQueenSide rights then maybe 0 (BB.bbFromSquare . toSquare) wq else 0) .|.
             (if blackKingSide rights then maybe 0 (BB.bbFromSquare . toSquare) bk else 0) .|.
             (if blackQueenSide rights then maybe 0 (BB.bbFromSquare . toSquare) bq else 0)

        nextTurnGS960 = GS.GameState
          { GS.turn = toColor (colorVal @(Opposite c))
          , GS.castlingRights = toCastlingRights960 newCR
          , GS.epSquare = case newEP of
                            Nothing -> Nothing
                            Just f -> Just (toSquare (Square f (epRank (colorVal @(Opposite c)))))
          , GS.halfmoveClock = newHMC
          , GS.fullmoveNumber = newFMN
          }

        isChecked = Val.isCheck baseBoard nextTurnGS960
        hasMoves = Val.hasLegalMoves baseBoard nextTurnGS960 || not (null (generateMoves (ActiveGame b' baseBoard newCR newEP newHMC newFMN (wk,wq,bk,bq) :: ActiveGame 'FischerRandom (Opposite c) 'Safe)))

    in case (isChecked, hasMoves) of
         (True, False) -> Checkmate (Winner c)
         (False, False) -> Stalemate
         (True, True) -> Continue (ActiveGame
                                    { gameBoard = b'
                                    , internalBoard = internalB'
                                    , castlingRights = newCR
                                    , enPassantTarget = newEP
                                    , halfMoveClock = newHMC
                                    , fullMoveNumber = newFMN
                                    , variantState = (wk, wq, bk, bq)
                                    } :: ActiveGame 'FischerRandom (Opposite c) 'Checked)
         (False, True) -> Continue (ActiveGame
                                    { gameBoard = b'
                                    , internalBoard = internalB'
                                    , castlingRights = newCR
                                    , enPassantTarget = newEP
                                    , halfMoveClock = newHMC
                                    , fullMoveNumber = newFMN
                                    , variantState = (wk, wq, bk, bq)
                                    } :: ActiveGame 'FischerRandom (Opposite c) 'Safe)
