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
import Data.Bits (setBit, (.&.), (.|.), testBit, countTrailingZeros, complement)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Maybe (maybeToList, isJust, fromMaybe, listToMaybe, catMaybes)
import qualified Data.List as List

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
gameFromFEN s = do
  (baseBoard, gs) <- Fen.parseFen s

  let c = case GS.turn gs of
            T.White -> White
            T.Black -> Black

      cr = CastlingRights
           { whiteKingSide = testBit (GS.castlingRights gs) (countTrailingZeros BB.BB_H1)
           , whiteQueenSide = testBit (GS.castlingRights gs) (countTrailingZeros BB.BB_A1)
           , blackKingSide = testBit (GS.castlingRights gs) (countTrailingZeros BB.BB_H8)
           , blackQueenSide = testBit (GS.castlingRights gs) (countTrailingZeros BB.BB_A8)
           }
      -- Note: castlingRights bitboard indices might rely on lsb being correct.
      -- A1=0, B1=1 ... H1=7.
      -- BB.BB_H1 is bit 7.
      -- BB.BB_A1 is bit 0.
      -- But Data.Bits.testBit takes Int index.
      -- So I should use countTrailingZeros on BB constants.

      ep = case GS.epSquare gs of
             Nothing -> Nothing
             Just sq -> Just (getFile (fromBaseSquare sq))

      hmc = GS.halfmoveClock gs
      fmn = GS.fullmoveNumber gs

      -- Check if current player is in check
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
    wKings = squaresToBB (maybeToList (whiteKing b))
    bKings = squaresToBB (maybeToList (blackKing b))

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
toGameState :: forall v c s. (KnownColor c, ChessVariant v) => ActiveGame v c s -> GS.GameState
toGameState = gameToGameState

defaultToGameState :: forall v c s. KnownColor c => ActiveGame v c s -> GS.GameState
defaultToGameState ag = GS.GameState
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
  gameToGameState :: KnownColor c => ActiveGame v c s -> GS.GameState
  gameToGameState = defaultToGameState

-- Generate Legal Moves
generateLegalMoves :: forall v c s. (KnownColor c, ChessVariant v) => ActiveGame v c s -> [Move c]
generateLegalMoves = generateMoves

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
       _ -> error "Invalid move generated" -- Should not happen if logic is consistent
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

getRank :: Square -> Rank
getRank (Square _ r) = r

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

       Castling960Move kFrom rFrom ->
          let rank = getRank kFrom
              isKingSide = getFile rFrom > getFile kFrom
              kTo = Square (if isKingSide then FileG else FileC) rank
              rTo = Square (if isKingSide then FileF else FileD) rank
              pc = toColor (colorVal @c)

              -- We use unsafe operations because we know pieces and colors
              b1 = Base.unsafeRemovePiece b (toSquare kFrom) pc T.King
              b2 = Base.unsafeRemovePiece b1 (toSquare rFrom) pc T.Rook
              b3 = Base.unsafePutPiece b2 (toSquare kTo) (T.Piece pc T.King)
              b4 = Base.unsafePutPiece b3 (toSquare rTo) (T.Piece pc T.Rook)
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
                       DropMove _ t -> (t, t) -- Should not happen
                       Castling960Move f _ -> (f, f)

        -- 2. Update Game State

        -- Update Castling Rights
        newCR = updateCastlingRights (castlingRights ag) from to

        -- Update EP Target
        -- Check if pawn moved
        movedPiece = Base.pieceAt internalB' (toSquare to)
        isPawn = case movedPiece of
                   Just p -> T.pieceType p == T.Pawn
                   _ -> False

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
                       DropMove _ t -> (t, t)
                       Castling960Move f _ -> (f, f)

        -- 2. Update Game State
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
        isKingCapture (T.DropMove _ _) = False

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
        isSelfExplosion (T.DropMove _ _) = False

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
        internalB = internalBoard ag

        -- 1. Apply Move Basic (Move piece, handle EP/Castling movement)
        bBasic = applyMoveBase m internalB

        (from, to) = case m of
                       StandardMove f t -> (f, t)
                       PromotionMove f t _ -> (f, t)
                       CastlingMove f t -> (f, t)
                       EnPassantMove f t -> (f, t)
                       DropMove _ t -> (t, t)
                       Castling960Move f _ -> (f, f)

        -- Check if capture
        isCapture = case m of
                      StandardMove _ t -> Base.pieceAt internalB (toSquare t) /= Nothing
                      PromotionMove _ t _ -> Base.pieceAt internalB (toSquare t) /= Nothing
                      EnPassantMove _ _ -> True
                      _ -> False

        -- Explosion Logic
        (bFinal, enemyKingExploded) = if isCapture
          then
            let center = to
                -- Capturing piece explodes (remove at center)
                b1 = Base.removePieceAt bBasic (toSquare center)

                -- Surrounding Squares
                surrounds = getAdjacentSquares center

                enemyKingSq = MG.kingSquare bBasic (toColor oppC)

                -- Explode surrounding
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

        -- State Updates
        newCR = updateCastlingRights (castlingRights ag) from to

        -- EP
        movedPiece = Base.pieceAt bBasic (toSquare to) -- Note: use bBasic to check piece type before explosion
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
                       DropMove _ t -> (t, t)
                       Castling960Move f _ -> (f, f)

        -- 2. Update Game State
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
                       DropMove _ t -> (t, t)
                       Castling960Move f _ -> (f, f)

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
        wKingSq = MG.kingSquare internalB' T.White
        bKingSq = MG.kingSquare internalB' T.Black
        wInGoal = case wKingSq of Just sq -> T.squareRank sq == 7; _ -> False
        bInGoal = case bKingSq of Just sq -> T.squareRank sq == 7; _ -> False

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
                       Castling960Move f _ -> (f, f)

        -- Handle Captures and Drops
        ((wPocket', bPocket'), promotedSet') = case m of
           DropMove p _ ->
              let pockets = if c == White
                            then (Map.adjust (\x -> x - 1) p wPocket, bPocket)
                            else (wPocket, Map.adjust (\x -> x - 1) p bPocket)
              in (pockets, promotedSet)
           _ ->
              -- Check capture
              let capture = case m of
                              StandardMove _ t -> Base.pieceAt internalB (toSquare t)
                              PromotionMove _ t _ -> Base.pieceAt internalB (toSquare t)
                              EnPassantMove _ _ -> Just (T.Piece (toColor oppC) T.Pawn) -- En Passant always captures Pawn
                              _ -> Nothing

                  capturedSquare = case m of
                                     EnPassantMove f t -> Just (getEpCapturedSquare f t)
                                     _ -> if capture /= Nothing then Just to else Nothing

                  -- Update Pockets
                  pockets' = case capture of
                     Just (T.Piece _ pt) ->
                        let capturedType = fromPieceType pt
                            isPromoted = case capturedSquare of
                                           Just sq -> Set.member sq promotedSet
                                           Nothing -> False
                            addToPocket = if isPromoted then Pawn else capturedType
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
                          PromotionMove _ _ _ -> Set.insert to ps2
                          _ -> ps2

              in (pockets', ps3)

        -- State Updates
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
               safe mv = not (Val.isCheck (applyMoveBase mv board) gs)
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

-- | Create a game from FEN string (Antichess variant).
antichessGameFromFEN :: String -> Maybe (Game 'Antichess 'Active)
antichessGameFromFEN s = do
  (baseBoard, gs) <- Fen.parseFen s

  let c = case GS.turn gs of
            T.White -> White
            T.Black -> Black

      -- Antichess: No castling rights.
      cr = CastlingRights False False False False

      ep = case GS.epSquare gs of
             Nothing -> Nothing
             Just sq -> Just (getFile (fromBaseSquare sq))

      hmc = GS.halfmoveClock gs
      fmn = GS.fullmoveNumber gs

  case c of
    White ->
       let ag = ActiveGame baseBoard cr ep hmc fmn () :: ActiveGame 'Antichess 'White 'Safe
           moves = generateMoves ag
       in if null moves
          then Nothing
          else return (InProgressGame ag)
    Black ->
       let ag = ActiveGame baseBoard cr ep hmc fmn () :: ActiveGame 'Antichess 'Black 'Safe
           moves = generateMoves ag
       in if null moves
          then Nothing
          else return (InProgressGame ag)

-- | Create a game from FEN string (Horde variant).
hordeGameFromFEN :: String -> Maybe (Game 'Horde 'Active)
hordeGameFromFEN s = do
  (baseBoard, gs) <- Fen.parseFen s

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

      checked = Val.isCheck baseBoard gs
      hasMoves = Val.hasLegalMoves baseBoard gs

  if hasMoves
  then case c of
      White -> if checked
               then return $ InProgressGame (ActiveGame baseBoard cr ep hmc fmn () :: ActiveGame 'Horde 'White 'Checked)
               else return $ InProgressGame (ActiveGame baseBoard cr ep hmc fmn () :: ActiveGame 'Horde 'White 'Safe)
      Black -> if checked
               then return $ InProgressGame (ActiveGame baseBoard cr ep hmc fmn () :: ActiveGame 'Horde 'Black 'Checked)
               else return $ InProgressGame (ActiveGame baseBoard cr ep hmc fmn () :: ActiveGame 'Horde 'Black 'Safe)
  else Nothing

-- | Create a game from FEN string (Fischer Random variant).
fischerRandomGameFromFEN :: String -> Maybe (Game 'FischerRandom 'Active)
fischerRandomGameFromFEN s = do
  (baseBoard, gs, _) <- Fen.parseFenRest s

  let c = case GS.turn gs of
            T.White -> White
            T.Black -> Black

      wKingSq = MG.kingSquare baseBoard T.White
      bKingSq = MG.kingSquare baseBoard T.Black

      wRooksBits = GS.castlingRights gs .&. BB.bbRank1 .&. Base.whiteRooks baseBoard
      bRooksBits = GS.castlingRights gs .&. BB.bbRank8 .&. Base.blackRooks baseBoard

      wRooks = BB.mapBitboard id wRooksBits
      bRooks = BB.mapBitboard id bRooksBits

      getRook kingSq rooks =
         let kFile = T.squareFile kingSq
             (ks, qs) = List.partition (\sq -> T.squareFile sq > kFile) rooks
         in (listToMaybe ks, listToMaybe qs)

      (wks, wqs) = case wKingSq of
                     Just k -> getRook k wRooks
                     Nothing -> (Nothing, Nothing)

      (bks, bqs) = case bKingSq of
                     Just k -> getRook k bRooks
                     Nothing -> (Nothing, Nothing)

      vs = (fmap fromSquare wks, fmap fromSquare wqs, fmap fromSquare bks, fmap fromSquare bqs)

      cr = CastlingRights
           { whiteKingSide = isJust wks
           , whiteQueenSide = isJust wqs
           , blackKingSide = isJust bks
           , blackQueenSide = isJust bqs
           }

      ep = case GS.epSquare gs of
             Nothing -> Nothing
             Just sq -> Just (getFile (fromBaseSquare sq))

      hmc = GS.halfmoveClock gs
      fmn = GS.fullmoveNumber gs

  -- Construct candidate to generate moves and check status
  let agCandidate = ActiveGame baseBoard cr ep hmc fmn vs :: ActiveGame 'FischerRandom 'White 'Safe

  case c of
    White ->
       let ag = ActiveGame baseBoard cr ep hmc fmn vs :: ActiveGame 'FischerRandom 'White 'Safe
           moves = generateMoves ag
           isChecked = Val.isCheck baseBoard gs
       in if null moves
          then Nothing
          else if isChecked
               then return $ InProgressGame (ActiveGame baseBoard cr ep hmc fmn vs :: ActiveGame 'FischerRandom 'White 'Checked)
               else return $ InProgressGame (ActiveGame baseBoard cr ep hmc fmn vs :: ActiveGame 'FischerRandom 'White 'Safe)
    Black ->
       let ag = ActiveGame baseBoard cr ep hmc fmn vs :: ActiveGame 'FischerRandom 'Black 'Safe
           moves = generateMoves ag
           isChecked = Val.isCheck baseBoard gs
       in if null moves
          then Nothing
          else if isChecked
               then return $ InProgressGame (ActiveGame baseBoard cr ep hmc fmn vs :: ActiveGame 'FischerRandom 'Black 'Checked)
               else return $ InProgressGame (ActiveGame baseBoard cr ep hmc fmn vs :: ActiveGame 'FischerRandom 'Black 'Safe)

instance ChessVariant 'Antichess where
  generateMoves (ag :: ActiveGame 'Antichess c s) =
    let baseBoard = internalBoard ag
        gs = toGameState ag
        pseudos = MG.pseudoLegalMoves baseBoard gs

        -- Extract moves and capture info
        movesWithCap = map (\(MG.GenMove m _ cap) -> (m, cap)) pseudos

        hasCapture = any (\(_, cap) -> isJust cap) movesWithCap

        validMoves = if hasCapture
                     then map fst $ filter (\(_, cap) -> isJust cap) movesWithCap
                     else map fst movesWithCap

    in map (toCoreMove baseBoard) validMoves

  executeMove (m :: Move c) (ag :: ActiveGame 'Antichess c s) =
    let c = colorVal @c
        oppC = colorVal @(Opposite c)
        internalB = internalBoard ag
        internalB' = applyMoveBase m internalB

        -- State Updates
        newCR = CastlingRights False False False False -- No castling

        (from, to) = case m of
                       StandardMove f t -> (f, t)
                       PromotionMove f t _ -> (f, t)
                       CastlingMove f t -> (f, t)
                       EnPassantMove f t -> (f, t)
                       DropMove _ t -> (t, t)
                       Castling960Move f _ -> (f, f)

        movedPiece = Base.pieceAt internalB' (toSquare to)
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
          , GS.castlingRights = toCastlingRights newCR -- All false
          , GS.epSquare = case newEP of
                            Nothing -> Nothing
                            Just f -> Just (toSquare (Square f (epRank oppC)))
          , GS.halfmoveClock = newHMC
          , GS.fullmoveNumber = newFMN
          }

        -- Check Win Conditions

        -- 1. Current player (c) has no pieces left?
        myPieces = Base.occupiedBy internalB' (toColor c)
        iLostAll = myPieces == 0

        -- 2. Opponent (oppC) has no pieces left?
        oppPieces = Base.occupiedBy internalB' (toColor oppC)
        oppLostAll = oppPieces == 0

        -- 3. Opponent has no moves?
        -- We need to generate opponent moves to check this.
        -- But generateMoves needs ActiveGame.
        nextGameCandidate :: ActiveGame 'Antichess (Opposite c) 'Safe -- Status doesn't matter for Antichess
        nextGameCandidate = ActiveGame
                                    { internalBoard = internalB'
                                    , castlingRights = newCR
                                    , enPassantTarget = newEP
                                    , halfMoveClock = newHMC
                                    , fullMoveNumber = newFMN
                                    , variantState = ()
                                    }

        oppMoves = generateMoves nextGameCandidate
        oppHasNoMoves = null oppMoves

    in if iLostAll then Checkmate (Winner c)
       else if oppLostAll then Checkmate (Winner oppC)
       else if oppHasNoMoves then Checkmate (Winner oppC) -- Stalemate = Win for stalemated player (oppC)
       else Continue nextGameCandidate

instance ChessVariant 'Horde where
  generateMoves (ag :: ActiveGame 'Horde c s) =
     let baseBoard = internalBoard ag
         gs = toGameState ag
         c = colorVal @c
     in if c == White
        then -- White (Pawns): Pseudo-legal (no King)
             map (toCoreMove baseBoard . (\(MG.GenMove m _ _) -> m)) (MG.pseudoLegalMoves baseBoard gs)
        else -- Black (Standard): Legal
             map (toCoreMove baseBoard) (MG.legalMoves baseBoard gs)

  executeMove (m :: Move c) (ag :: ActiveGame 'Horde c s) =
    let c = colorVal @c
        oppC = colorVal @(Opposite c)
        internalB = internalBoard ag
        internalB' = applyMoveBase m internalB

        -- Standard Updates
        (from, to) = case m of
                       StandardMove f t -> (f, t)
                       PromotionMove f t _ -> (f, t)
                       CastlingMove f t -> (f, t)
                       EnPassantMove f t -> (f, t)
                       DropMove _ t -> (t, t)
                       Castling960Move f _ -> (f, f)

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

        nextTurnGS = GS.GameState
          { GS.turn = toColor oppC
          , GS.castlingRights = toCastlingRights newCR
          , GS.epSquare = case newEP of
                            Nothing -> Nothing
                            Just f -> Just (toSquare (Square f (epRank oppC)))
          , GS.halfmoveClock = newHMC
          , GS.fullmoveNumber = newFMN
          }

        -- Win Conditions

        -- 1. Black wins if all White pieces captured.
        whitePieces = Base.occupiedBy internalB' T.White
        whiteLostAll = whitePieces == 0

        -- 2. White wins if Black is Checkmated.
        -- 3. Draw if Stalemate.

        isChecked = Val.isCheck internalB' nextTurnGS

        -- Check if opponent has moves
        nextGameCandidate :: ActiveGame 'Horde (Opposite c) 'Safe -- Placeholder status
        nextGameCandidate = ActiveGame
                                    { internalBoard = internalB'
                                    , castlingRights = newCR
                                    , enPassantTarget = newEP
                                    , halfMoveClock = newHMC
                                    , fullMoveNumber = newFMN
                                    , variantState = ()
                                    }

        oppMoves = generateMoves nextGameCandidate
        hasMoves = not (null oppMoves)

        -- Fix status for nextGameCandidate based on check
        nextGameChecked :: ActiveGame 'Horde (Opposite c) 'Checked
        nextGameChecked = ActiveGame
                                    { internalBoard = internalB'
                                    , castlingRights = newCR
                                    , enPassantTarget = newEP
                                    , halfMoveClock = newHMC
                                    , fullMoveNumber = newFMN
                                    , variantState = ()
                                    }

    in if whiteLostAll then Checkmate (Winner Black)
       else case (isChecked, hasMoves) of
         (True, False) -> Checkmate (Winner White) -- Black mated (White cannot be mated)
         (False, False) -> Stalemate
         (True, True) -> Continue nextGameChecked
         (False, True) -> Continue nextGameCandidate

instance ChessVariant 'FischerRandom where
  generateMoves (ag :: ActiveGame 'FischerRandom c s) =
    let baseBoard = internalBoard ag
        gs = defaultToGameState ag
        c = colorVal @c

        isStandardCastling (T.Move from to _) =
             case Base.pieceAt baseBoard (toSquare from) of
                Just (T.Piece _ T.King) -> abs (T.unSquare from - T.unSquare to) == 2
                _ -> False
        isStandardCastling _ = False

        baseMoves = MG.legalMoves baseBoard gs
        validBaseMoves = filter (not . isStandardCastling) baseMoves

        coreBaseMoves = map (toCoreMove baseBoard) validBaseMoves

        castling960 = generateCastlingMoves960 ag

    in coreBaseMoves ++ castling960

  executeMove (m :: Move c) (ag :: ActiveGame 'FischerRandom c s) =
    let c = colorVal @c
        oppC = colorVal @(Opposite c)
        internalB = internalBoard ag

        internalB' = applyMoveBase m internalB

        (from, to) = case m of
             Castling960Move kFrom _ -> (kFrom, kFrom)
             StandardMove f t -> (f, t)
             PromotionMove f t _ -> (f, t)
             CastlingMove f t -> (f, t)
             EnPassantMove f t -> (f, t)
             DropMove _ t -> (t, t)

        oldCR = castlingRights ag
        newCR = case m of
             Castling960Move _ _ ->
                 if c == White
                 then oldCR { whiteKingSide = False, whiteQueenSide = False }
                 else oldCR { blackKingSide = False, blackQueenSide = False }
             _ ->
                 let (wks, wqs, bks, bqs) = variantState ag

                     upd right (Just rSq) | from == rSq || to == rSq = False
                     upd right _ = right

                     movingPiece = Base.pieceAt internalB (toSquare from)
                     isKing = case movingPiece of Just (T.Piece _ T.King) -> True; _ -> False

                     oldWKS = whiteKingSide oldCR
                     oldWQS = whiteQueenSide oldCR
                     oldBKS = blackKingSide oldCR
                     oldBQS = blackQueenSide oldCR

                     wks' = if isKing && c == White then False else upd oldWKS wks
                     wqs' = if isKing && c == White then False else upd oldWQS wqs
                     bks' = if isKing && c == Black then False else upd oldBKS bks
                     bqs' = if isKing && c == Black then False else upd oldBQS bqs

                 in CastlingRights wks' wqs' bks' bqs'

        movedPiece = Base.pieceAt internalB' (toSquare to)
        isPawn = case movedPiece of
                   Just p -> T.pieceType p == T.Pawn
                   _ -> False

        newEP = case m of
                  StandardMove f t -> if isPawn && isDoublePush f t then Just (getFile f) else Nothing
                  _ -> Nothing

        newHMC = halfMoveClock ag + 1
        newFMN = fullMoveNumber ag + (if c == Black then 1 else 0)

        vs = variantState ag

        -- Opponent check
        oppCR = newCR
        oppGame = ActiveGame internalB' newCR newEP newHMC newFMN vs :: ActiveGame 'FischerRandom (Opposite c) 'Safe
        oppMoves = generateMoves oppGame
        realHasMoves = not (null oppMoves)

        nextTurnGS = (defaultToGameState ag)
          { GS.turn = toColor oppC
          , GS.castlingRights = toCastlingRights960 newCR vs
          , GS.epSquare = case newEP of
                            Nothing -> Nothing
                            Just f -> Just (toSquare (Square f (epRank oppC)))
          , GS.halfmoveClock = newHMC
          , GS.fullmoveNumber = newFMN
          }

        isChecked = Val.isCheck internalB' nextTurnGS

    in case (isChecked, realHasMoves) of
         (True, False) -> Checkmate (Winner c)
         (False, False) -> Stalemate
         (True, True) -> Continue (ActiveGame internalB' newCR newEP newHMC newFMN vs :: ActiveGame 'FischerRandom (Opposite c) 'Checked)
         (False, True) -> Continue (ActiveGame internalB' newCR newEP newHMC newFMN vs :: ActiveGame 'FischerRandom (Opposite c) 'Safe)

  gameToGameState ag =
      let (wks, wqs, bks, bqs) = variantState ag
          cr = castlingRights ag
          bitFor s = BB.bbFromSquare (toSquare s)
          wksB = if whiteKingSide cr then maybe 0 bitFor wks else 0
          wqsB = if whiteQueenSide cr then maybe 0 bitFor wqs else 0
          bksB = if blackKingSide cr then maybe 0 bitFor bks else 0
          bqsB = if blackQueenSide cr then maybe 0 bitFor bqs else 0
          newCR = wksB .|. wqsB .|. bksB .|. bqsB

          baseGS = defaultToGameState ag
      in baseGS { GS.castlingRights = newCR }

toCastlingRights960 :: CastlingRights -> VariantState 'FischerRandom -> GS.CastlingRights
toCastlingRights960 cr (wks, wqs, bks, bqs) =
    let bitFor s = BB.bbFromSquare (toSquare s)
        wksB = if whiteKingSide cr then maybe 0 bitFor wks else 0
        wqsB = if whiteQueenSide cr then maybe 0 bitFor wqs else 0
        bksB = if blackKingSide cr then maybe 0 bitFor bks else 0
        bqsB = if blackQueenSide cr then maybe 0 bitFor bqs else 0
    in wksB .|. wqsB .|. bksB .|. bqsB

generateCastlingMoves960 :: forall c s. KnownColor c => ActiveGame 'FischerRandom c s -> [Move c]
generateCastlingMoves960 ag =
  let c = colorVal @c
      cr = castlingRights ag
      (wks, wqs, bks, bqs) = variantState ag

      board = internalBoard ag
      kingSqT = MG.kingSquare board (toColor c)

      (ksRook, qsRook) = if c == White then (wks, wqs) else (bks, bqs)
      (canKS, canQS) = if c == White
                       then (whiteKingSide cr, whiteQueenSide cr)
                       else (blackKingSide cr, blackQueenSide cr)

      rank = if c == White then Rank1 else Rank8

      ksKingDest = Square FileG rank
      ksRookDest = Square FileF rank
      qsKingDest = Square FileC rank
      qsRookDest = Square FileD rank

      checkCastling :: Maybe Square -> Square -> Square -> Bool -> Maybe (Move c)
      checkCastling Nothing _ _ _ = Nothing
      checkCastling (Just rookSq) kDest rDest allowed
        | not allowed = Nothing
        | otherwise =
           case kingSqT of
             Nothing -> Nothing
             Just kSqT ->
               let
                   -- Convert Core Squares to Types.Square for BB operations
                   rookSqT = toSquare rookSq
                   kDestT = toSquare kDest
                   rDestT = toSquare rDest

                   bbPathK = BB.between kSqT kDestT .|. BB.bbFromSquare kDestT
                   bbPathR = BB.between rookSqT rDestT .|. BB.bbFromSquare rDestT

                   requiredEmpty = bbPathK .|. bbPathR

                   ignored = BB.bbFromSquare kSqT .|. BB.bbFromSquare rookSqT
                   occupied = Base.occupiedTotal board .&. complement ignored

                   pathClear = (requiredEmpty .&. occupied) == 0

                   oppC = Base.oppositeColor (toColor c)
                   isSafe sq = not (Base.isAttackedBy board oppC sq)

                   kingPathSquares = BB.mapBitboard id (BB.between kSqT kDestT) ++ [kSqT, kDestT]
                   pathSafe = all isSafe kingPathSquares

               in if pathClear && pathSafe
                  then Just (Castling960Move (fromSquare kSqT) rookSq) -- Use Core Squares for Move
                  else Nothing

      moves = catMaybes
              [ checkCastling ksRook ksKingDest ksRookDest canKS
              , checkCastling qsRook qsKingDest qsRookDest canQS
              ]
  in moves
