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
import Data.Bits (setBit, (.&.), (.|.), testBit, countTrailingZeros)
import qualified Data.Map as Map
import qualified Data.Set as Set

-- | Create the initial game state for Standard chess.
initialGame :: Game 'Standard 'Active
initialGame =
  let b = initialBoard
      ag = ActiveGame
           { gameBoard = b
           , internalBoard = toBaseBoard b
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
               then return $ InProgressGame (ActiveGame board baseBoard cr ep hmc fmn () :: ActiveGame 'Standard 'White 'Checked)
               else return $ InProgressGame (ActiveGame board baseBoard cr ep hmc fmn () :: ActiveGame 'Standard 'White 'Safe)
      Black -> if checked
               then return $ InProgressGame (ActiveGame board baseBoard cr ep hmc fmn () :: ActiveGame 'Standard 'Black 'Checked)
               else return $ InProgressGame (ActiveGame board baseBoard cr ep hmc fmn () :: ActiveGame 'Standard 'Black 'Safe)
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
                    then return $ InProgressGame (ActiveGame board baseBoard cr ep hmc fmn vs :: ActiveGame 'Crazyhouse 'White 'Checked)
                    else return $ InProgressGame (ActiveGame board baseBoard cr ep hmc fmn vs :: ActiveGame 'Crazyhouse 'White 'Safe)
               else Nothing
      Black -> if hasMoves @'Black
               then if checked
                    then return $ InProgressGame (ActiveGame board baseBoard cr ep hmc fmn vs :: ActiveGame 'Crazyhouse 'Black 'Checked)
                    else return $ InProgressGame (ActiveGame board baseBoard cr ep hmc fmn vs :: ActiveGame 'Crazyhouse 'Black 'Safe)
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
       (Just (SomePiece _), Just pt) ->
          PromotionMove fromSq toSq (fromPieceType pt)
       (Just (SomePiece piece), Nothing) ->
          if isCastlingMove piece fromSq toSq
          then CastlingMove fromSq toSq
          else if isEnPassantMove piece fromSq toSq b
               then EnPassantMove fromSq toSq
               else StandardMove fromSq toSq
       _ -> error "Invalid move generated" -- Should not happen if logic is consistent
toCoreMove _ (T.DropMove pt t) = DropMove (fromPieceType pt) (fromSquare t)
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

-- Apply Move Helper (pure board update)
applyMoveBoard :: forall c. KnownColor c => Board -> Move c -> Board
applyMoveBoard b m =
    case m of
       StandardMove f t -> movePiece f t b
       PromotionMove f t pt ->
         let b1 = removePieceAt f b
             promoted = mkPiece (colorVal @c) pt
         in putPieceAt t promoted b1
       CastlingMove f t ->
         let b1 = movePiece f t b
             (rf, rt) = getCastlingRookMove f t
         in movePiece rf rt b1
       EnPassantMove f t ->
         let b1 = movePiece f t b
             capSq = getEpCapturedSquare f t
         in removePieceAt capSq b1
       DropMove p t ->
         let promoted = mkPiece (colorVal @c) p
         in putPieceAt t promoted b

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

-- Apply Move
applyMove :: forall v c s. (KnownColor c, KnownColor (Opposite c), ChessVariant v) => Move c -> ActiveGame v c s -> MoveResult v (Opposite c)
applyMove = executeMove

instance ChessVariant 'Standard where
  generateMoves (ag :: ActiveGame 'Standard c s) =
    let b = gameBoard ag
        baseBoard = internalBoard ag
        gs = toGameState ag
        baseMoves = MG.legalMoves baseBoard gs
    in map (toCoreMove b) baseMoves

  executeMove (m :: Move c) (ag :: ActiveGame 'Standard c s) =
    let
        -- 1. Update Board
        c = colorVal @c
        b = gameBoard ag
        b' = applyMoveBoard b m

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
                                    { gameBoard = b'
                                    , internalBoard = internalB'
                                    , castlingRights = newCR
                                    , enPassantTarget = newEP
                                    , halfMoveClock = newHMC
                                    , fullMoveNumber = newFMN
                                    , variantState = ()
                                    } :: ActiveGame 'Standard (Opposite c) 'Checked)
         (False, True) -> Continue (ActiveGame
                                    { gameBoard = b'
                                    , internalBoard = internalB'
                                    , castlingRights = newCR
                                    , enPassantTarget = newEP
                                    , halfMoveClock = newHMC
                                    , fullMoveNumber = newFMN
                                    , variantState = ()
                                    } :: ActiveGame 'Standard (Opposite c) 'Safe)

instance ChessVariant 'ThreeCheck where
  generateMoves (ag :: ActiveGame 'ThreeCheck c s) =
    let b = gameBoard ag
        baseBoard = internalBoard ag
        gs = toGameState ag
        baseMoves = MG.legalMoves baseBoard gs
    in map (toCoreMove b) baseMoves

  executeMove (m :: Move c) (ag :: ActiveGame 'ThreeCheck c s) =
    let
        -- 1. Update Board
        c = colorVal @c
        oppC = colorVal @(Opposite c)
        b = gameBoard ag
        b' = applyMoveBoard b m

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

        movedPiece = getPieceAt to b'
        isPawn = case movedPiece of
                   Just (SomePiece WPawn) -> True
                   Just (SomePiece BPawn) -> True
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
                                    { gameBoard = b'
                                    , internalBoard = internalB'
                                    , castlingRights = newCR
                                    , enPassantTarget = newEP
                                    , halfMoveClock = newHMC
                                    , fullMoveNumber = newFMN
                                    , variantState = newVariantState
                                    } :: ActiveGame 'ThreeCheck (Opposite c) 'Checked)
         (False, True) -> Continue (ActiveGame
                                    { gameBoard = b'
                                    , internalBoard = internalB'
                                    , castlingRights = newCR
                                    , enPassantTarget = newEP
                                    , halfMoveClock = newHMC
                                    , fullMoveNumber = newFMN
                                    , variantState = newVariantState
                                    } :: ActiveGame 'ThreeCheck (Opposite c) 'Safe)

instance ChessVariant 'Atomic where
  generateMoves (ag :: ActiveGame 'Atomic c s) =
    let b = gameBoard ag
        baseBoard = internalBoard ag
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

    in map (toCoreMove b . (\(MG.GenMove m _ _) -> m)) validMoves

  executeMove (m :: Move c) (ag :: ActiveGame 'Atomic c s) =
    let c = colorVal @c
        oppC = colorVal @(Opposite c)
        b = gameBoard ag

        -- 1. Apply Move Basic (Move piece, handle EP/Castling movement)
        bBasic = applyMoveBoard b m

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
                                    , internalBoard = baseBoard
                                    , castlingRights = newCR
                                    , enPassantTarget = newEP
                                    , halfMoveClock = newHMC
                                    , fullMoveNumber = newFMN
                                    , variantState = ()
                                    } :: ActiveGame 'Atomic (Opposite c) 'Checked)
         (False, True) -> Continue (ActiveGame
                                    { gameBoard = bFinal
                                    , internalBoard = baseBoard
                                    , castlingRights = newCR
                                    , enPassantTarget = newEP
                                    , halfMoveClock = newHMC
                                    , fullMoveNumber = newFMN
                                    , variantState = ()
                                    } :: ActiveGame 'Atomic (Opposite c) 'Safe)

instance ChessVariant 'KingOfTheHill where
  generateMoves (ag :: ActiveGame 'KingOfTheHill c s) =
    let b = gameBoard ag
        baseBoard = internalBoard ag
        gs = toGameState ag
        baseMoves = MG.legalMoves baseBoard gs
    in map (toCoreMove b) baseMoves

  executeMove (m :: Move c) (ag :: ActiveGame 'KingOfTheHill c s) =
    let
        -- 1. Update Board
        c = colorVal @c
        b = gameBoard ag
        b' = applyMoveBoard b m

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

        movedPiece = getPieceAt to b'
        isPawn = case movedPiece of
                   Just (SomePiece WPawn) -> True
                   Just (SomePiece BPawn) -> True
                   _ -> False

        isKing = case movedPiece of
                   Just (SomePiece WKing) -> True
                   Just (SomePiece BKing) -> True
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
                                    { gameBoard = b'
                                    , internalBoard = internalB'
                                    , castlingRights = newCR
                                    , enPassantTarget = newEP
                                    , halfMoveClock = newHMC
                                    , fullMoveNumber = newFMN
                                    , variantState = ()
                                    } :: ActiveGame 'KingOfTheHill (Opposite c) 'Checked)
         (False, True) -> Continue (ActiveGame
                                    { gameBoard = b'
                                    , internalBoard = internalB'
                                    , castlingRights = newCR
                                    , enPassantTarget = newEP
                                    , halfMoveClock = newHMC
                                    , fullMoveNumber = newFMN
                                    , variantState = ()
                                    } :: ActiveGame 'KingOfTheHill (Opposite c) 'Safe)

instance ChessVariant 'RacingKings where
  generateMoves (ag :: ActiveGame 'RacingKings c s) =
    let b = gameBoard ag
        baseBoard = internalBoard ag
        gs = toGameState ag

        -- Get Standard moves (handles own check safety)
        baseMoves = MG.legalMoves baseBoard gs
        coreMoves = map (toCoreMove b) baseMoves

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
        b = gameBoard ag

        b' = applyMoveBoard b m

        -- Update Base Board
        internalB = internalBoard ag
        internalB' = applyMoveBase m internalB

        (from, to) = case m of
                       StandardMove f t -> (f, t)
                       PromotionMove f t _ -> (f, t)
                       CastlingMove f t -> (f, t)
                       EnPassantMove f t -> (f, t)

        newCR = updateCastlingRights (castlingRights ag) from to

        movedPiece = getPieceAt to b'
        isPawn = case movedPiece of
                   Just (SomePiece WPawn) -> True
                   Just (SomePiece BPawn) -> True
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
                                    { gameBoard = b'
                                    , internalBoard = internalB'
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
        wKing = whiteKing b'
        bKing = blackKing b'
        wInGoal = case wKing of Square _ Rank8 -> True; _ -> False
        bInGoal = case bKing of Square _ Rank8 -> True; _ -> False

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
    let b = gameBoard ag
        baseBoard = internalBoard ag
        gs = toGameState ag
        c = colorVal @c

        baseMoves = MG.legalMoves baseBoard gs
        standardMoves = map (toCoreMove b) baseMoves

        (wPocket, bPocket, _) = variantState ag
        pocket = if c == White then wPocket else bPocket

        emptySquares = [ Square f r | f <- [FileA .. FileH], r <- [Rank1 .. Rank8], getPieceAt (Square f r) b == Nothing ]

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
        b = gameBoard ag
        b' = applyMoveBoard b m
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
              let capture = case m of
                              StandardMove _ t -> getPieceAt t b
                              PromotionMove _ t _ -> getPieceAt t b
                              EnPassantMove _ _ -> Just (mkPiece oppC Pawn) -- En Passant always captures Pawn
                              _ -> Nothing

                  capturedSquare = case m of
                                     EnPassantMove f t -> Just (getEpCapturedSquare f t)
                                     _ -> if capture /= Nothing then Just to else Nothing

                  -- Update Pockets
                  pockets' = case capture of
                     Just (SomePiece p) ->
                        let capturedType = pieceType p
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
                          PromotionMove _ t _ -> Set.insert to ps2
                          _ -> ps2

              in (pockets', ps3)

        -- State Updates
        newCR = case m of
                  DropMove _ _ -> castlingRights ag
                  _ -> updateCastlingRights (castlingRights ag) from to

        movedPiece = getPieceAt to b'
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
                                    { gameBoard = b'
                                    , internalBoard = internalB'
                                    , castlingRights = newCR
                                    , enPassantTarget = newEP
                                    , halfMoveClock = newHMC
                                    , fullMoveNumber = newFMN
                                    , variantState = newState
                                    } :: ActiveGame 'Crazyhouse (Opposite c) 'Checked)
         (False, True) -> Continue (ActiveGame
                                    { gameBoard = b'
                                    , internalBoard = internalB'
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
