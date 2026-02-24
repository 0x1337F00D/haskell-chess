{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms #-}
module Chess.Board
  ( -- * The Board Type
    Board(..)
  , initialBoard
    -- * Game Logic
  , applyMove
  , applyGenMove
  , applyGenMoveFast
  , legalMoves
  , legalGenMoves
  , legalGenMovesVector
  , pseudoLegalMoves
  , pseudoLegalGenMoves
  , captureMoves
  , captureGenMoves
  , legalGenQuiets
  , legalGenPromotions
  , pseudoLegalQuiets
  , pseudoLegalPromotions
  , isCheck
  , isKingSafe
  , isCheckmate
  , isStalemate
  , hasInsufficientMaterial
  , outcome
    -- * Safe Interface
  , ValidatedBoard
  , SomeValidatedBoard(..)
  , LegalMove
  , trustBoard
  , getBoard
  , getGenMove
  , MoveGenerator(..)
  , applyLegalMove
  , moveFrom
  , moveTo
  , movePromotion
  , isCapture
  , isPromotion
  , toGenMove
  , isLegalMove
  , mkLegalMove
    -- * Notation
  , fen
  , parseFen
  , san
  , parseSan
  , uci
  , fromUci
    -- * Re-exports
  , module Chess.Types
  , MoveGen.GenMove(..)
  , pattern MoveGen.GenQuiet
  , pattern MoveGen.GenCapture
  , pattern MoveGen.GenEnPassant
  , pattern MoveGen.GenCastling
  , pattern MoveGen.GenPromotion
  , pattern MoveGen.GenPromotionCapture
  ) where

import Data.Bits (testBit, xor)
import Data.Word (Word64)
import qualified Data.Vector.Unboxed as U

import Chess.Types
import qualified Chess.Board.Base as Base
import qualified Chess.Board.GameState as GS
import qualified Chess.Board.MoveGen as MoveGen
import qualified Chess.Board.Validation as Val
import qualified Chess.Board.Fen as Fen
import qualified Chess.Board.San as San
import qualified Chess.Board.Uci as Uci
import qualified Chess.Board.Zobrist as Zobrist

-- | The primary board type combining piece placement and game state.
data Board = Board
  { pieces :: !Base.Board
  , history :: ![Word64]
  } deriving (Eq, Show)

-- | The standard initial chess position.
initialBoard :: Board
initialBoard =
  case Fen.parseFen startingFEN of
    Just b -> Board b []
    Nothing -> error "Internal error: Failed to parse starting FEN"

-- | Apply a move to the board, updating pieces and game state (counters, rights, etc).
applyMove :: Board -> Move -> Board
applyMove board@(Board b _) (Move from to promo) =
    let c = GS.getTurn (Base.statePacked b)
        fromI = unSquare from
    in if not (testBit (Base.occupiedBy b c) fromI)
       then board -- Invalid move (empty or wrong color)
       else
        let
            -- Resolve pieces efficiently
            pt = Base.findPieceType b c from
            toI = unSquare to
            isCapture = testBit (Base.occupiedTotal b) toI

            gm = case promo of
                   Just ppt ->
                       if isCapture
                       then
                           let capPt = Base.findPieceType b (Base.oppositeColor c) to
                           in MoveGen.GenPromotionCapture from to ppt capPt
                       else MoveGen.GenPromotion from to ppt
                   Nothing ->
                       if isCapture
                       then
                           let capPt = Base.findPieceType b (Base.oppositeColor c) to
                           in MoveGen.GenCapture from to pt capPt
                       else
                          if pt == Pawn && squareFile from /= squareFile to
                          then MoveGen.GenEnPassant from to
                          else if pt == King && abs (unSquare from - unSquare to) == 2
                          then MoveGen.GenCastling from to
                          else MoveGen.GenQuiet from to pt

        in applyMoveHelper board gm
applyMove b NullMove = b
applyMove b _ = b

-- | Apply a move using GenMove info (skipping piece lookup).
applyGenMove :: Board -> MoveGen.GenMove -> Board
applyGenMove board gm = applyMoveHelper board gm

-- | Apply a move using GenMove info (skipping Zobrist updates for performance).
applyGenMoveFast :: Board -> MoveGen.GenMove -> Board
applyGenMoveFast board gm = applyMoveHelperFast board gm

-- | Helper to apply move logic given resolved pieces.
{-# INLINE applyMoveHelper #-}
applyMoveHelper :: Board -> MoveGen.GenMove -> Board
applyMoveHelper (Board b hist) gm =
    let
        -- 1. Apply move to pieces (returns board with OLD state)
        b' = MoveGen.applyMoveBoardFast b gm

        s = Base.statePacked b
        z = Base.stateZobrist b
        c = GS.getTurn s
        oppC = Base.oppositeColor c

        -- Extract info from GenMove
        (from, to, pt, captured, promo, isCap, isCastling, isEP) = case gm of
            MoveGen.GenQuiet f t p -> (f, t, p, Nothing, Nothing, False, False, False)
            MoveGen.GenCapture f t p cap -> (f, t, p, Just cap, Nothing, True, False, False)
            MoveGen.GenEnPassant f t -> (f, t, Pawn, Just Pawn, Nothing, True, False, True)
            MoveGen.GenCastling f t -> (f, t, King, Nothing, Nothing, False, True, False)
            MoveGen.GenPromotion f t p -> (f, t, Pawn, Nothing, Just p, False, False, False)
            MoveGen.GenPromotionCapture f t p cap -> (f, t, Pawn, Just cap, Just p, True, False, False)

        isPawn = pt == Pawn

        -- Zobrist Updates ----------------------------------------------------
        epSq = GS.getEpSquare s
        castlingRights = GS.getCastlingRights s

        h0 = z
             `xor` Zobrist.zobristEp epSq           -- Remove old EP
             `xor` Zobrist.zobristCastling castlingRights -- Remove old CR
             `xor` Zobrist.zobristBlackMove                     -- Toggle turn

        -- Remove moving piece from 'from'
        h1 = h0 `xor` Zobrist.zobristPiece c pt from

        -- Add moving piece to 'to' (or promoted piece)
        h2 = case promo of
               Nothing -> h1 `xor` Zobrist.zobristPiece c pt to
               Just p  -> h1 `xor` Zobrist.zobristPiece c p to

        -- Handle Captures
        h3 = case captured of
               Just cp ->
                   if isEP
                   then
                        let capSq = if c == White
                                    then Square (unSquare to - 8)
                                    else Square (unSquare to + 8)
                        in h2 `xor` Zobrist.zobristPiece oppC Pawn capSq
                   else h2 `xor` Zobrist.zobristPiece oppC cp to
               Nothing -> h2

        -- Handle Castling (Rook moves)
        h4 = if isCastling
             then
                   let (rookFrom, rookTo) = if to == Square (unSquare from + 2) -- Kingside
                                            then (Square (unSquare from + 3), Square (unSquare from + 1))
                                            else (Square (unSquare from - 4), Square (unSquare from - 1))
                   in h3 `xor` Zobrist.zobristPiece c Rook rookFrom
                         `xor` Zobrist.zobristPiece c Rook rookTo
             else h3

        -- --------------------------------------------------------------------

        halfmove = GS.getHalfmoveClock s
        fullmove = GS.getFullmoveNumber s

        -- Halfmove clock: Reset on pawn move or capture
        halfmove' = if isPawn || isCap then 0 else halfmove + 1

        -- Fullmove number: Increment after Black's move
        fullmove' = if c == Black then fullmove + 1 else fullmove

        -- Castling rights updates
        -- If moving King, lose castling rights for that color
        s1 = case pt of
                King -> GS.removeColorCastlingRights s c
                Rook -> GS.removeCastlingRight s from
                _ -> s

        -- If capturing Rook, lose castling rights for that rook's square
        s2 = case captured of
                Just Rook ->
                    -- For EP, capture is Pawn, so this won't trigger.
                    -- Normal capture of Rook triggers.
                    GS.removeCastlingRight s1 to
                _ -> s1

        -- En Passant square
        -- Set if pawn double push
        ep' = if isPawn && abs (squareRank from - squareRank to) == 2
              then midSquare from to
              else NoSquare

        -- Finalize Hash
        hFinal = h4 `xor` Zobrist.zobristCastling (GS.getCastlingRights s2)
                    `xor` Zobrist.zobristEp ep'

        nextTurn = oppC

        -- Pack new state
        newState = GS.mkStatePacked nextTurn (GS.getCastlingRights s2) ep' halfmove' fullmove'

        -- 0. Update history
        -- Store the Zobrist hash of the previous position (z)
        hist' = z : hist

        -- Optimization: Clear history if halfmove clock resets (pawn move or capture)
        histFinal = if halfmove' == 0 then [] else hist'

        -- Update fields in b'
        bFinal = b' { Base.statePacked = newState, Base.stateZobrist = hFinal }

    in Board bFinal histFinal

-- | Helper to apply move logic given resolved pieces (no Zobrist).
{-# INLINE applyMoveHelperFast #-}
applyMoveHelperFast :: Board -> MoveGen.GenMove -> Board
applyMoveHelperFast (Board b _) gm =
    let
        -- 1. Update pieces (using fast path)
        b' = MoveGen.applyMoveBoardFast b gm

        s = Base.statePacked b
        c = GS.getTurn s
        oppC = Base.oppositeColor c

        -- Extract info from GenMove
        (from, to, pt, captured, isCap, isPawn) = case gm of
            MoveGen.GenQuiet f t p -> (f, t, p, Nothing, False, p == Pawn)
            MoveGen.GenCapture f t p cap -> (f, t, p, Just cap, True, p == Pawn)
            MoveGen.GenEnPassant f t -> (f, t, Pawn, Just Pawn, True, True)
            MoveGen.GenCastling f t -> (f, t, King, Nothing, False, False)
            MoveGen.GenPromotion f t _ -> (f, t, Pawn, Nothing, False, True)
            MoveGen.GenPromotionCapture f t _ cap -> (f, t, Pawn, Just cap, True, True)

        halfmove = GS.getHalfmoveClock s
        fullmove = GS.getFullmoveNumber s

        -- Halfmove clock: Reset on pawn move or capture
        halfmove' = if isPawn || isCap then 0 else halfmove + 1

        -- Fullmove number: Increment after Black's move
        fullmove' = if c == Black then fullmove + 1 else fullmove

        -- Castling rights updates
        s1 = case pt of
                King -> GS.removeColorCastlingRights s c
                Rook -> GS.removeCastlingRight s from
                _ -> s

        s2 = case captured of
                Just Rook -> GS.removeCastlingRight s1 to
                _ -> s1

        -- En Passant square
        ep' = if isPawn && abs (squareRank from - squareRank to) == 2
              then midSquare from to
              else NoSquare

        nextTurn = oppC

        newState = GS.mkStatePacked nextTurn (GS.getCastlingRights s2) ep' halfmove' fullmove'

        -- No history tracking for fast perft
        -- Zobrist hash is not updated (kept stale)
        bFinal = b' { Base.statePacked = newState }

    in Board bFinal []

-- | Generate all legal moves for the current position.
legalMoves :: Board -> [Move]
legalMoves (Board b _) = MoveGen.legalMoves b

-- | Generate all legal moves preserving piece info.
legalGenMoves :: Board -> [MoveGen.GenMove]
legalGenMoves (Board b _) = U.toList $ MoveGen.legalGenMoves b

-- | Generate all legal moves as an unboxed vector.
legalGenMovesVector :: Board -> U.Vector MoveGen.GenMove
legalGenMovesVector (Board b _) = MoveGen.legalGenMoves b

-- | Generate all pseudo-legal moves.
pseudoLegalMoves :: Board -> [Move]
pseudoLegalMoves (Board b _) = map MoveGen.genMoveToMove $ U.toList $ MoveGen.pseudoLegalMoves b

-- | Generate all pseudo-legal moves as GenMoves.
pseudoLegalGenMoves :: Board -> [MoveGen.GenMove]
pseudoLegalGenMoves (Board b _) = MoveGen.pseudoLegalMovesList b

-- | Check if the king of the given color is safe (not attacked).
isKingSafe :: Board -> Color -> Bool
isKingSafe (Board b _) c =
    case MoveGen.kingSquare b c of
        Nothing -> False -- Should not happen
        Just k -> not (Base.isAttackedBy b (Base.oppositeColor c) k)

-- | Generate all legal capture moves.
captureMoves :: Board -> [Move]
captureMoves (Board b _) = MoveGen.legalCaptures b

-- | Generate all legal capture moves preserving piece info.
captureGenMoves :: Board -> [MoveGen.GenMove]
captureGenMoves (Board b _) = U.toList $ MoveGen.legalGenCaptures b

-- | Generate all legal quiet moves preserving piece info.
legalGenQuiets :: Board -> [MoveGen.GenMove]
legalGenQuiets (Board b _) = U.toList $ MoveGen.legalGenQuiets b

-- | Generate all legal promotion moves preserving piece info.
legalGenPromotions :: Board -> [MoveGen.GenMove]
legalGenPromotions (Board b _) = U.toList $ MoveGen.legalGenPromotions b

-- | Generate all pseudo-legal quiet moves.
pseudoLegalQuiets :: Board -> [Move]
pseudoLegalQuiets (Board b _) = map MoveGen.genMoveToMove $ U.toList $ MoveGen.pseudoLegalQuiets b

-- | Generate all pseudo-legal promotion moves.
pseudoLegalPromotions :: Board -> [Move]
pseudoLegalPromotions (Board b _) = map MoveGen.genMoveToMove $ U.toList $ MoveGen.pseudoLegalPromotions b

-- | Check if the side to move is in check.
isCheck :: Board -> Bool
isCheck (Board b _) = Val.isCheck b

-- | Check if the side to move is checkmated.
isCheckmate :: Board -> Bool
isCheckmate (Board b _) = Val.isCheckmate b

-- | Check if the game is in stalemate.
isStalemate :: Board -> Bool
isStalemate (Board b _) = Val.isStalemate b

-- | Check if the game is drawn by insufficient material.
hasInsufficientMaterial :: Board -> Bool
hasInsufficientMaterial (Board b _) = Val.hasInsufficientMaterial b

-- | Determine the outcome of the game, if ended.
outcome :: Board -> Maybe Outcome
outcome (Board b h) = Val.outcome b h

-- | Convert board to FEN string.
fen :: Board -> String
fen (Board b _) = Fen.fen b

-- | Parse FEN string to Board.
parseFen :: String -> Maybe Board
parseFen s = case Fen.parseFen s of
    Just b -> Just (Board b [])
    Nothing -> Nothing

-- | Convert move to SAN.
san :: Board -> Move -> String
san (Board b _) m = San.san b m

-- | Parse SAN string to Move.
parseSan :: Board -> String -> Maybe Move
parseSan (Board b _) s = San.parseSan b s

-- | Convert move to UCI.
uci :: Move -> String
uci = Uci.uci

-- | Parse UCI string to Move.
fromUci :: String -> Maybe Move
fromUci = Uci.fromUci

-- Safe Interface

newtype ValidatedBoard (s :: CheckStatus) = ValidatedBoard Board deriving (Eq, Show)
data SomeValidatedBoard where
    InCheckBoard :: ValidatedBoard 'InCheck -> SomeValidatedBoard
    NotInCheckBoard :: ValidatedBoard 'NotInCheck -> SomeValidatedBoard

deriving instance Show SomeValidatedBoard

newtype LegalMove = LegalMove MoveGen.GenMove deriving (Eq, Show)

trustBoard :: Board -> SomeValidatedBoard
trustBoard b@(Board bb _) =
    if Val.isCheck bb
    then InCheckBoard (ValidatedBoard b)
    else NotInCheckBoard (ValidatedBoard b)

getBoard :: ValidatedBoard s -> Board
getBoard (ValidatedBoard b) = b

getGenMove :: LegalMove -> MoveGen.GenMove
getGenMove (LegalMove gm) = gm

class MoveGenerator (s :: CheckStatus) where
    legalMovesValidated :: ValidatedBoard s -> [LegalMove]
    captureMovesValidated :: ValidatedBoard s -> [LegalMove]
    legalQuietsValidated :: ValidatedBoard s -> [LegalMove]
    legalPromotionsValidated :: ValidatedBoard s -> [LegalMove]

instance MoveGenerator 'InCheck where
    legalMovesValidated (ValidatedBoard (Board b _)) = map LegalMove $ U.toList $ MoveGen.generateEvasions b
    captureMovesValidated (ValidatedBoard (Board b _)) = map LegalMove $ U.toList $ MoveGen.generateEvasionCaptures b
    legalQuietsValidated (ValidatedBoard (Board b _)) = map LegalMove $ U.toList $ MoveGen.generateEvasionQuiets b
    legalPromotionsValidated (ValidatedBoard (Board b _)) = map LegalMove $ U.toList $ MoveGen.generateEvasionPromotions b

instance MoveGenerator 'NotInCheck where
    legalMovesValidated (ValidatedBoard (Board b _)) = map LegalMove $ U.toList $ MoveGen.pseudoLegalMoves b
    captureMovesValidated (ValidatedBoard (Board b _)) = map LegalMove $ U.toList $ MoveGen.pseudoLegalCaptures b
    legalQuietsValidated (ValidatedBoard (Board b _)) = map LegalMove $ U.toList $ MoveGen.pseudoLegalQuiets b
    legalPromotionsValidated (ValidatedBoard (Board b _)) = map LegalMove $ U.toList $ MoveGen.pseudoLegalPromotions b

mkLegalMove :: MoveGen.GenMove -> LegalMove
mkLegalMove = LegalMove

toGenMove :: Board -> Move -> Maybe MoveGen.GenMove
toGenMove (Board b _) m = MoveGen.toGenMove b m

isLegalMove :: Board -> Move -> Bool
isLegalMove (Board b _) m = MoveGen.isLegalMove b m

applyLegalMove :: ValidatedBoard s -> LegalMove -> SomeValidatedBoard
applyLegalMove (ValidatedBoard b) (LegalMove gm) =
    let b' = applyGenMove b gm
    in if MoveGen.givesCheck (pieces b) gm
       then InCheckBoard (ValidatedBoard b')
       else NotInCheckBoard (ValidatedBoard b')

-- Safe Accessors for LegalMove

moveFrom :: LegalMove -> Square
moveFrom (LegalMove (MoveGen.GenQuiet f _ _)) = f
moveFrom (LegalMove (MoveGen.GenCapture f _ _ _)) = f
moveFrom (LegalMove (MoveGen.GenEnPassant f _)) = f
moveFrom (LegalMove (MoveGen.GenCastling f _)) = f
moveFrom (LegalMove (MoveGen.GenPromotion f _ _)) = f
moveFrom (LegalMove (MoveGen.GenPromotionCapture f _ _ _)) = f

moveTo :: LegalMove -> Square
moveTo (LegalMove (MoveGen.GenQuiet _ t _)) = t
moveTo (LegalMove (MoveGen.GenCapture _ t _ _)) = t
moveTo (LegalMove (MoveGen.GenEnPassant _ t)) = t
moveTo (LegalMove (MoveGen.GenCastling _ t)) = t
moveTo (LegalMove (MoveGen.GenPromotion _ t _)) = t
moveTo (LegalMove (MoveGen.GenPromotionCapture _ t _ _)) = t

movePromotion :: LegalMove -> Maybe PieceType
movePromotion (LegalMove (MoveGen.GenPromotion _ _ p)) = Just p
movePromotion (LegalMove (MoveGen.GenPromotionCapture _ _ p _)) = Just p
movePromotion _ = Nothing

isCapture :: LegalMove -> Bool
isCapture (LegalMove (MoveGen.GenCapture {})) = True
isCapture (LegalMove (MoveGen.GenPromotionCapture {})) = True
isCapture (LegalMove (MoveGen.GenEnPassant {})) = True
isCapture _ = False

isPromotion :: LegalMove -> Bool
isPromotion (LegalMove (MoveGen.GenPromotion {})) = True
isPromotion (LegalMove (MoveGen.GenPromotionCapture {})) = True
isPromotion _ = False

-- Helpers

midSquare :: Square -> Square -> Square
midSquare f t = Square ((unSquare f + unSquare t) `div` 2)
