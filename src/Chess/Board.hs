{-# LANGUAGE PatternSynonyms #-}
module Chess.Board
  ( -- * The Board Type
    Board(..)
  , initialBoard
    -- * Game Logic
  , applyMove
  , applyGenMove
  , legalMoves
  , legalGenMoves
  , legalGenMovesVector
  , pseudoLegalMoves
  , captureMoves
  , captureGenMoves
  , legalGenQuiets
  , legalGenPromotions
  , pseudoLegalQuiets
  , pseudoLegalPromotions
  , isCheck
  , isCheckmate
  , isStalemate
  , hasInsufficientMaterial
  , outcome
    -- * Safe Interface
  , ValidatedBoard
  , LegalMove
  , trustBoard
  , getBoard
  , getGenMove
  , legalMovesValidated
  , captureMovesValidated
  , legalQuietsValidated
  , legalPromotionsValidated
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
  , state  :: {-# UNPACK #-} !GS.GameState
  , history :: ![Word64]
  } deriving (Eq, Show)

-- | The standard initial chess position.
initialBoard :: Board
initialBoard =
  case Fen.parseFen startingFEN of
    Just (b, gs) -> Board b gs []
    Nothing      -> error "Internal error: Failed to parse starting FEN"

-- | Apply a move to the board, updating pieces and game state (counters, rights, etc).
applyMove :: Board -> Move -> Board
applyMove board@(Board b gs _) (Move from to promo) =
    let c = GS.turn gs
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

-- | Helper to apply move logic given resolved pieces.
{-# INLINE applyMoveHelper #-}
applyMoveHelper :: Board -> MoveGen.GenMove -> Board
applyMoveHelper (Board b gs hist) gm =
    let
        -- 1. Update pieces (using fast path)
        b' = MoveGen.applyMoveBoardFast b gs gm

        c = GS.turn gs
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
        h0 = GS.zobristHash gs
             `xor` Zobrist.zobristEp (GS.epSquare gs)           -- Remove old EP
             `xor` Zobrist.zobristCastling (GS.castlingRights gs) -- Remove old CR
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

        -- Halfmove clock: Reset on pawn move or capture
        halfmove' = if isPawn || isCap then 0 else GS.halfmoveClock gs + 1

        -- Fullmove number: Increment after Black's move
        fullmove' = if c == Black then GS.fullmoveNumber gs + 1 else GS.fullmoveNumber gs

        -- Castling rights updates
        -- If moving King, lose castling rights for that color
        gs1 = case pt of
                King -> GS.removeColorCastlingRights gs c
                Rook -> GS.removeCastlingRight gs from
                _ -> gs

        -- If capturing Rook, lose castling rights for that rook's square
        gs2 = case captured of
                Just Rook ->
                    -- For EP, capture is Pawn, so this won't trigger.
                    -- Normal capture of Rook triggers.
                    GS.removeCastlingRight gs1 to
                _ -> gs1

        -- En Passant square
        -- Set if pawn double push
        ep' = if isPawn && abs (squareRank from - squareRank to) == 2
              then midSquare from to
              else NoSquare

        -- Finalize Hash
        hFinal = h4 `xor` Zobrist.zobristCastling (GS.castlingRights gs2)
                    `xor` Zobrist.zobristEp ep'

        nextTurn = oppC

        -- 0. Update history
        -- Store the Zobrist hash of the previous position (gs)
        hist' = GS.zobristHash gs : hist

        -- Optimization: Clear history if halfmove clock resets (pawn move or capture)
        histFinal = if halfmove' == 0 then [] else hist'

        gs3 = GS.setTurn gs2 nextTurn
        gs4 = GS.setEpSquare gs3 ep'
        gs5 = GS.setHalfmoveClock gs4 halfmove'
        gs6 = GS.setFullmoveNumber gs5 fullmove'
        gsFinal = GS.setZobristHash gs6 hFinal

    in Board b' gsFinal histFinal

-- | Generate all legal moves for the current position.
legalMoves :: Board -> [Move]
legalMoves (Board b gs _) = MoveGen.legalMoves b gs

-- | Generate all legal moves preserving piece info.
legalGenMoves :: Board -> [MoveGen.GenMove]
legalGenMoves (Board b gs _) = U.toList $ MoveGen.legalGenMoves b gs

-- | Generate all legal moves as an unboxed vector.
legalGenMovesVector :: Board -> U.Vector MoveGen.GenMove
legalGenMovesVector (Board b gs _) = MoveGen.legalGenMoves b gs

-- | Generate all pseudo-legal moves.
pseudoLegalMoves :: Board -> [Move]
pseudoLegalMoves (Board b gs _) = map MoveGen.genMoveToMove $ U.toList $ MoveGen.pseudoLegalMoves b gs

-- | Generate all legal capture moves.
captureMoves :: Board -> [Move]
captureMoves (Board b gs _) = MoveGen.legalCaptures b gs

-- | Generate all legal capture moves preserving piece info.
captureGenMoves :: Board -> [MoveGen.GenMove]
captureGenMoves (Board b gs _) = U.toList $ MoveGen.legalGenCaptures b gs

-- | Generate all legal quiet moves preserving piece info.
legalGenQuiets :: Board -> [MoveGen.GenMove]
legalGenQuiets (Board b gs _) = U.toList $ MoveGen.legalGenQuiets b gs

-- | Generate all legal promotion moves preserving piece info.
legalGenPromotions :: Board -> [MoveGen.GenMove]
legalGenPromotions (Board b gs _) = U.toList $ MoveGen.legalGenPromotions b gs

-- | Generate all pseudo-legal quiet moves.
pseudoLegalQuiets :: Board -> [Move]
pseudoLegalQuiets (Board b gs _) = map MoveGen.genMoveToMove $ U.toList $ MoveGen.pseudoLegalQuiets b gs

-- | Generate all pseudo-legal promotion moves.
pseudoLegalPromotions :: Board -> [Move]
pseudoLegalPromotions (Board b gs _) = map MoveGen.genMoveToMove $ U.toList $ MoveGen.pseudoLegalPromotions b gs

-- | Check if the side to move is in check.
isCheck :: Board -> Bool
isCheck (Board b gs _) = Val.isCheck b gs

-- | Check if the side to move is checkmated.
isCheckmate :: Board -> Bool
isCheckmate (Board b gs _) = Val.isCheckmate b gs

-- | Check if the game is in stalemate.
isStalemate :: Board -> Bool
isStalemate (Board b gs _) = Val.isStalemate b gs

-- | Check if the game is drawn by insufficient material.
hasInsufficientMaterial :: Board -> Bool
hasInsufficientMaterial (Board b _ _) = Val.hasInsufficientMaterial b

-- | Determine the outcome of the game, if ended.
outcome :: Board -> Maybe Outcome
outcome (Board b gs h) = Val.outcome b gs h

-- | Convert board to FEN string.
fen :: Board -> String
fen (Board b gs _) = Fen.fen b gs

-- | Parse FEN string to Board.
parseFen :: String -> Maybe Board
parseFen s = case Fen.parseFen s of
    Just (b, gs) -> Just (Board b gs [])
    Nothing      -> Nothing

-- | Convert move to SAN.
san :: Board -> Move -> String
san (Board b gs _) m = San.san b gs m

-- | Parse SAN string to Move.
parseSan :: Board -> String -> Maybe Move
parseSan (Board b gs _) s = San.parseSan b gs s

-- | Convert move to UCI.
uci :: Move -> String
uci = Uci.uci

-- | Parse UCI string to Move.
fromUci :: String -> Maybe Move
fromUci = Uci.fromUci

-- Safe Interface

newtype ValidatedBoard = ValidatedBoard Board deriving (Eq, Show)
newtype LegalMove = LegalMove MoveGen.GenMove deriving (Eq, Show)

trustBoard :: Board -> ValidatedBoard
trustBoard = ValidatedBoard

getBoard :: ValidatedBoard -> Board
getBoard (ValidatedBoard b) = b

getGenMove :: LegalMove -> MoveGen.GenMove
getGenMove (LegalMove gm) = gm

legalMovesValidated :: ValidatedBoard -> [LegalMove]
legalMovesValidated (ValidatedBoard b) = map LegalMove (legalGenMoves b)

captureMovesValidated :: ValidatedBoard -> [LegalMove]
captureMovesValidated (ValidatedBoard b) = map LegalMove (captureGenMoves b)

legalQuietsValidated :: ValidatedBoard -> [LegalMove]
legalQuietsValidated (ValidatedBoard b) = map LegalMove (legalGenQuiets b)

legalPromotionsValidated :: ValidatedBoard -> [LegalMove]
legalPromotionsValidated (ValidatedBoard b) = map LegalMove (legalGenPromotions b)

mkLegalMove :: MoveGen.GenMove -> LegalMove
mkLegalMove = LegalMove

toGenMove :: Board -> Move -> Maybe MoveGen.GenMove
toGenMove (Board b gs _) m = MoveGen.toGenMove b gs m

isLegalMove :: Board -> Move -> Bool
isLegalMove (Board b gs _) m = MoveGen.isLegalMove b gs m

applyLegalMove :: ValidatedBoard -> LegalMove -> ValidatedBoard
applyLegalMove (ValidatedBoard b) (LegalMove gm) = ValidatedBoard (applyGenMove b gm)

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
