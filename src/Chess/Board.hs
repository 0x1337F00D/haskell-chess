{-# LANGUAGE PatternSynonyms #-}
module Chess.Board
  ( -- * The Board Type
    Board(..)
  , initialBoard
    -- * Game Logic
  , applyMove
  , legalMoves
  , pseudoLegalMoves
  , captureMoves
  , isCheck
  , isCheckmate
  , isStalemate
  , hasInsufficientMaterial
  , outcome
    -- * Notation
  , fen
  , parseFen
  , san
  , parseSan
  , uci
  , fromUci
    -- * Re-exports
  , module Chess.Types
  ) where

import Data.Maybe (isJust)
import Data.Bits (testBit)

import Chess.Types
import qualified Chess.Board.Base as Base
import qualified Chess.Board.GameState as GS
import qualified Chess.Board.MoveGen as MoveGen
import qualified Chess.Board.Validation as Val
import qualified Chess.Board.Fen as Fen
import qualified Chess.Board.San as San
import qualified Chess.Board.Uci as Uci

-- | The primary board type combining piece placement and game state.
data Board = Board
  { pieces :: !Base.Board
  , state  :: !GS.GameState
  , history :: ![Val.PositionRep]
  } deriving (Eq, Show)

-- | The standard initial chess position.
initialBoard :: Board
initialBoard =
  case Fen.parseFen startingFEN of
    Just (b, gs) -> Board b gs []
    Nothing      -> error "Internal error: Failed to parse starting FEN"

-- | Apply a move to the board, updating pieces and game state (counters, rights, etc).
applyMove :: Board -> Move -> Board
applyMove (Board b gs hist) m@(Move from to _) =
    let c = GS.turn gs
        fromI = unSquare from
    in if not (testBit (Base.occupiedBy b c) fromI)
       then Board b gs hist -- Invalid move (empty or wrong color)
       else
        let
            -- Resolve pieces efficiently (avoiding pieceAt lookups)
            pt = Base.findPieceType b c from
            toI = unSquare to
            capturedPt = if testBit (Base.occupiedTotal b) toI
                         then Just (Base.findPieceType b (Base.oppositeColor c) to)
                         else Nothing

            -- 1. Update pieces (using fast path)
            b' = MoveGen.applyMoveBoardFast b gs m pt capturedPt

            -- 2. Analyze move for state updates using resolved pieces
            isPawn = pt == Pawn
            isEp = isPawn && capturedPt == Nothing && squareFile from /= squareFile to
            isCap = isJust capturedPt || isEp

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
            gs2 = case capturedPt of
                    Just Rook -> GS.removeCastlingRight gs1 to
                    _ -> gs1

            -- En Passant square
            -- Set if pawn double push
            ep' = if isPawn && abs (squareRank from - squareRank to) == 2
                  then Just (midSquare from to)
                  else Nothing

            nextTurn = Base.oppositeColor c

            -- 0. Update history
            posRep = Val.PositionRep b c (GS.castlingRights gs) (GS.epSquare gs)
            hist' = posRep : hist

            -- Optimization: Clear history if halfmove clock resets (pawn move or capture)
            histFinal = if halfmove' == 0 then [] else hist'

        in Board b' (gs2 { GS.turn = nextTurn
                         , GS.epSquare = ep'
                         , GS.halfmoveClock = halfmove'
                         , GS.fullmoveNumber = fullmove'
                         }) histFinal
applyMove b NullMove = b
applyMove b _ = b

-- | Generate all legal moves for the current position.
legalMoves :: Board -> [Move]
legalMoves (Board b gs _) = MoveGen.legalMoves b gs

-- | Generate all pseudo-legal moves.
pseudoLegalMoves :: Board -> [Move]
pseudoLegalMoves (Board b gs _) = map (\(MoveGen.GenMove m _ _) -> m) $ MoveGen.pseudoLegalMoves b gs

-- | Generate all legal capture moves.
captureMoves :: Board -> [Move]
captureMoves (Board b gs _) = MoveGen.legalCaptures b gs

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

-- Helpers

midSquare :: Square -> Square -> Square
midSquare f t = Square ((unSquare f + unSquare t) `div` 2)
