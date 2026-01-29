{-# LANGUAGE BangPatterns #-}
module Chess.Core.FastPerft where

import Data.Bits ((.&.), (.|.), complement, testBit)

import Chess.Types
import Chess.Bitboard
import Chess.Board.Base
import Chess.Board.GameState
import Chess.Board.MoveGen

-- | High-performance perft function that operates directly on Board and GameState.
-- Bypasses the overhead of ActiveGame and Move/Transition types.
fastPerft :: Depth -> Board -> GameState -> Int
fastPerft 0 _ _ = 1
fastPerft 1 b gs = length (legalGenMoves b gs)
fastPerft d b gs = sum (map loop (legalGenMoves b gs))
  where
    loop :: GenMove -> Int
    loop gm =
        let !b' = applyMoveBoardFast b gs gm
            !gs' = updateGameState gs gm
        in fastPerft (d - 1) b' gs'

-- | Update GameState based on a GenMove.
-- This mirrors the logic in Chess.Core.Rules.Common.genericApplyMove but optimized for GenMove.
updateGameState :: GameState -> GenMove -> GameState
updateGameState gs gm =
    let
        c = turn gs
        nextC = oppositeColor c

        -- Update Castling Rights
        cr = castlingRights gs
        cr1 = case gm of
                GenCastling _ _ -> cr -- Castling itself uses rights, implicitly handled by check below or just kept (logic usually removes rights on King move)
                _ -> cr

        -- Extract from/to
        (from, to) = case gm of
            GenQuiet f t _ -> (f, t)
            GenCapture f t _ _ -> (f, t)
            GenEnPassant f t -> (f, t)
            GenCastling f t -> (f, t)
            GenPromotion f t _ -> (f, t)
            GenPromotionCapture f t _ _ -> (f, t)

        -- Check for King Move (including Castling)
        isKingMove = case gm of
            GenQuiet _ _ King -> True
            GenCapture _ _ King _ -> True
            GenCastling _ _ -> True
            _ -> False

        cr2 = if isKingMove
              then if c == White
                   then cr1 .&. complement (BB_A1 .|. BB_H1)
                   else cr1 .&. complement (BB_A8 .|. BB_H8)
              else cr1

        -- Check for Rook Move or Capture

        updateRookRights rights sq = rights .&. complement (bbFromSquare sq)

        -- If piece moving is Rook, remove rights for 'from'
        isRookMove = case gm of
            GenQuiet _ _ Rook -> True
            GenCapture _ _ Rook _ -> True
            _ -> False

        cr3 = if isRookMove then updateRookRights cr2 from else cr2

        -- If piece captured is Rook, remove rights for 'to'
        isRookCapture = case gm of
            GenCapture _ _ _ Rook -> True
            GenPromotionCapture _ _ _ Rook -> True
            _ -> False

        cr4 = if isRookCapture then updateRookRights cr3 to else cr3

        -- Update En Passant Target
        newEP = case gm of
            GenQuiet f t Pawn ->
                let diff = abs (unSquare f - unSquare t)
                in if diff == 16 then Just (squareFile f) else Nothing
            _ -> Nothing

        -- Update Clocks
        isPawn = case gm of
            GenQuiet _ _ Pawn -> True
            GenCapture _ _ Pawn _ -> True
            GenEnPassant _ _ -> True
            GenPromotion _ _ _ -> True
            GenPromotionCapture _ _ _ _ -> True
            _ -> False

        isCapture = case gm of
            GenCapture {} -> True
            GenPromotionCapture {} -> True
            GenEnPassant {} -> True
            _ -> False

        newHMC = if isPawn || isCapture then 0 else halfmoveClock gs + 1
        newFMN = fullmoveNumber gs + (if c == Black then 1 else 0)

    in GameState
        { turn = nextC
        , castlingRights = cr4
        , epSquare = case newEP of
                       Just fIdx ->
                           -- Convert File Index (0-7) to Square for EP target
                           -- If White moved (Rank 2->4), target is Rank 3 (index 2).
                           -- If Black moved (Rank 7->5), target is Rank 6 (index 5).
                           -- 2*8 = 16, 5*8 = 40
                           let rankOffset = if c == White then 16 else 40
                           in Just (Square (fIdx + rankOffset))
                       Nothing -> Nothing
        , halfmoveClock = newHMC
        , fullmoveNumber = newFMN
        , zobristHash = 0 -- Not updating hash for perft
        }
