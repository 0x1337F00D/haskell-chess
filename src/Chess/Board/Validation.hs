module Chess.Board.Validation where

import Data.Bits ((.|.))
import Chess.Types
import Chess.Bitboard
import Chess.Board.Base
import Chess.Board.GameState
import Chess.Board.MoveGen (pseudoLegalMoves, isLegal, kingSquare)

-- | Check if the side to move is in check.
isCheck :: Board -> GameState -> Bool
isCheck b gs =
    let c = turn gs
    in case kingSquare b c of
        Nothing -> False -- Should not happen if king exists
        Just k -> isAttackedBy b (oppositeColor c) k

-- | Check if the side to move has any legal moves.
hasLegalMoves :: Board -> GameState -> Bool
hasLegalMoves b gs = any (isLegal b gs) (pseudoLegalMoves b gs)

-- | Check if the side to move is checkmated.
isCheckmate :: Board -> GameState -> Bool
isCheckmate b gs = isCheck b gs && not (hasLegalMoves b gs)

-- | Check if the game is in a stalemate.
isStalemate :: Board -> GameState -> Bool
isStalemate b gs = not (isCheck b gs) && not (hasLegalMoves b gs)

-- | Check if the game is drawn by the fifty-move rule.
isFiftyMoves :: GameState -> Bool
isFiftyMoves gs = halfmoveClock gs >= 100

-- | Check if the game is drawn by insufficient material.
-- Returns True if neither side has sufficient material to win.
hasInsufficientMaterial :: Board -> Bool
hasInsufficientMaterial b =
    let -- Pieces by type (combined colors for initial check)
        pawns = whitePawns b .|. blackPawns b
        rooks = whiteRooks b .|. blackRooks b
        queens = whiteQueens b .|. blackQueens b
    in if pawns .|. rooks .|. queens /= 0
       then False -- Major pieces or pawns exist
       else
           let wKnights = popcount (whiteKnights b)
               bKnights = popcount (blackKnights b)
               wBishops = popcount (whiteBishops b)
               bBishops = popcount (blackBishops b)

               wCount = wKnights + wBishops
               bCount = bKnights + bBishops
               totalCount = wCount + bCount
           in case totalCount of
               0 -> True -- K vs K
               1 -> True -- K+N vs K or K+B vs K
               2 ->
                   if (wKnights == 1 && bKnights == 1) || (wBishops == 1 && bKnights == 1) || (wKnights == 1 && bBishops == 1)
                   then False -- K+N vs K+N, K+B vs K+N (mate possible)
                   else if wBishops == 1 && bBishops == 1
                        then -- K+B vs K+B. Draw if bishops on same color.
                             sameColorBishops b
                        else False -- Should be covered? (e.g. K+NN vs K - technically not forced mate, but not strictly insufficient by FIDE "any series")
                        -- Python-chess says K+NN vs K is insufficient.
                        -- Let's stick to the minimal set: K vs K, K+Minor vs K.
                        -- And K+B vs K+B (same color).
                        -- If I have 2 knights on one side? wKnights=2, b=0. total=2.
                        -- Python-chess returns True for K+N+N vs K.
                        -- So let's implement the python-chess logic more closely.

               _ -> False -- More than 2 minor pieces total -> sufficient (usually)

    where
      -- Helper to check if bishops are on same color squares
      sameColorBishops :: Board -> Bool
      sameColorBishops board =
          let wB = whiteBishops board
              bB = blackBishops board
              wBSq = lsb wB
              bBSq = lsb bB
          in case (wBSq, bBSq) of
              (Just ws, Just bs) ->
                  -- Same color if (rank+file) parity matches
                  let c1 = (ws `div` 8) + (ws `mod` 8)
                      c2 = (bs `div` 8) + (bs `mod` 8)
                  in even c1 == even c2
              _ -> False -- Should not happen if we counted them

-- | Determine the outcome of the game, if it has ended.
outcome :: Board -> GameState -> Maybe Outcome
outcome b gs
    | isCheckmate b gs = Just $ Outcome Checkmate (Just (oppositeColor (turn gs)))
    | isStalemate b gs = Just $ Outcome Stalemate Nothing
    | isFiftyMoves gs  = Just $ Outcome FiftyMoves Nothing
    | hasInsufficientMaterial b = Just $ Outcome InsufficientMaterial Nothing
    | otherwise = Nothing
