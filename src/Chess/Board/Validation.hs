module Chess.Board.Validation where

import Data.Bits ((.|.))
import Data.Word (Word64)
import qualified Data.Vector.Unboxed as U
import Chess.Types
import Chess.Bitboard
import Chess.Board.Base
import Chess.Board.GameState
import Chess.Board.MoveGen (pseudoLegalMoves, isLegal, kingSquare, hasLegalMove)

-- | Check if the side to move is in check.
isCheck :: Board -> GameState -> Bool
isCheck b gs =
    let c = turn gs
    in case kingSquare b c of
        Nothing -> False -- Should not happen if king exists
        Just k -> isAttackedBy b (oppositeColor c) k

-- | Check if the side to move has any legal moves.
hasLegalMoves :: Board -> GameState -> Bool
hasLegalMoves = hasLegalMove

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
                   if (wKnights == 1 && bKnights == 1)
                   then False -- K+N vs K+N (Sufficient)
                   else if wBishops == 1 && bBishops == 1
                        then -- K+B vs K+B. Draw if bishops on same color.
                             sameColorBishops b
                        else if (wKnights == 1 && bBishops == 1) || (wBishops == 1 && bKnights == 1)
                             then True -- K+N vs K+B (Insufficient). Matches pychess behavior.
                             else False -- K+NN, K+BB, K+NB vs K are Sufficient

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

-- | Check if the current position has occurred at least 3 times.
isThreefoldRepetition :: Board -> GameState -> [Word64] -> Bool
isThreefoldRepetition _ gs history =
    let currentHash = zobristHash gs
        -- Count occurrences of currentHash in history.
        -- We need 2 past occurrences + current = 3.
        count = length (filter (== currentHash) history)
    in count >= 2

-- | Determine the outcome of the game, if it has ended.
outcome :: Board -> GameState -> [Word64] -> Maybe Outcome
outcome b gs history
    | isCheckmate b gs = Just $ Outcome Checkmate (Just (oppositeColor (turn gs)))
    | isStalemate b gs = Just $ Outcome Stalemate Nothing
    | isThreefoldRepetition b gs history = Just $ Outcome ThreefoldRepetition Nothing
    | isFiftyMoves gs  = Just $ Outcome FiftyMoves Nothing
    | hasInsufficientMaterial b = Just $ Outcome InsufficientMaterial Nothing
    | otherwise = Nothing
