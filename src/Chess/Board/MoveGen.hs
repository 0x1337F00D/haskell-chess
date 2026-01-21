{-# LANGUAGE PatternSynonyms #-}
module Chess.Board.MoveGen where

import Data.Bits

import Chess.Types
import Chess.Bitboard
import Chess.Board.Base
import Chess.Board.GameState

-- | Generate all pseudo-legal moves for the side to move.
-- Pseudo-legal means moves that follow piece movement rules and capture rules,
-- but do not necessarily respect the rule that the king must not be in check.
pseudoLegalMoves :: Board -> GameState -> [Move]
pseudoLegalMoves b gs = concat
    [ pawnMoves b gs
    , pieceMoves b gs Knight
    , pieceMoves b gs Bishop
    , pieceMoves b gs Rook
    , pieceMoves b gs Queen
    , pieceMoves b gs King
    , castlingMoves b gs
    ]

-- | Generate all legal moves.
-- Filters pseudo-legal moves to ensure the king is not left in check.
legalMoves :: Board -> GameState -> [Move]
legalMoves b gs = filter (isLegal b gs) (pseudoLegalMoves b gs)

-- | Check if a move is legal (does not leave own king in check).
-- Note: This function assumes the move is pseudo-legal.
isLegal :: Board -> GameState -> Move -> Bool
isLegal b gs m =
    let c = turn gs
        b' = applyMoveBoard b gs m
        kingSq = kingSquare b' c
    in case kingSq of
        Nothing -> False -- Should not happen if king exists
        Just k -> not (isAttackedBy b' (oppositeColor c) k) && castlingSafe b gs m

    where
         -- Special check for castling: path must not be attacked.
         -- The standard isAttackedBy check on the final position handles the "into check" part.
         -- But we also need to check "out of check" and "through check".
         -- "Out of check" is covered by the standard check (start position checked, final position checked).
         -- Wait, if we are in check, we can't castle.
         -- And we can't cross attacked squares.
         castlingSafe :: Board -> GameState -> Move -> Bool
         castlingSafe _ _ (Move f t _ )
            | isCastlingMove f t =
                let c = turn gs
                    step = (unSquare t - unSquare f) `div` 2
                    mid = Square (unSquare f + step)
                    -- Check if current square is attacked (can't castle out of check)
                    startAttacked = isAttackedBy b (oppositeColor c) f
                    -- Check if passed square is attacked (can't castle through check)
                    midAttacked = isAttackedBy b (oppositeColor c) mid
                in not startAttacked && not midAttacked
            | otherwise = True
         castlingSafe _ _ _ = True

         isCastlingMove f t =
             let d = abs (unSquare f - unSquare t)
                 p = pieceAt b f
             in d == 2 && fmap pieceType p == Just King

-- | Apply a move to the board (without updating game state like counters).
-- Handles en passant capture removal and castling rook moves.
applyMoveBoard :: Board -> GameState -> Move -> Board
applyMoveBoard b gs (Move from to promo) =
    let p = pieceAt b from
        c = turn gs
    in case p of
        Nothing -> b -- Should not happen
        Just (Piece _ pt) ->
            let
                -- Handle Basic Move and Promotion
                bAfterMove =
                    case promo of
                        Just ppt ->
                            -- If promotion:
                            -- 1. Remove pawn from 'from'
                            -- 2. Remove potential capture at 'to'
                            -- 3. Put promoted piece at 'to'
                            let b1 = unsafeRemovePiece b from c Pawn
                                captured = if testBit (occupiedTotal b) (unSquare to) then pieceAt b to else Nothing
                                b2 = case captured of
                                       Nothing -> b1
                                       Just (Piece capC capPt) -> unsafeRemovePiece b1 to capC capPt
                                newPiece = Piece c ppt
                            in unsafePutPiece b2 to newPiece
                        Nothing ->
                            -- Normal move: use optimized movePiece
                            movePiece b from to c pt

                -- Handle En Passant capture
                -- If pawn moves diagonally to empty square, it's EP.
                isEP = pt == Pawn && squareFile from /= squareFile to && not (testBit (occupiedTotal b) (unSquare to))

                bAfterEP = if isEP
                           then let capSq = Square (unSquare to + (if c == White then -8 else 8))
                                    -- Capture is opposite color Pawn
                                in unsafeRemovePiece bAfterMove capSq (oppositeColor c) Pawn
                           else bAfterMove

                -- Handle Castling (move rook)
                isCastling = pt == King && abs (unSquare from - unSquare to) == 2
                bFinal = if isCastling
                         then let (rookFrom, rookTo) = castlingRookMove from to
                                  -- Rook is same color, Rook.
                                  -- Move rook from rookFrom to rookTo.
                              in movePiece bAfterEP rookFrom rookTo c Rook
                         else bAfterEP

            in bFinal
applyMoveBoard b _ NullMove = b

-- Helper to determine rook move for castling
castlingRookMove :: Square -> Square -> (Square, Square)
castlingRookMove kingFrom kingTo
    | kingTo > kingFrom = (H1 `relativeTo` kingFrom, F1 `relativeTo` kingFrom) -- Kingside
    | otherwise         = (A1 `relativeTo` kingFrom, D1 `relativeTo` kingFrom) -- Queenside
  where
    relativeTo (Square i) (Square k) =
        let rankOffset = (k `div` 8) * 8
            fileOffset = i `mod` 8
        in Square (rankOffset + fileOffset)

-- | Get the square of the king of a given color.
kingSquare :: Board -> Color -> Maybe Square
kingSquare b c = fmap Square (lsb (pieceBitboard b c King))

-- Move Generators ------------------------------------------------------------

pieceMoves :: Board -> GameState -> PieceType -> [Move]
pieceMoves b gs pt = concatMap genMoves sqs
  where
    c = turn gs
    bb = pieceBitboard b c pt
    sqs = map Square (scanForward bb)

    genMoves :: Square -> [Move]
    genMoves from =
        let att = attacks b from
            -- For non-pawns, valid moves are attacks on empty or enemy squares.
            -- attacks() already handles blocking for sliding pieces.
            -- We just need to exclude own pieces.
            valid = att .&. complement (occupiedBy b c)
            toSquares = map Square (scanForward valid)
        in [ Move from to Nothing | to <- toSquares ]

pawnMoves :: Board -> GameState -> [Move]
pawnMoves b gs = concatMap genPawnMoves sqs
  where
    c = turn gs
    bb = pieceBitboard b c Pawn
    sqs = map Square (scanForward bb)

    genPawnMoves :: Square -> [Move]
    genPawnMoves from = pushes ++ captures
      where
        pushes =
            let fwd = if c == White then 8 else -8
                to1 = Square (unSquare from + fwd)
                isPromRank s = (c == White && squareRank s == 7) || (c == Black && squareRank s == 0)

                singlePush =
                    if pieceAt b to1 == Nothing
                    then if isPromRank to1
                         then [ Move from to1 (Just p) | p <- [Queen, Rook, Bishop, Knight] ]
                         else [ Move from to1 Nothing ]
                    else []

                doublePush =
                    let to2 = Square (unSquare to1 + fwd)
                        startRank = if c == White then 1 else 6
                    in if squareRank from == startRank && pieceAt b to1 == Nothing && pieceAt b to2 == Nothing
                       then [ Move from to2 Nothing ]
                       else []
            in singlePush ++ doublePush

        captures =
            let att = pawnAttacks c from
                -- Normal captures: enemy pieces
                enemy = occupiedBy b (oppositeColor c)
                validMatches = att .&. enemy

                -- En Passant
                epMatch = case epSquare gs of
                            Just epSq -> if testBit att (unSquare epSq) then bbFromSquare epSq else 0
                            Nothing -> 0

                valid = validMatches .|. epMatch
                toSquares = map Square (scanForward valid)

                mkMove to =
                    if (c == White && squareRank to == 7) || (c == Black && squareRank to == 0)
                    then [ Move from to (Just p) | p <- [Queen, Rook, Bishop, Knight] ]
                    else [ Move from to Nothing ]

            in concatMap mkMove toSquares

castlingMoves :: Board -> GameState -> [Move]
castlingMoves b gs = ks ++ qs
  where
    c = turn gs
    ks = if canCastleKingside gs c && kingsideClear then [mkCastlingMove True] else []
    qs = if canCastleQueenside gs c && queensideClear then [mkCastlingMove False] else []

    rank = if c == White then 0 else 7
    kingSq = Square (rank * 8 + 4) -- E1/E8

    mkCastlingMove isKingside =
        let toFile = if isKingside then 6 else 2 -- G1/G8 or C1/C8
            toSq = Square (rank * 8 + toFile)
        in Move kingSq toSq Nothing

    kingsideClear =
        let f1 = Square (rank * 8 + 5) -- F
            g1 = Square (rank * 8 + 6) -- G
        in pieceAt b f1 == Nothing && pieceAt b g1 == Nothing

    queensideClear =
        let d1 = Square (rank * 8 + 3) -- D
            c1 = Square (rank * 8 + 2) -- C
            b1 = Square (rank * 8 + 1) -- B
        in pieceAt b d1 == Nothing && pieceAt b c1 == Nothing && pieceAt b b1 == Nothing
