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
pseudoLegalMoves :: Board -> GameState -> [GenMove]
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
legalMoves b gs = map (\(GenMove m _ _) -> m) $ filter (isLegal b gs) (pseudoLegalMoves b gs)

-- | Check if a move is legal (does not leave own king in check).
-- Note: This function assumes the move is pseudo-legal.
isLegal :: Board -> GameState -> GenMove -> Bool
isLegal b gs (GenMove m pt cap) =
    let c = turn gs
        b' = applyMoveBoardFast b gs m pt cap
        kingSq = kingSquare b' c
    in case kingSq of
        Nothing -> False -- Should not happen if king exists
        Just k -> not (isAttackedBy b' (oppositeColor c) k) && castlingSafe b gs m pt

    where
         -- Special check for castling: path must not be attacked.
         -- The standard isAttackedBy check on the final position handles the "into check" part.
         -- But we also need to check "out of check" and "through check".
         -- "Out of check" is covered by the standard check (start position checked, final position checked).
         -- Wait, if we are in check, we can't castle.
         -- And we can't cross attacked squares.
         castlingSafe :: Board -> GameState -> Move -> PieceType -> Bool
         castlingSafe _ _ (Move f t _ ) movingPt
            | isCastlingMove f t movingPt =
                let c = turn gs
                    step = (unSquare t - unSquare f) `div` 2
                    mid = Square (unSquare f + step)
                    -- Check if current square is attacked (can't castle out of check)
                    startAttacked = isAttackedBy b (oppositeColor c) f
                    -- Check if passed square is attacked (can't castle through check)
                    midAttacked = isAttackedBy b (oppositeColor c) mid
                in not startAttacked && not midAttacked
            | otherwise = True
         castlingSafe _ _ _ _ = True

         isCastlingMove f t movingPt =
             let d = abs (unSquare f - unSquare t)
             in d == 2 && movingPt == King

-- | A move coupled with the piece type moving and optionally the piece type captured.
-- If captured is Nothing, it's a quiet move (except possibly EP, which is handled specially).
-- For EP, this field is ignored (or implicitly Pawn).
data GenMove = GenMove !Move !PieceType !(Maybe PieceType)

-- | Faster version of applyMoveBoard that avoids pieceAt lookups by using provided piece info.
applyMoveBoardFast :: Board -> GameState -> Move -> PieceType -> Maybe PieceType -> Board
applyMoveBoardFast b gs (Move from to promo) pt capturedPt =
    let c = turn gs
    in
        -- Handle Basic Move and Promotion
        let bAfterMove =
                case promo of
                    Just ppt ->
                        -- If promotion:
                        -- 1. Remove pawn from 'from'
                        -- 2. Remove potential capture at 'to'
                        -- 3. Put promoted piece at 'to'
                        let b1 = unsafeRemovePiece b from c Pawn
                            b2 = case capturedPt of
                                   Nothing -> b1
                                   Just capPt -> unsafeRemovePiece b1 to (oppositeColor c) capPt
                            newPiece = Piece c ppt
                        in unsafePutPiece b2 to newPiece
                    Nothing ->
                        -- Normal move
                        case capturedPt of
                            -- If capture, we must remove the captured piece and then move the piece.
                            Just capPt ->
                                let b1 = unsafeRemovePiece b to (oppositeColor c) capPt
                                in movePieceFast b1 from to c pt
                            Nothing ->
                                -- Quiet move
                                movePieceFast b from to c pt

            -- Handle En Passant capture
            -- If pawn moves diagonally to empty square (capturedPt is Nothing), it's EP.
            isEP = pt == Pawn && squareFile from /= squareFile to &&
                   (case capturedPt of Nothing -> True; _ -> False)

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
                          in movePieceFast bAfterEP rookFrom rookTo c Rook
                     else bAfterEP

        in bFinal
applyMoveBoardFast b _ NullMove _ _ = b

-- | Optimized movePiece that assumes capture handling is done or not needed (target empty).
-- It only updates the bitboards for the moving piece.
movePieceFast :: Board -> Square -> Square -> Color -> PieceType -> Board
movePieceFast b from to c pt =
    let fromI = unSquare from
        toI   = unSquare to
        mask = (1 `shiftL` fromI) `xor` (1 `shiftL` toI)

        b2 = case (c, pt) of
               (White, Pawn)   -> b { whitePawns   = whitePawns b `xor` mask }
               (White, Knight) -> b { whiteKnights = whiteKnights b `xor` mask }
               (White, Bishop) -> b { whiteBishops = whiteBishops b `xor` mask }
               (White, Rook)   -> b { whiteRooks   = whiteRooks b `xor` mask }
               (White, Queen)  -> b { whiteQueens  = whiteQueens b `xor` mask }
               (White, King)   -> b { whiteKings   = whiteKings b `xor` mask }
               (Black, Pawn)   -> b { blackPawns   = blackPawns b `xor` mask }
               (Black, Knight) -> b { blackKnights = blackKnights b `xor` mask }
               (Black, Bishop) -> b { blackBishops = blackBishops b `xor` mask }
               (Black, Rook)   -> b { blackRooks   = blackRooks b `xor` mask }
               (Black, Queen)  -> b { blackQueens  = blackQueens b `xor` mask }
               (Black, King)   -> b { blackKings   = blackKings b `xor` mask }

        whiteOcc = if c == White then occupiedWhite b `xor` mask else occupiedWhite b
        blackOcc = if c == Black then occupiedBlack b `xor` mask else occupiedBlack b
        totalOcc = occupiedTotal b `xor` mask

    in b2 { occupiedWhite = whiteOcc, occupiedBlack = blackOcc, occupiedTotal = totalOcc }

-- | Apply a move to the board (without updating game state like counters).
-- Handles en passant capture removal and castling rook moves.
applyMoveBoard :: Board -> GameState -> Move -> Board
applyMoveBoard b gs m@(Move from to _) =
    let c = turn gs
        fromI = unSquare from
    in if not (testBit (occupiedBy b c) fromI)
       then b
       else
           let pt = findPieceType b c from
               toI = unSquare to
               capturedPt = if testBit (occupiedTotal b) toI
                            then Just (findPieceType b (oppositeColor c) to)
                            else Nothing
           in applyMoveBoardFast b gs m pt capturedPt
applyMoveBoard b _ NullMove = b
applyMoveBoard b _ _ = b

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

pieceMoves :: Board -> GameState -> PieceType -> [GenMove]
pieceMoves b gs pt = flatMapBitboard genMoves bb
  where
    c = turn gs
    bb = pieceBitboard b c pt

    genMoves :: Square -> [GenMove]
    genMoves from =
        let att = case pt of
                    Knight -> knightAttacks from
                    Bishop -> bishopAttacks from (occupiedTotal b)
                    Rook   -> rookAttacks from (occupiedTotal b)
                    Queen  -> bishopAttacks from (occupiedTotal b) .|. rookAttacks from (occupiedTotal b)
                    King   -> kingAttacks from
                    _      -> 0

            valid = att .&. complement (occupiedBy b c)

            getCapture to =
                if testBit (occupiedTotal b) (unSquare to)
                then Just (findPieceType b (oppositeColor c) to)
                else Nothing

            mkMove to = GenMove (Move from to Nothing) pt (getCapture to)

        in mapBitboard mkMove valid

pawnMoves :: Board -> GameState -> [GenMove]
pawnMoves b gs = flatMapBitboard genPawnMoves bb
  where
    c = turn gs
    bb = pieceBitboard b c Pawn

    genPawnMoves :: Square -> [GenMove]
    genPawnMoves from = pushes ++ captures
      where
        pushes =
            let fwd = if c == White then 8 else -8
                to1 = Square (unSquare from + fwd)
                isPromRank s = (c == White && squareRank s == 7) || (c == Black && squareRank s == 0)

                singlePush =
                    if not (testBit (occupiedTotal b) (unSquare to1))
                    then if isPromRank to1
                         then [ GenMove (Move from to1 (Just p)) Pawn Nothing | p <- [Queen, Rook, Bishop, Knight] ]
                         else [ GenMove (Move from to1 Nothing) Pawn Nothing ]
                    else []

                doublePush =
                    let to2 = Square (unSquare to1 + fwd)
                        startRank = if c == White then 1 else 6
                    in if squareRank from == startRank && not (testBit (occupiedTotal b) (unSquare to1)) && not (testBit (occupiedTotal b) (unSquare to2))
                       then [ GenMove (Move from to2 Nothing) Pawn Nothing ]
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

                mkMove to =
                    -- Determine captured piece.
                    -- If 'to' is occupied, it's a normal capture.
                    -- If 'to' is empty (but valid), it's EP.
                    let toI = unSquare to
                        cap = if testBit (occupiedTotal b) toI
                              then Just (findPieceType b (oppositeColor c) to)
                              else Nothing
                        mvs = if (c == White && squareRank to == 7) || (c == Black && squareRank to == 0)
                              then [ GenMove (Move from to (Just p)) Pawn cap | p <- [Queen, Rook, Bishop, Knight] ]
                              else [ GenMove (Move from to Nothing) Pawn cap ]
                    in mvs

            in flatMapBitboard mkMove valid

castlingMoves :: Board -> GameState -> [GenMove]
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
        in GenMove (Move kingSq toSq Nothing) King Nothing

    kingsideClear =
        let f1 = Square (rank * 8 + 5) -- F
            g1 = Square (rank * 8 + 6) -- G
        in not (testBit (occupiedTotal b) (unSquare f1)) && not (testBit (occupiedTotal b) (unSquare g1))

    queensideClear =
        let d1 = Square (rank * 8 + 3) -- D
            c1 = Square (rank * 8 + 2) -- C
            b1 = Square (rank * 8 + 1) -- B
        in not (testBit (occupiedTotal b) (unSquare d1)) && not (testBit (occupiedTotal b) (unSquare c1)) && not (testBit (occupiedTotal b) (unSquare b1))
