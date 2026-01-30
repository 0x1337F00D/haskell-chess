{-# LANGUAGE PatternSynonyms #-}
module Chess.Board.MoveGen where

import Data.Bits
import GHC.Conc (par, pseq)

import Chess.Types
import Chess.Bitboard
import Chess.Board.Base
import Chess.Board.GameState

-- | A move coupled with explicit semantics.
-- Replaces the previous product type (Move, PieceType, MoveTag) with a categorical Sum Type.
data GenMove
  = GenQuiet !Square !Square !PieceType
  | GenCapture !Square !Square !PieceType !PieceType -- ^ from, to, moving, captured
  | GenEnPassant !Square !Square                     -- ^ from, to (moving=Pawn, captured=Pawn)
  | GenCastling !Square !Square                      -- ^ from, to (moving=King)
  | GenPromotion !Square !Square !PieceType          -- ^ from, to, promotion (moving=Pawn)
  | GenPromotionCapture !Square !Square !PieceType !PieceType -- ^ from, to, promo, captured (moving=Pawn)
  deriving (Eq, Show)

-- | Convert a GenMove back to a standard Move.
genMoveToMove :: GenMove -> Move
genMoveToMove (GenQuiet f t _) = Move f t Nothing
genMoveToMove (GenCapture f t _ _) = Move f t Nothing
genMoveToMove (GenEnPassant f t) = Move f t Nothing
genMoveToMove (GenCastling f t) = Move f t Nothing
genMoveToMove (GenPromotion f t p) = Move f t (Just p)
genMoveToMove (GenPromotionCapture f t p _) = Move f t (Just p)

-- | Generate all pseudo-legal moves for the side to move.
-- Pseudo-legal means moves that follow piece movement rules and capture rules,
-- but do not necessarily respect the rule that the king must not be in check.
-- Optimized with basic parallelism.
pseudoLegalMoves :: Board -> GameState -> [GenMove]
pseudoLegalMoves b gs =
    let pm = pawnMoves b gs
        nm = pieceMoves b gs Knight
        bm = pieceMoves b gs Bishop
        rm = pieceMoves b gs Rook
        qm = pieceMoves b gs Queen
        km = pieceMoves b gs King
        cm = castlingMoves b gs

        -- Spark evaluations of heavier generators
    in nm `par` bm `par` rm `par` qm `par`
       (pm ++ nm ++ bm ++ rm ++ qm ++ km ++ cm)

-- | Generate all legal moves.
-- Filters pseudo-legal moves to ensure the king is not left in check.
legalMoves :: Board -> GameState -> [Move]
legalMoves b gs = map genMoveToMove $ filter (isLegal b gs) (pseudoLegalMoves b gs)

-- | Generate all legal moves returning GenMove (preserving piece info).
legalGenMoves :: Board -> GameState -> [GenMove]
legalGenMoves b gs = filter (isLegal b gs) (pseudoLegalMoves b gs)

-- | Generate all pseudo-legal capture moves.
pseudoLegalCaptures :: Board -> GameState -> [GenMove]
pseudoLegalCaptures b gs = concat
    [ pawnCaptures b gs
    , pieceCaptures b gs Knight
    , pieceCaptures b gs Bishop
    , pieceCaptures b gs Rook
    , pieceCaptures b gs Queen
    , pieceCaptures b gs King
    ]

-- | Generate all legal capture moves.
legalCaptures :: Board -> GameState -> [Move]
legalCaptures b gs = map genMoveToMove $ filter (isLegal b gs) (pseudoLegalCaptures b gs)

-- | Generate all legal capture moves returning GenMove.
legalGenCaptures :: Board -> GameState -> [GenMove]
legalGenCaptures b gs = filter (isLegal b gs) (pseudoLegalCaptures b gs)

-- | Generate all pseudo-legal quiet moves (pushes, quiets, castling).
pseudoLegalQuiets :: Board -> GameState -> [GenMove]
pseudoLegalQuiets b gs = concat
    [ pawnQuiets b gs
    , pieceQuiets b gs Knight
    , pieceQuiets b gs Bishop
    , pieceQuiets b gs Rook
    , pieceQuiets b gs Queen
    , pieceQuiets b gs King
    , castlingMoves b gs
    ]

-- | Generate all legal quiet moves returning GenMove.
legalGenQuiets :: Board -> GameState -> [GenMove]
legalGenQuiets b gs = filter (isLegal b gs) (pseudoLegalQuiets b gs)

-- | Generate all pseudo-legal promotion moves (quiet only).
pseudoLegalPromotions :: Board -> GameState -> [GenMove]
pseudoLegalPromotions b gs = pawnPromotions b gs

-- | Generate all legal promotion moves returning GenMove.
legalGenPromotions :: Board -> GameState -> [GenMove]
legalGenPromotions b gs = filter (isLegal b gs) (pseudoLegalPromotions b gs)

-- | Check if a move is legal (does not leave own king in check).
-- Note: This function assumes the move is pseudo-legal.
isLegal :: Board -> GameState -> GenMove -> Bool
isLegal b gs gm =
    let c = turn gs
        b' = applyMoveBoardFast b gs gm
        kingSq = kingSquare b' c
    in case kingSq of
        Nothing -> False -- Should not happen if king exists
        Just k -> not (isAttackedBy b' (oppositeColor c) k) && castlingSafe b gs gm

    where
         castlingSafe :: Board -> GameState -> GenMove -> Bool
         castlingSafe _ _ (GenCastling f t) =
                let c = turn gs
                    step = (unSquare t - unSquare f) `div` 2
                    mid = Square (unSquare f + step)
                    -- Check if current square is attacked (can't castle out of check)
                    startAttacked = isAttackedBy b (oppositeColor c) f
                    -- Check if passed square is attacked (can't castle through check)
                    midAttacked = isAttackedBy b (oppositeColor c) mid
                in not startAttacked && not midAttacked
         castlingSafe _ _ _ = True

-- | Attempt to convert a Move to GenMove and check legality.
isLegalMove :: Board -> GameState -> Move -> Bool
isLegalMove b gs m = case toGenMove b gs m of
    Just gm -> isLegal b gs gm
    Nothing -> False

-- | Attempt to convert a Move to GenMove.
toGenMove :: Board -> GameState -> Move -> Maybe GenMove
toGenMove b gs (Move from to promo) =
    let c = turn gs
        fromI = unSquare from
    in if not (testBit (occupiedBy b c) fromI)
       then Nothing
       else
           let pt = findPieceType b c from
               toI = unSquare to
               isCapture = testBit (occupiedTotal b) toI
           in case promo of
               Just ppt ->
                   if isCapture
                   then Just (GenPromotionCapture from to ppt (findPieceType b (oppositeColor c) to))
                   else Just (GenPromotion from to ppt)
               Nothing ->
                   if isCapture
                   then Just (GenCapture from to pt (findPieceType b (oppositeColor c) to))
                   else
                      if pt == Pawn && squareFile from /= squareFile to
                      then Just (GenEnPassant from to)
                      else if pt == King && abs (unSquare from - unSquare to) == 2
                      then Just (GenCastling from to)
                      else
                          let dest = unSquare to
                          in if pt == Pawn && (dest >= 56 || dest <= 7)
                             then Nothing -- Missing promotion
                             else Just (GenQuiet from to pt)
toGenMove _ _ _ = Nothing

-- | Faster version of applyMoveBoard that avoids pieceAt lookups by using provided piece info.
applyMoveBoardFast :: Board -> GameState -> GenMove -> Board
applyMoveBoardFast b gs gm =
    case gm of
        GenQuiet from to pt ->
            movePieceFast b from to (turn gs) pt

        GenCapture from to pt capPt ->
            let c = turn gs
                b1 = unsafeRemovePiece b to (oppositeColor c) capPt
            in movePieceFast b1 from to c pt

        GenEnPassant from to ->
            let c = turn gs
                capSq = Square (unSquare to + (if c == White then -8 else 8))
                b1 = unsafeRemovePiece b capSq (oppositeColor c) Pawn
            in movePieceFast b1 from to c Pawn

        GenCastling from to ->
            let c = turn gs
                (rookFrom, rookTo) = castlingRookMove from to
                b1 = movePieceFast b from to c King
            in movePieceFast b1 rookFrom rookTo c Rook

        GenPromotion from to promoPt ->
            let c = turn gs
                b1 = unsafeRemovePiece b from c Pawn
            in unsafePutPiece b1 to (Piece c promoPt)

        GenPromotionCapture from to promoPt capPt ->
            let c = turn gs
                b1 = unsafeRemovePiece b from c Pawn
                b2 = unsafeRemovePiece b1 to (oppositeColor c) capPt
            in unsafePutPiece b2 to (Piece c promoPt)

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
               (White, Bishop) -> b { whiteBishops = whiteBishops b `xor` mask
                                    , whiteDiagonal = whiteDiagonal b `xor` mask }
               (White, Rook)   -> b { whiteRooks   = whiteRooks b `xor` mask
                                    , whiteOrthogonal = whiteOrthogonal b `xor` mask }
               (White, Queen)  -> b { whiteQueens  = whiteQueens b `xor` mask
                                    , whiteDiagonal = whiteDiagonal b `xor` mask
                                    , whiteOrthogonal = whiteOrthogonal b `xor` mask }
               (White, King)   -> b { whiteKings   = whiteKings b `xor` mask }
               (Black, Pawn)   -> b { blackPawns   = blackPawns b `xor` mask }
               (Black, Knight) -> b { blackKnights = blackKnights b `xor` mask }
               (Black, Bishop) -> b { blackBishops = blackBishops b `xor` mask
                                    , blackDiagonal = blackDiagonal b `xor` mask }
               (Black, Rook)   -> b { blackRooks   = blackRooks b `xor` mask
                                    , blackOrthogonal = blackOrthogonal b `xor` mask }
               (Black, Queen)  -> b { blackQueens  = blackQueens b `xor` mask
                                    , blackDiagonal = blackDiagonal b `xor` mask
                                    , blackOrthogonal = blackOrthogonal b `xor` mask }
               (Black, King)   -> b { blackKings   = blackKings b `xor` mask }

        whiteOcc = if c == White then occupiedWhite b `xor` mask else occupiedWhite b
        blackOcc = if c == Black then occupiedBlack b `xor` mask else occupiedBlack b
        totalOcc = occupiedTotal b `xor` mask

    in b2 { occupiedWhite = whiteOcc, occupiedBlack = blackOcc, occupiedTotal = totalOcc }

-- | Apply a move to the board (without updating game state like counters).
-- Handles en passant capture removal and castling rook moves.
-- It now constructs a GenMove and delegates to applyMoveBoardFast.
applyMoveBoard :: Board -> GameState -> Move -> Board
applyMoveBoard b gs m@(Move from to promo) =
    let c = turn gs
        fromI = unSquare from
    in if not (testBit (occupiedBy b c) fromI)
       then b
       else
           let pt = findPieceType b c from
               toI = unSquare to
               isCapture = testBit (occupiedTotal b) toI

               gm = case promo of
                   Just ppt ->
                       if isCapture
                       then
                           let capPt = findPieceType b (oppositeColor c) to
                           in GenPromotionCapture from to ppt capPt
                       else GenPromotion from to ppt
                   Nothing ->
                       if isCapture
                       then
                           let capPt = findPieceType b (oppositeColor c) to
                           in GenCapture from to pt capPt
                       else
                          if pt == Pawn && squareFile from /= squareFile to
                          then GenEnPassant from to
                          else if pt == King && abs (unSquare from - unSquare to) == 2
                          then GenCastling from to
                          else GenQuiet from to pt

           in applyMoveBoardFast b gs gm
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

            mkMove to =
                if testBit (occupiedTotal b) (unSquare to)
                then GenCapture from to pt (findPieceType b (oppositeColor c) to)
                else GenQuiet from to pt

        in mapBitboard mkMove valid

pieceCaptures :: Board -> GameState -> PieceType -> [GenMove]
pieceCaptures b gs pt = flatMapBitboard genMoves bb
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

            -- Only squares occupied by enemy
            valid = att .&. occupiedBy b (oppositeColor c)

            mkMove to = GenCapture from to pt (findPieceType b (oppositeColor c) to)

        in mapBitboard mkMove valid

pieceQuiets :: Board -> GameState -> PieceType -> [GenMove]
pieceQuiets b gs pt = flatMapBitboard genMoves bb
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

            valid = att .&. complement (occupiedTotal b) -- Only empty squares
            mkMove to = GenQuiet from to pt

        in mapBitboard mkMove valid

pawnMoves :: Board -> GameState -> [GenMove]
pawnMoves b gs =
    pawnQuiets b gs ++ pawnCaptures b gs ++ pawnPromotions b gs

pawnQuiets :: Board -> GameState -> [GenMove]
pawnQuiets b gs =
    if c == White then whitePawnQuiets else blackPawnQuiets
  where
    c = turn gs
    pawns = pieceBitboard b c Pawn
    occ   = occupiedTotal b

    whitePawnQuiets =
        [ m
        | from <- mapBitboard id pawns
        , let i = unSquare from
        , m <- genWhiteQuiets i from
        ]

    genWhiteQuiets i from =
        let to8 = i + 8
        in if not (testBit occ to8)
           then
               if to8 >= 56 -- Promotion (Rank 8) - Handled in Proms
               then []
               else
                   let moves = [GenQuiet from (Square to8) Pawn]
                       to16 = i + 16
                   in if i >= 8 && i <= 15 -- Rank 2
                         && not (testBit occ to16)
                      then moves ++ [GenQuiet from (Square to16) Pawn]
                      else moves
           else []

    blackPawnQuiets =
        [ m
        | from <- mapBitboard id pawns
        , let i = unSquare from
        , m <- genBlackQuiets i from
        ]

    genBlackQuiets i from =
        let to8 = i - 8
        in if not (testBit occ to8)
           then
               if to8 <= 7 -- Promotion (Rank 1) - Handled in Proms
               then []
               else
                   let moves = [GenQuiet from (Square to8) Pawn]
                       to16 = i - 16
                   in if i >= 48 && i <= 55 -- Rank 7
                         && not (testBit occ to16)
                      then moves ++ [GenQuiet from (Square to16) Pawn]
                      else moves
           else []

pawnPromotions :: Board -> GameState -> [GenMove]
pawnPromotions b gs =
    if c == White then whitePawnPromotions else blackPawnPromotions
  where
    c = turn gs
    pawns = pieceBitboard b c Pawn
    occ   = occupiedTotal b

    whitePawnPromotions =
        [ m
        | from <- mapBitboard id pawns
        , let i = unSquare from
        , m <- genWhitePromotions i from
        ]

    genWhitePromotions i from =
        let to8 = i + 8
        in if not (testBit occ to8) && to8 >= 56 -- Rank 8
           then [ GenPromotion from (Square to8) p | p <- [Queen, Rook, Bishop, Knight] ]
           else []

    blackPawnPromotions =
        [ m
        | from <- mapBitboard id pawns
        , let i = unSquare from
        , m <- genBlackPromotions i from
        ]

    genBlackPromotions i from =
        let to8 = i - 8
        in if not (testBit occ to8) && to8 <= 7 -- Rank 1
           then [ GenPromotion from (Square to8) p | p <- [Queen, Rook, Bishop, Knight] ]
           else []

pawnCaptures :: Board -> GameState -> [GenMove]
pawnCaptures b gs =
    if c == White then whitePawnCaptures else blackPawnCaptures
  where
    c = turn gs
    pawns = pieceBitboard b c Pawn
    enemy = occupiedBy b (oppositeColor c)

    mkCap from dest =
        let capPt = findPieceType b (oppositeColor c) dest
        in if unSquare dest >= 56 || unSquare dest <= 7
           then [ GenPromotionCapture from dest p capPt | p <- [Queen, Rook, Bishop, Knight] ]
           else [ GenCapture from dest Pawn capPt ]

    whitePawnCaptures =
        [ m
        | from <- mapBitboard id pawns
        , let i = unSquare from
        , m <- genWhiteCaptures i from
        ]

    genWhiteCaptures i from =
        let
            capLeftMoves =
                if (i `mod` 8) /= 0
                then let to7 = i + 7 in if testBit enemy to7 then mkCap from (Square to7) else []
                else []
            capRightMoves =
                if (i `mod` 8) /= 7
                then let to9 = i + 9 in if testBit enemy to9 then mkCap from (Square to9) else []
                else []
            epMoves = case epSquare gs of
                Nothing -> []
                Just ep ->
                    let epIdx = unSquare ep
                    in if (i + 7) == epIdx && (i `mod` 8) /= 0 then [GenEnPassant from ep]
                       else if (i + 9) == epIdx && (i `mod` 8) /= 7 then [GenEnPassant from ep]
                       else []
        in capLeftMoves ++ capRightMoves ++ epMoves

    blackPawnCaptures =
        [ m
        | from <- mapBitboard id pawns
        , let i = unSquare from
        , m <- genBlackCaptures i from
        ]

    genBlackCaptures i from =
        let
            capLeftMoves =
                if (i `mod` 8) /= 0
                then let to9 = i - 9 in if testBit enemy to9 then mkCap from (Square to9) else []
                else []
            capRightMoves =
                if (i `mod` 8) /= 7
                then let to7 = i - 7 in if testBit enemy to7 then mkCap from (Square to7) else []
                else []
            epMoves = case epSquare gs of
                Nothing -> []
                Just ep ->
                    let epIdx = unSquare ep
                    in if (i - 9) == epIdx && (i `mod` 8) /= 0 then [GenEnPassant from ep]
                       else if (i - 7) == epIdx && (i `mod` 8) /= 7 then [GenEnPassant from ep]
                       else []
        in capLeftMoves ++ capRightMoves ++ epMoves

castlingMoves :: Board -> GameState -> [GenMove]
castlingMoves b gs = ks ++ qs
  where
    c = turn gs
    ks = if canCastleKingside gs c && kingsideClear then [mkCastlingMove True] else []
    qs = if canCastleQueenside gs c && queensideClear then [mkCastlingMove False] else []

    rank = if c == White then 0 else 7
    kingSq = Square (rank * 8 + 4)

    mkCastlingMove isKingside =
        let toFile = if isKingside then 6 else 2
            toSq = Square (rank * 8 + toFile)
        in GenCastling kingSq toSq

    kingsideClear =
        let f1 = Square (rank * 8 + 5)
            g1 = Square (rank * 8 + 6)
        in not (testBit (occupiedTotal b) (unSquare f1)) && not (testBit (occupiedTotal b) (unSquare g1))

    queensideClear =
        let d1 = Square (rank * 8 + 3)
            c1 = Square (rank * 8 + 2)
            b1 = Square (rank * 8 + 1)
        in not (testBit (occupiedTotal b) (unSquare d1)) && not (testBit (occupiedTotal b) (unSquare c1)) && not (testBit (occupiedTotal b) (unSquare b1))
