{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE BangPatterns #-}

module Chess.Board.MoveGen where

import Data.Bits
import Data.Word (Word64)
import Foreign.Storable (Storable)
import Control.Monad (liftM)
import Data.Coerce (coerce)

import qualified Data.Vector.Generic         as G
import qualified Data.Vector.Generic.Mutable as M
import qualified Data.Vector.Unboxed         as U
import qualified Data.Vector.Unboxed.Mutable as UM

import Chess.Types
import Chess.Bitboard
import Chess.Board.Base
import Chess.Board.GameState

-- | A move coupled with explicit semantics, packed into a Word64.
-- Layout:
-- Bits 0-5: From Square
-- Bits 6-11: To Square
-- Bits 12-14: Tag
--    0: Quiet (Moving)
--    1: Capture (Moving, Captured)
--    2: EnPassant
--    3: Castling
--    4: Promotion (Promo)
--    5: PromotionCapture (Promo, Captured)
-- Bits 15-17: Piece 1 (Moving for Quiet/Cap, Promo for Prom/PromCap)
-- Bits 18-20: Piece 2 (Captured for Cap/PromCap)
newtype GenMove = MkGenMove Word64
  deriving (Eq, Ord)
  deriving newtype (Show, Storable)

-- Unbox Instances
newtype instance U.MVector s GenMove = MV_GenMove (U.MVector s Word64)
newtype instance U.Vector    GenMove = V_GenMove  (U.Vector    Word64)

instance U.Unbox GenMove

instance M.MVector U.MVector GenMove where
  basicLength (MV_GenMove v) = M.basicLength v
  basicUnsafeSlice i n (MV_GenMove v) = MV_GenMove (M.basicUnsafeSlice i n v)
  basicOverlaps (MV_GenMove v1) (MV_GenMove v2) = M.basicOverlaps v1 v2
  basicUnsafeNew n = MV_GenMove `liftM` M.basicUnsafeNew n
  basicInitialize (MV_GenMove v) = M.basicInitialize v
  basicUnsafeReplicate n x = MV_GenMove `liftM` M.basicUnsafeReplicate n (coerce x)
  basicUnsafeRead (MV_GenMove v) i = coerce `liftM` M.basicUnsafeRead v i
  basicUnsafeWrite (MV_GenMove v) i x = M.basicUnsafeWrite v i (coerce x)
  basicClear (MV_GenMove v) = M.basicClear v
  basicSet (MV_GenMove v) x = M.basicSet v (coerce x)
  basicUnsafeCopy (MV_GenMove v1) (MV_GenMove v2) = M.basicUnsafeCopy v1 v2
  basicUnsafeMove (MV_GenMove v1) (MV_GenMove v2) = M.basicUnsafeMove v1 v2
  basicUnsafeGrow (MV_GenMove v) n = MV_GenMove `liftM` M.basicUnsafeGrow v n

instance G.Vector U.Vector GenMove where
  basicUnsafeFreeze (MV_GenMove v) = V_GenMove `liftM` G.basicUnsafeFreeze v
  basicUnsafeThaw (V_GenMove v) = MV_GenMove `liftM` G.basicUnsafeThaw v
  basicLength (V_GenMove v) = G.basicLength v
  basicUnsafeSlice i n (V_GenMove v) = V_GenMove (G.basicUnsafeSlice i n v)
  basicUnsafeIndexM (V_GenMove v) i = coerce `liftM` G.basicUnsafeIndexM v i
  basicUnsafeCopy (MV_GenMove mv) (V_GenMove v) = G.basicUnsafeCopy mv v
  elemseq _ = seq

-- Pattern Synonyms

pattern GenQuiet :: Square -> Square -> PieceType -> GenMove
pattern GenQuiet f t p <- (unpackQuiet -> Just (f, t, p))
  where GenQuiet f t p = mkQuiet f t p

pattern GenCapture :: Square -> Square -> PieceType -> PieceType -> GenMove
pattern GenCapture f t p c <- (unpackCapture -> Just (f, t, p, c))
  where GenCapture f t p c = mkCapture f t p c

pattern GenEnPassant :: Square -> Square -> GenMove
pattern GenEnPassant f t <- (unpackEnPassant -> Just (f, t))
  where GenEnPassant f t = mkEnPassant f t

pattern GenCastling :: Square -> Square -> GenMove
pattern GenCastling f t <- (unpackCastling -> Just (f, t))
  where GenCastling f t = mkCastling f t

pattern GenPromotion :: Square -> Square -> PieceType -> GenMove
pattern GenPromotion f t p <- (unpackPromotion -> Just (f, t, p))
  where GenPromotion f t p = mkPromotion f t p

pattern GenPromotionCapture :: Square -> Square -> PieceType -> PieceType -> GenMove
pattern GenPromotionCapture f t p c <- (unpackPromotionCapture -> Just (f, t, p, c))
  where GenPromotionCapture f t p c = mkPromotionCapture f t p c

{-# COMPLETE GenQuiet, GenCapture, GenEnPassant, GenCastling, GenPromotion, GenPromotionCapture #-}

-- Helpers for packing/unpacking

mkQuiet :: Square -> Square -> PieceType -> GenMove
mkQuiet (Square f) (Square t) p = MkGenMove $
    fromIntegral f .|. (fromIntegral t `shiftL` 6) .|. (0 `shiftL` 12) .|. (fromIntegral (fromEnum p) `shiftL` 15)

unpackQuiet :: GenMove -> Maybe (Square, Square, PieceType)
unpackQuiet (MkGenMove w) =
    if (w `shiftR` 12) .&. 0x7 == 0
    then Just (Square (fromIntegral (w .&. 0x3F)), Square (fromIntegral ((w `shiftR` 6) .&. 0x3F)), toEnum (fromIntegral ((w `shiftR` 15) .&. 0x7)))
    else Nothing

mkCapture :: Square -> Square -> PieceType -> PieceType -> GenMove
mkCapture (Square f) (Square t) p c = MkGenMove $
    fromIntegral f .|. (fromIntegral t `shiftL` 6) .|. (1 `shiftL` 12) .|. (fromIntegral (fromEnum p) `shiftL` 15) .|. (fromIntegral (fromEnum c) `shiftL` 18)

unpackCapture :: GenMove -> Maybe (Square, Square, PieceType, PieceType)
unpackCapture (MkGenMove w) =
    if (w `shiftR` 12) .&. 0x7 == 1
    then Just (Square (fromIntegral (w .&. 0x3F)), Square (fromIntegral ((w `shiftR` 6) .&. 0x3F)), toEnum (fromIntegral ((w `shiftR` 15) .&. 0x7)), toEnum (fromIntegral ((w `shiftR` 18) .&. 0x7)))
    else Nothing

mkEnPassant :: Square -> Square -> GenMove
mkEnPassant (Square f) (Square t) = MkGenMove $
    fromIntegral f .|. (fromIntegral t `shiftL` 6) .|. (2 `shiftL` 12)

unpackEnPassant :: GenMove -> Maybe (Square, Square)
unpackEnPassant (MkGenMove w) =
    if (w `shiftR` 12) .&. 0x7 == 2
    then Just (Square (fromIntegral (w .&. 0x3F)), Square (fromIntegral ((w `shiftR` 6) .&. 0x3F)))
    else Nothing

mkCastling :: Square -> Square -> GenMove
mkCastling (Square f) (Square t) = MkGenMove $
    fromIntegral f .|. (fromIntegral t `shiftL` 6) .|. (3 `shiftL` 12)

unpackCastling :: GenMove -> Maybe (Square, Square)
unpackCastling (MkGenMove w) =
    if (w `shiftR` 12) .&. 0x7 == 3
    then Just (Square (fromIntegral (w .&. 0x3F)), Square (fromIntegral ((w `shiftR` 6) .&. 0x3F)))
    else Nothing

mkPromotion :: Square -> Square -> PieceType -> GenMove
mkPromotion (Square f) (Square t) p = MkGenMove $
    fromIntegral f .|. (fromIntegral t `shiftL` 6) .|. (4 `shiftL` 12) .|. (fromIntegral (fromEnum p) `shiftL` 15)

unpackPromotion :: GenMove -> Maybe (Square, Square, PieceType)
unpackPromotion (MkGenMove w) =
    if (w `shiftR` 12) .&. 0x7 == 4
    then Just (Square (fromIntegral (w .&. 0x3F)), Square (fromIntegral ((w `shiftR` 6) .&. 0x3F)), toEnum (fromIntegral ((w `shiftR` 15) .&. 0x7)))
    else Nothing

mkPromotionCapture :: Square -> Square -> PieceType -> PieceType -> GenMove
mkPromotionCapture (Square f) (Square t) p c = MkGenMove $
    fromIntegral f .|. (fromIntegral t `shiftL` 6) .|. (5 `shiftL` 12) .|. (fromIntegral (fromEnum p) `shiftL` 15) .|. (fromIntegral (fromEnum c) `shiftL` 18)

unpackPromotionCapture :: GenMove -> Maybe (Square, Square, PieceType, PieceType)
unpackPromotionCapture (MkGenMove w) =
    if (w `shiftR` 12) .&. 0x7 == 5
    then Just (Square (fromIntegral (w .&. 0x3F)), Square (fromIntegral ((w `shiftR` 6) .&. 0x3F)), toEnum (fromIntegral ((w `shiftR` 15) .&. 0x7)), toEnum (fromIntegral ((w `shiftR` 18) .&. 0x7)))
    else Nothing

-- | Convert a GenMove back to a standard Move.
genMoveToMove :: GenMove -> Move
genMoveToMove (GenQuiet f t _) = Move f t Nothing
genMoveToMove (GenCapture f t _ _) = Move f t Nothing
genMoveToMove (GenEnPassant f t) = Move f t Nothing
genMoveToMove (GenCastling f t) = Move f t Nothing
genMoveToMove (GenPromotion f t p) = Move f t (Just p)
genMoveToMove (GenPromotionCapture f t p _) = Move f t (Just p)

-- | Generate all pseudo-legal moves.
pseudoLegalMoves :: Board -> GameState -> U.Vector GenMove
pseudoLegalMoves b gs =
    let pm = pawnMoves b gs
        nm = pieceMoves b gs Knight
        bm = pieceMoves b gs Bishop
        rm = pieceMoves b gs Rook
        qm = pieceMoves b gs Queen
        km = pieceMoves b gs King
        cm = castlingMoves b gs

    in U.concat [pm, nm, bm, rm, qm, km, cm]

-- | Generate all legal moves.
legalMoves :: Board -> GameState -> [Move]
legalMoves b gs = U.toList $ U.map genMoveToMove $ U.filter (isLegal b gs) (pseudoLegalMoves b gs)

-- | Generate all legal moves returning GenMove.
legalGenMoves :: Board -> GameState -> U.Vector GenMove
legalGenMoves b gs = U.filter (isLegal b gs) (pseudoLegalMoves b gs)

-- | Generate all pseudo-legal capture moves.
pseudoLegalCaptures :: Board -> GameState -> U.Vector GenMove
pseudoLegalCaptures b gs = U.concat
    [ pawnCaptures b gs
    , pieceCaptures b gs Knight
    , pieceCaptures b gs Bishop
    , pieceCaptures b gs Rook
    , pieceCaptures b gs Queen
    , pieceCaptures b gs King
    ]

-- | Generate all legal capture moves.
legalCaptures :: Board -> GameState -> [Move]
legalCaptures b gs = U.toList $ U.map genMoveToMove $ U.filter (isLegal b gs) (pseudoLegalCaptures b gs)

-- | Generate all legal capture moves returning GenMove.
legalGenCaptures :: Board -> GameState -> U.Vector GenMove
legalGenCaptures b gs = U.filter (isLegal b gs) (pseudoLegalCaptures b gs)

-- | Generate all pseudo-legal quiet moves.
pseudoLegalQuiets :: Board -> GameState -> U.Vector GenMove
pseudoLegalQuiets b gs = U.concat
    [ pawnQuiets b gs
    , pieceQuiets b gs Knight
    , pieceQuiets b gs Bishop
    , pieceQuiets b gs Rook
    , pieceQuiets b gs Queen
    , pieceQuiets b gs King
    , castlingMoves b gs
    ]

-- | Generate all legal quiet moves returning GenMove.
legalGenQuiets :: Board -> GameState -> U.Vector GenMove
legalGenQuiets b gs = U.filter (isLegal b gs) (pseudoLegalQuiets b gs)

-- | Generate all pseudo-legal promotion moves.
pseudoLegalPromotions :: Board -> GameState -> U.Vector GenMove
pseudoLegalPromotions b gs = pawnPromotions b gs

-- | Generate all legal promotion moves returning GenMove.
legalGenPromotions :: Board -> GameState -> U.Vector GenMove
legalGenPromotions b gs = U.filter (isLegal b gs) (pseudoLegalPromotions b gs)

-- | Check if a move is legal.
isLegal :: Board -> GameState -> GenMove -> Bool
isLegal b gs gm =
    let c = turn gs
        b' = applyMoveBoardFast b gs gm
        kingSq = kingSquare b' c
    in case kingSq of
        Nothing -> False
        Just k -> not (isAttackedBy b' (oppositeColor c) k) && castlingSafe b gs gm

    where
         castlingSafe :: Board -> GameState -> GenMove -> Bool
         castlingSafe _ _ (GenCastling f t) =
                let c = turn gs
                    step = (unSquare t - unSquare f) `div` 2
                    mid = Square (unSquare f + step)
                    startAttacked = isAttackedBy b (oppositeColor c) f
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
                             then Nothing
                             else Just (GenQuiet from to pt)
toGenMove _ _ _ = Nothing

-- | Faster version of applyMoveBoard that avoids pieceAt lookups.
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

        mb = U.modify (\v -> do
            UM.unsafeWrite v fromI 0
            UM.unsafeWrite v toI (pieceToWord8 (Piece c pt))
            ) (mailbox b)

    in b2 { occupiedWhite = whiteOcc, occupiedBlack = blackOcc, occupiedTotal = totalOcc, mailbox = mb }

castlingRookMove :: Square -> Square -> (Square, Square)
castlingRookMove kingFrom kingTo
    | kingTo > kingFrom = (H1 `relativeTo` kingFrom, F1 `relativeTo` kingFrom)
    | otherwise         = (A1 `relativeTo` kingFrom, D1 `relativeTo` kingFrom)
  where
    relativeTo (Square i) (Square k) =
        let rankOffset = (k `div` 8) * 8
            fileOffset = i `mod` 8
        in Square (rankOffset + fileOffset)

kingSquare :: Board -> Color -> Maybe Square
kingSquare b c = fmap Square (lsb (pieceBitboard b c King))

-- Move Generators using Vector construction

pieceMoves :: Board -> GameState -> PieceType -> U.Vector GenMove
pieceMoves b gs pt = U.create $ do
    let c = turn gs
    let bb = pieceBitboard b c pt
    let occ = occupiedTotal b
    let friends = occupiedBy b c
    let enemies = occupiedBy b (oppositeColor c)
    let oppC = oppositeColor c

    let getAttacks from = case pt of
             Knight -> knightAttacks from
             Bishop -> bishopAttacks from occ
             Rook   -> rookAttacks from occ
             Queen  -> bishopAttacks from occ .|. rookAttacks from occ
             King   -> kingAttacks from
             _      -> 0

    -- Pass 1: Count
    let countMoves acc from = acc + popCount (getAttacks from .&. complement friends)
    let total = foldBitboard countMoves 0 bb

    mv <- M.unsafeNew total

    -- Pass 2: Fill
    let fillAcc !idx from = do
            let att = getAttacks from
            let valid = att .&. complement friends

            let writeMove idx2 to = do
                    let toI = unSquare to
                    let isCap = testBit enemies toI
                    let gm = if isCap
                             then GenCapture from to pt (findPieceType b oppC to)
                             else GenQuiet from to pt
                    M.unsafeWrite mv idx2 gm
                    return (idx2 + 1)

            foldBitboardM writeMove idx valid

    _ <- foldBitboardM fillAcc 0 bb
    return mv

pieceCaptures :: Board -> GameState -> PieceType -> U.Vector GenMove
pieceCaptures b gs pt = U.create $ do
    let c = turn gs
    let bb = pieceBitboard b c pt
    let occ = occupiedTotal b
    let enemies = occupiedBy b (oppositeColor c)
    let oppC = oppositeColor c

    let getAttacks from = case pt of
             Knight -> knightAttacks from
             Bishop -> bishopAttacks from occ
             Rook   -> rookAttacks from occ
             Queen  -> bishopAttacks from occ .|. rookAttacks from occ
             King   -> kingAttacks from
             _      -> 0

    -- Pass 1: Count
    let countMoves acc from = acc + popCount (getAttacks from .&. enemies)
    let total = foldBitboard countMoves 0 bb

    mv <- M.unsafeNew total

    -- Pass 2: Fill
    let fillAcc !idx from = do
            let att = getAttacks from
            let valid = att .&. enemies

            let writeMove idx2 to = do
                    let gm = GenCapture from to pt (findPieceType b oppC to)
                    M.unsafeWrite mv idx2 gm
                    return (idx2 + 1)

            foldBitboardM writeMove idx valid

    _ <- foldBitboardM fillAcc 0 bb
    return mv

pieceQuiets :: Board -> GameState -> PieceType -> U.Vector GenMove
pieceQuiets b gs pt = U.create $ do
    let c = turn gs
    let bb = pieceBitboard b c pt
    let occ = occupiedTotal b

    let getAttacks from = case pt of
             Knight -> knightAttacks from
             Bishop -> bishopAttacks from occ
             Rook   -> rookAttacks from occ
             Queen  -> bishopAttacks from occ .|. rookAttacks from occ
             King   -> kingAttacks from
             _      -> 0

    -- Pass 1: Count
    let countMoves acc from = acc + popCount (getAttacks from .&. complement occ)
    let total = foldBitboard countMoves 0 bb

    mv <- M.unsafeNew total

    -- Pass 2: Fill
    let fillAcc !idx from = do
            let att = getAttacks from
            let valid = att .&. complement occ

            let writeMove idx2 to = do
                    let gm = GenQuiet from to pt
                    M.unsafeWrite mv idx2 gm
                    return (idx2 + 1)

            foldBitboardM writeMove idx valid

    _ <- foldBitboardM fillAcc 0 bb
    return mv

pawnMoves :: Board -> GameState -> U.Vector GenMove
pawnMoves b gs = U.concat [pawnQuiets b gs, pawnCaptures b gs, pawnPromotions b gs]

pawnQuiets :: Board -> GameState -> U.Vector GenMove
pawnQuiets b gs = U.create $ do
    let c = turn gs
    let pawns = pieceBitboard b c Pawn
    let occ = occupiedTotal b

    -- Pass 1: Count
    let countMoves acc from =
            let i = unSquare from
            in if c == White
               then
                   let to8 = i + 8
                   in if testBit occ to8 then acc
                      else
                          let acc1 = if to8 >= 56 then acc else acc + 1
                              to16 = i + 16
                          in if i >= 8 && i <= 15 && not (testBit occ to16)
                             then acc1 + 1
                             else acc1
               else
                   let to8 = i - 8
                   in if testBit occ to8 then acc
                      else
                          let acc1 = if to8 <= 7 then acc else acc + 1
                              to16 = i - 16
                          in if i >= 48 && i <= 55 && not (testBit occ to16)
                             then acc1 + 1
                             else acc1

    let total = foldBitboard countMoves 0 pawns
    mv <- M.unsafeNew total

    -- Pass 2: Fill
    let fillMoves !idx from =
            let i = unSquare from
            in if c == White
               then
                   let to8 = i + 8
                   in if testBit occ to8 then return idx
                      else do
                          idx1 <- if to8 >= 56 then return idx
                                  else do
                                      M.unsafeWrite mv idx (GenQuiet from (Square to8) Pawn)
                                      return (idx + 1)
                          let to16 = i + 16
                          if i >= 8 && i <= 15 && not (testBit occ to16)
                          then do
                              M.unsafeWrite mv idx1 (GenQuiet from (Square to16) Pawn)
                              return (idx1 + 1)
                          else return idx1
               else
                   let to8 = i - 8
                   in if testBit occ to8 then return idx
                      else do
                          idx1 <- if to8 <= 7 then return idx
                                  else do
                                      M.unsafeWrite mv idx (GenQuiet from (Square to8) Pawn)
                                      return (idx + 1)
                          let to16 = i - 16
                          if i >= 48 && i <= 55 && not (testBit occ to16)
                          then do
                              M.unsafeWrite mv idx1 (GenQuiet from (Square to16) Pawn)
                              return (idx1 + 1)
                          else return idx1

    _ <- foldBitboardM fillMoves 0 pawns
    return mv

pawnPromotions :: Board -> GameState -> U.Vector GenMove
pawnPromotions b gs = U.create $ do
    let c = turn gs
    let pawns = pieceBitboard b c Pawn
    let occ = occupiedTotal b

    -- Pass 1: Count
    let countMoves acc from =
            let i = unSquare from
            in if c == White
               then
                   let to8 = i + 8
                   in if not (testBit occ to8) && to8 >= 56 then acc + 4 else acc
               else
                   let to8 = i - 8
                   in if not (testBit occ to8) && to8 <= 7 then acc + 4 else acc

    let total = foldBitboard countMoves 0 pawns
    mv <- M.unsafeNew total

    -- Pass 2: Fill
    let fillMoves !idx from =
            let i = unSquare from
            in if c == White
               then
                   let to8 = i + 8
                       dest = Square to8
                   in if not (testBit occ to8) && to8 >= 56
                      then do
                          M.unsafeWrite mv idx     (GenPromotion from dest Queen)
                          M.unsafeWrite mv (idx+1) (GenPromotion from dest Rook)
                          M.unsafeWrite mv (idx+2) (GenPromotion from dest Bishop)
                          M.unsafeWrite mv (idx+3) (GenPromotion from dest Knight)
                          return (idx + 4)
                      else return idx
               else
                   let to8 = i - 8
                       dest = Square to8
                   in if not (testBit occ to8) && to8 <= 7
                      then do
                          M.unsafeWrite mv idx     (GenPromotion from dest Queen)
                          M.unsafeWrite mv (idx+1) (GenPromotion from dest Rook)
                          M.unsafeWrite mv (idx+2) (GenPromotion from dest Bishop)
                          M.unsafeWrite mv (idx+3) (GenPromotion from dest Knight)
                          return (idx + 4)
                      else return idx

    _ <- foldBitboardM fillMoves 0 pawns
    return mv

pawnCaptures :: Board -> GameState -> U.Vector GenMove
pawnCaptures b gs = U.create $ do
    let c = turn gs
    let pawns = pieceBitboard b c Pawn
    let enemy = occupiedBy b (oppositeColor c)
    let oppC = oppositeColor c
    let ep = epSquare gs
    let epIdx = unSquare ep

    -- Pass 1: Count
    let countMoves acc from =
            let i = unSquare from
            in if c == White then
                let
                    -- EP
                    cnt1 = if ep /= NoSquare
                           then if (i + 7) == epIdx && (i `mod` 8) /= 0 then acc + 1
                                else if (i + 9) == epIdx && (i `mod` 8) /= 7 then acc + 1
                                else acc
                           else acc
                    -- Right Capture (i+9)
                    cnt2 = if (i `mod` 8) /= 7
                           then let to9 = i + 9
                                in if testBit enemy to9
                                   then if to9 >= 56 then cnt1 + 4 else cnt1 + 1
                                   else cnt1
                           else cnt1
                    -- Left Capture (i+7)
                    cnt3 = if (i `mod` 8) /= 0
                           then let to7 = i + 7
                                in if testBit enemy to7
                                   then if to7 >= 56 then cnt2 + 4 else cnt2 + 1
                                   else cnt2
                           else cnt2
                in cnt3
            else -- Black
                let
                    -- EP
                    cnt1 = if ep /= NoSquare
                           then if (i - 9) == epIdx && (i `mod` 8) /= 0 then acc + 1
                                else if (i - 7) == epIdx && (i `mod` 8) /= 7 then acc + 1
                                else acc
                           else acc
                    -- Right Capture (i-7)
                    cnt2 = if (i `mod` 8) /= 7
                           then let to7 = i - 7
                                in if testBit enemy to7
                                   then if to7 <= 7 then cnt1 + 4 else cnt1 + 1
                                   else cnt1
                           else cnt1
                    -- Left Capture (i-9)
                    cnt3 = if (i `mod` 8) /= 0
                           then let to9 = i - 9
                                in if testBit enemy to9
                                   then if to9 <= 7 then cnt2 + 4 else cnt2 + 1
                                   else cnt2
                           else cnt2
                in cnt3

    let total = foldBitboard countMoves 0 pawns
    mv <- M.unsafeNew total

    -- Pass 2: Fill
    let fillMoves !idx from =
            let i = unSquare from
            in if c == White then do
                -- EP
                idx1 <- if ep /= NoSquare
                        then if (i + 7) == epIdx && (i `mod` 8) /= 0
                             then do M.unsafeWrite mv idx (GenEnPassant from ep); return (idx + 1)
                             else if (i + 9) == epIdx && (i `mod` 8) /= 7
                             then do M.unsafeWrite mv idx (GenEnPassant from ep); return (idx + 1)
                             else return idx
                        else return idx
                -- Right Capture (i+9)
                idx2 <- if (i `mod` 8) /= 7
                        then let to9 = i + 9
                             in if testBit enemy to9
                                then let dest = Square to9
                                         capPt = findPieceType b oppC dest
                                     in if to9 >= 56
                                        then do
                                            M.unsafeWrite mv idx1 (GenPromotionCapture from dest Queen capPt)
                                            M.unsafeWrite mv (idx1+1) (GenPromotionCapture from dest Rook capPt)
                                            M.unsafeWrite mv (idx1+2) (GenPromotionCapture from dest Bishop capPt)
                                            M.unsafeWrite mv (idx1+3) (GenPromotionCapture from dest Knight capPt)
                                            return (idx1 + 4)
                                        else do
                                            M.unsafeWrite mv idx1 (GenCapture from dest Pawn capPt)
                                            return (idx1 + 1)
                                else return idx1
                        else return idx1
                -- Left Capture (i+7)
                if (i `mod` 8) /= 0
                then let to7 = i + 7
                     in if testBit enemy to7
                        then let dest = Square to7
                                 capPt = findPieceType b oppC dest
                             in if to7 >= 56
                                then do
                                    M.unsafeWrite mv idx2 (GenPromotionCapture from dest Queen capPt)
                                    M.unsafeWrite mv (idx2+1) (GenPromotionCapture from dest Rook capPt)
                                    M.unsafeWrite mv (idx2+2) (GenPromotionCapture from dest Bishop capPt)
                                    M.unsafeWrite mv (idx2+3) (GenPromotionCapture from dest Knight capPt)
                                    return (idx2 + 4)
                                else do
                                    M.unsafeWrite mv idx2 (GenCapture from dest Pawn capPt)
                                    return (idx2 + 1)
                        else return idx2
                else return idx2
            else do -- Black
                -- EP
                idx1 <- if ep /= NoSquare
                        then if (i - 9) == epIdx && (i `mod` 8) /= 0
                             then do M.unsafeWrite mv idx (GenEnPassant from ep); return (idx + 1)
                             else if (i - 7) == epIdx && (i `mod` 8) /= 7
                             then do M.unsafeWrite mv idx (GenEnPassant from ep); return (idx + 1)
                             else return idx
                        else return idx
                -- Right Capture (i-7)
                idx2 <- if (i `mod` 8) /= 7
                        then let to7 = i - 7
                             in if testBit enemy to7
                                then let dest = Square to7
                                         capPt = findPieceType b oppC dest
                                     in if to7 <= 7
                                        then do
                                            M.unsafeWrite mv idx1 (GenPromotionCapture from dest Queen capPt)
                                            M.unsafeWrite mv (idx1+1) (GenPromotionCapture from dest Rook capPt)
                                            M.unsafeWrite mv (idx1+2) (GenPromotionCapture from dest Bishop capPt)
                                            M.unsafeWrite mv (idx1+3) (GenPromotionCapture from dest Knight capPt)
                                            return (idx1 + 4)
                                        else do
                                            M.unsafeWrite mv idx1 (GenCapture from dest Pawn capPt)
                                            return (idx1 + 1)
                                else return idx1
                        else return idx1
                -- Left Capture (i-9)
                if (i `mod` 8) /= 0
                then let to9 = i - 9
                     in if testBit enemy to9
                        then let dest = Square to9
                                 capPt = findPieceType b oppC dest
                             in if to9 <= 7
                                then do
                                    M.unsafeWrite mv idx2 (GenPromotionCapture from dest Queen capPt)
                                    M.unsafeWrite mv (idx2+1) (GenPromotionCapture from dest Rook capPt)
                                    M.unsafeWrite mv (idx2+2) (GenPromotionCapture from dest Bishop capPt)
                                    M.unsafeWrite mv (idx2+3) (GenPromotionCapture from dest Knight capPt)
                                    return (idx2 + 4)
                                else do
                                    M.unsafeWrite mv idx2 (GenCapture from dest Pawn capPt)
                                    return (idx2 + 1)
                        else return idx2
                else return idx2

    _ <- foldBitboardM fillMoves 0 pawns
    return mv

castlingMoves :: Board -> GameState -> U.Vector GenMove
castlingMoves b gs = U.fromList (ks ++ qs)
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

-- | Apply a move to the board (without updating game state like counters).
applyMoveBoard :: Board -> GameState -> Move -> Board
applyMoveBoard b gs m =
    case toGenMove b gs m of
        Just gm -> applyMoveBoardFast b gs gm
        Nothing -> b

-- List Adapters for Core
{-# INLINE pseudoLegalMovesList #-}
pseudoLegalMovesList :: Board -> GameState -> [GenMove]
pseudoLegalMovesList b gs = U.toList (pseudoLegalMoves b gs)

{-# INLINE legalGenMovesList #-}
legalGenMovesList :: Board -> GameState -> [GenMove]
legalGenMovesList b gs = U.toList (legalGenMoves b gs)

{-# INLINE pawnMovesList #-}
pawnMovesList :: Board -> GameState -> [GenMove]
pawnMovesList b gs = U.toList (pawnMoves b gs)

{-# INLINE pieceMovesList #-}
pieceMovesList :: Board -> GameState -> PieceType -> [GenMove]
pieceMovesList b gs pt = U.toList (pieceMoves b gs pt)

{-# INLINE castlingMovesList #-}
castlingMovesList :: Board -> GameState -> [GenMove]
castlingMovesList b gs = U.toList (castlingMoves b gs)
