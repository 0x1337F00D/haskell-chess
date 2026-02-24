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
import Data.Maybe (fromMaybe)
import Control.Monad.ST (ST)
import Data.Coerce (coerce)

import qualified Data.Vector.Generic         as G
import qualified Data.Vector.Generic.Mutable as M
import qualified Data.Vector.Unboxed         as U

import Chess.Types
import Chess.Bitboard
import Chess.Board.Base
import Chess.Board.GameState hiding (unpackCastling)

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


pattern GenDrop :: PieceType -> Square -> GenMove
pattern GenDrop p t <- (unpackGenDrop -> Just (p, t))
  where GenDrop p t = mkGenDrop p t

pattern GenCastling960 :: Square -> Square -> GenMove
pattern GenCastling960 f t <- (unpackGenCastling960 -> Just (f, t))
  where GenCastling960 f t = mkGenCastling960 f t
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

mkGenDrop :: PieceType -> Square -> GenMove
mkGenDrop p (Square t) = MkGenMove $
    fromIntegral t `shiftL` 6 .|. (6 `shiftL` 12) .|. (fromIntegral (fromEnum p) `shiftL` 15)

unpackGenDrop :: GenMove -> Maybe (PieceType, Square)
unpackGenDrop (MkGenMove w) =
    if (w `shiftR` 12) .&. 0x7 == 6
    then Just (toEnum (fromIntegral ((w `shiftR` 15) .&. 0x7)), Square (fromIntegral ((w `shiftR` 6) .&. 0x3F)))
    else Nothing

mkGenCastling960 :: Square -> Square -> GenMove
mkGenCastling960 (Square f) (Square t) = MkGenMove $
    fromIntegral f .|. (fromIntegral t `shiftL` 6) .|. (7 `shiftL` 12)

unpackGenCastling960 :: GenMove -> Maybe (Square, Square)
unpackGenCastling960 (MkGenMove w) =
    if (w `shiftR` 12) .&. 0x7 == 7
    then Just (Square (fromIntegral (w .&. 0x3F)), Square (fromIntegral ((w `shiftR` 6) .&. 0x3F)))
    else Nothing

-- | Convert a GenMove back to a standard Move.
genMoveToMove :: GenMove -> Move
genMoveToMove (GenQuiet f t _) = Move f t Nothing
genMoveToMove (GenCapture f t _ _) = Move f t Nothing
genMoveToMove (GenEnPassant f t) = Move f t Nothing
genMoveToMove (GenCastling f t) = Move f t Nothing
genMoveToMove (GenPromotion f t p) = Move f t (Just p)
genMoveToMove (GenPromotionCapture f t p _) = Move f t (Just p)

-- | Pinned bitboard calculation
{-# INLINE pinnedBits #-}
pinnedBits :: Board -> Color -> Bitboard
pinnedBits b c =
    case kingSquare b c of
        Nothing -> 0 -- No King, no pins (e.g. Horde White)
        Just kingSq ->
            let occ = occupiedTotal b
                friends = occupiedBy b c
                oppC = oppositeColor c
                -- Gather all enemy sliders (R, B, Q)
                rooks = pieceBitboard b oppC Rook .|. pieceBitboard b oppC Queen
                bishops = pieceBitboard b oppC Bishop .|. pieceBitboard b oppC Queen
                sliders = rooks .|. bishops

                checkPinner acc pinner =
                    let r = ray kingSq pinner
                    in if r == 0
                       then acc
                       else
                           -- Check compatibility (Rook/Queen for Orth, Bishop/Queen for Diag)
                           let isOrth = squareFile kingSq == squareFile pinner || squareRank kingSq == squareRank pinner
                               compatible = if isOrth then testBit rooks (unSquare pinner) else testBit bishops (unSquare pinner)
                           in if not compatible then acc
                              else
                                  let blockers = between kingSq pinner .&. occ
                                  in if popCount blockers == 1 && (blockers .&. friends /= 0)
                                     then acc .|. blockers
                                     else acc
            in foldBitboard checkPinner 0 sliders

-- | Check if three squares are collinear.
{-# INLINE areCollinear #-}
areCollinear :: Square -> Square -> Square -> Bool
areCollinear (Square s1) (Square s2) (Square s3) =
    let f1 = s1 .&. 7
        r1 = s1 `shiftR` 3
        f2 = s2 .&. 7
        r2 = s2 `shiftR` 3
        f3 = s3 .&. 7
        r3 = s3 `shiftR` 3
    in (f1 - f2) * (r2 - r3) == (f2 - f3) * (r1 - r2)

-- | Context-Aware Legality Check
{-# INLINE isLegalSafe #-}
isLegalSafe :: Board -> Bitboard -> GenMove -> Bool
isLegalSafe b pinned gm = case gm of
    GenQuiet from to pt ->
        if pt == King then isLegal b gm
        else checkPinned from to
    GenCapture from to pt _ ->
        if pt == King then isLegal b gm
        else checkPinned from to
    GenPromotion from to _ -> checkPinned from to
    GenPromotionCapture from to _ _ -> checkPinned from to
    GenEnPassant _ _ -> isLegal b gm
    GenCastling _ _ -> isLegal b gm
    GenDrop _ _ -> True
    GenCastling960 _ _ -> isLegal b gm

  where
    c = getTurn (statePacked b)
    kingSq = case kingSquare b c of Just k -> k; Nothing -> Square 0

    checkPinned from to =
        if not (testBit pinned (unSquare from))
        then True
        else areCollinear kingSq from to

-- | Generate all pseudo-legal moves.
pseudoLegalMoves :: Board -> U.Vector GenMove
pseudoLegalMoves b = U.concat
    [ pawnMoves b
    , pieceMoves b Knight
    , pieceMoves b Bishop
    , pieceMoves b Rook
    , pieceMoves b Queen
    , pieceMoves b King
    , castlingMoves b
    ]

-- | Generate all legal moves.
legalMoves :: Board -> [Move]
legalMoves b =
    let c = getTurn (statePacked b)
        occ = occupiedTotal b
        mbKing = kingSquare b c
        attackers = case mbKing of
            Nothing -> 0
            Just k -> attackersTo b k occ .&. occupiedBy b (oppositeColor c)
    in if attackers /= 0
       then map genMoveToMove $ U.toList $ generateEvasions b
       else
           let pinned = pinnedBits b c
               pseudo = pseudoLegalMoves b
               step gm acc = if isLegalSafe b pinned gm then genMoveToMove gm : acc else acc
           in U.foldr step [] pseudo

-- | Generate all legal moves returning GenMove.
legalGenMoves :: Board -> U.Vector GenMove
legalGenMoves b =
    let c = getTurn (statePacked b)
        occ = occupiedTotal b
        mbKing = kingSquare b c
        attackers = case mbKing of
            Nothing -> 0
            Just k -> attackersTo b k occ .&. occupiedBy b (oppositeColor c)
    in if attackers /= 0
       then generateEvasions b
       else
           let pinned = pinnedBits b c
               pseudo = pseudoLegalMoves b
           in U.filter (isLegalSafe b pinned) pseudo

-- | Generate all pseudo-legal capture moves.
pseudoLegalCaptures :: Board -> U.Vector GenMove
pseudoLegalCaptures b = U.concat
    [ pawnCaptures b
    , pieceCaptures b Knight
    , pieceCaptures b Bishop
    , pieceCaptures b Rook
    , pieceCaptures b Queen
    , pieceCaptures b King
    ]

-- | Generate all legal capture moves.
legalCaptures :: Board -> [Move]
legalCaptures b =
    let c = getTurn (statePacked b)
        occ = occupiedTotal b
        mbKing = kingSquare b c
        attackers = case mbKing of
            Nothing -> 0
            Just k -> attackersTo b k occ .&. occupiedBy b (oppositeColor c)
    in if attackers /= 0
       then map genMoveToMove $ U.toList $ generateEvasionCaptures b
       else
           let pinned = pinnedBits b c
               pseudo = pseudoLegalCaptures b
               step gm acc = if isLegalSafe b pinned gm then genMoveToMove gm : acc else acc
           in U.foldr step [] pseudo

-- | Generate all legal capture moves returning GenMove.
legalGenCaptures :: Board -> U.Vector GenMove
legalGenCaptures b =
    let c = getTurn (statePacked b)
        occ = occupiedTotal b
        mbKing = kingSquare b c
        attackers = case mbKing of
            Nothing -> 0
            Just k -> attackersTo b k occ .&. occupiedBy b (oppositeColor c)
    in if attackers /= 0
       then generateEvasionCaptures b
       else
           let pinned = pinnedBits b c
               pseudo = pseudoLegalCaptures b
           in U.filter (isLegalSafe b pinned) pseudo

-- | Generate all pseudo-legal quiet moves.
pseudoLegalQuiets :: Board -> U.Vector GenMove
pseudoLegalQuiets b = U.concat
    [ pawnQuiets b
    , pieceQuiets b Knight
    , pieceQuiets b Bishop
    , pieceQuiets b Rook
    , pieceQuiets b Queen
    , pieceQuiets b King
    , castlingMoves b
    ]

-- | Generate all legal quiet moves returning GenMove.
legalGenQuiets :: Board -> U.Vector GenMove
legalGenQuiets b =
    let c = getTurn (statePacked b)
        occ = occupiedTotal b
        mbKing = kingSquare b c
        attackers = case mbKing of
            Nothing -> 0
            Just k -> attackersTo b k occ .&. occupiedBy b (oppositeColor c)
    in if attackers /= 0
       then generateEvasionQuiets b
       else
           let pinned = pinnedBits b c
               pseudo = pseudoLegalQuiets b
               step gm acc = if isLegalSafe b pinned gm then genMoveToMove gm : acc else acc
           in U.foldr step [] pseudo

-- | Generate all pseudo-legal promotion moves.
pseudoLegalPromotions :: Board -> U.Vector GenMove
pseudoLegalPromotions b = pawnPromotions b

-- | Generate all legal promotion moves returning GenMove.
legalGenPromotions :: Board -> U.Vector GenMove
legalGenPromotions b =
    let c = getTurn (statePacked b)
        occ = occupiedTotal b
        mbKing = kingSquare b c
        attackers = case mbKing of
            Nothing -> 0
            Just k -> attackersTo b k occ .&. occupiedBy b (oppositeColor c)
    in if attackers /= 0
       then generateEvasionPromotions b
       else
           let pinned = pinnedBits b c
               pseudo = pseudoLegalPromotions b
           in U.filter (isLegalSafe b pinned) pseudo

-- | Generate only legal moves when the king is in check.
generateEvasions :: Board -> U.Vector GenMove
generateEvasions b = U.create $ do
    mv <- M.unsafeNew 256
    generateEvasionsInto b mv 0 >>= \idx -> return (M.slice 0 idx mv)

{-# INLINE generateEvasionsInto #-}
generateEvasionsInto :: Board -> U.MVector s GenMove -> Int -> ST s Int
generateEvasionsInto b mv !startIdx = do
    let c = getTurn (statePacked b)
    let kingSq = case kingSquare b c of
                    Just k -> k
                    Nothing -> Square 0

    let occ = occupiedTotal b
    let enemies = occupiedBy b (oppositeColor c)

    let attackers = attackersTo b kingSq occ .&. enemies
    let numAttackers = popCount attackers

    if numAttackers == 0
    then return startIdx
    else do
        idx0 <- fillKingEvasions b (complement 0) mv startIdx

        if numAttackers > 1
        then return idx0
        else do
            let attackerSq = Square (fromMaybe 0 (lsb attackers))
            let r = ray kingSq attackerSq
            let targetMask = if r == 0 then bbFromSquare attackerSq else r

            let ep = getEpSquare (statePacked b)
            let realTargetMask = case ep of
                    NoSquare -> targetMask
                    Square e ->
                         let captureSq = if c == White then Square (e - 8) else Square (e + 8)
                         in if captureSq == attackerSq
                            then targetMask `setBit` e
                            else targetMask

            idx1 <- fillPawnEvasions b realTargetMask mv idx0
            idx2 <- fillPieceEvasions b Knight realTargetMask mv idx1
            idx3 <- fillPieceEvasions b Bishop realTargetMask mv idx2
            idx4 <- fillPieceEvasions b Rook realTargetMask mv idx3
            idx5 <- fillPieceEvasions b Queen realTargetMask mv idx4

            return idx5

generateEvasionCaptures :: Board -> U.Vector GenMove
generateEvasionCaptures b = U.create $ do
    mv <- M.unsafeNew 256
    let c = getTurn (statePacked b)
    let kingSq = case kingSquare b c of Just k -> k; Nothing -> Square 0
    let occ = occupiedTotal b
    let enemies = occupiedBy b (oppositeColor c)
    let attackers = attackersTo b kingSq occ .&. enemies
    let numAttackers = popCount attackers

    if numAttackers == 0 then return (M.slice 0 0 mv)
    else do
        idx0 <- fillKingEvasions b enemies mv 0

        if numAttackers > 1 then return (M.slice 0 idx0 mv)
        else do
            let attackerSq = Square (fromMaybe 0 (lsb attackers))
            let targetMask = bbFromSquare attackerSq

            let ep = getEpSquare (statePacked b)
            let realTargetMask = case ep of
                    NoSquare -> targetMask
                    Square e ->
                         let captureSq = if c == White then Square (e - 8) else Square (e + 8)
                         in if captureSq == attackerSq
                            then targetMask `setBit` e
                            else targetMask

            idx1 <- fillPawnEvasions b realTargetMask mv idx0
            idx2 <- fillPieceEvasions b Knight realTargetMask mv idx1
            idx3 <- fillPieceEvasions b Bishop realTargetMask mv idx2
            idx4 <- fillPieceEvasions b Rook realTargetMask mv idx3
            idx5 <- fillPieceEvasions b Queen realTargetMask mv idx4

            return (M.slice 0 idx5 mv)

generateEvasionQuiets :: Board -> U.Vector GenMove
generateEvasionQuiets b = U.create $ do
    mv <- M.unsafeNew 256
    let c = getTurn (statePacked b)
    let kingSq = case kingSquare b c of Just k -> k; Nothing -> Square 0
    let occ = occupiedTotal b
    let enemies = occupiedBy b (oppositeColor c)
    let attackers = attackersTo b kingSq occ .&. enemies
    let numAttackers = popCount attackers

    if numAttackers == 0 then return (M.slice 0 0 mv)
    else do
        idx0 <- fillKingEvasions b (complement enemies) mv 0

        if numAttackers > 1 then return (M.slice 0 idx0 mv)
        else do
            let attackerSq = Square (fromMaybe 0 (lsb attackers))
            let r = ray kingSq attackerSq
            let targetMask = r

            if targetMask == 0
            then return (M.slice 0 idx0 mv)
            else do
                idx1 <- fillPawnEvasions b targetMask mv idx0
                idx2 <- fillPieceEvasions b Knight targetMask mv idx1
                idx3 <- fillPieceEvasions b Bishop targetMask mv idx2
                idx4 <- fillPieceEvasions b Rook targetMask mv idx3
                idx5 <- fillPieceEvasions b Queen targetMask mv idx4

                return (M.slice 0 idx5 mv)

generateEvasionPromotions :: Board -> U.Vector GenMove
generateEvasionPromotions b = U.create $ do
    mv <- M.unsafeNew 64
    let c = getTurn (statePacked b)
    let kingSq = case kingSquare b c of Just k -> k; Nothing -> Square 0
    let occ = occupiedTotal b
    let enemies = occupiedBy b (oppositeColor c)
    let attackers = attackersTo b kingSq occ .&. enemies
    let numAttackers = popCount attackers

    if numAttackers > 1
    then return (M.slice 0 0 mv)
    else do
         let attackerSq = Square (fromMaybe 0 (lsb attackers))
         let r = ray kingSq attackerSq
         let targetMask = if r == 0 then bbFromSquare attackerSq else r

         idx0 <- fillPawnEvasionPromotions b targetMask mv 0
         return (M.slice 0 idx0 mv)

{-# INLINE fillKingEvasions #-}
fillKingEvasions :: Board -> Bitboard -> U.MVector s GenMove -> Int -> ST s Int
fillKingEvasions b targetMask mv !startIdx =
    let c = getTurn (statePacked b)
        bb = pieceBitboard b c King
        friends = occupiedBy b c
        fillAcc !idx from = do
            let att = kingAttacks from
            let valid = att .&. complement friends .&. targetMask
            let writeMove idx2 to = do
                    let toI = unSquare to
                    let isCap = testBit (occupiedBy b (oppositeColor c)) toI
                    let gm = if isCap
                             then GenCapture from to King (findPieceType b (oppositeColor c) to)
                             else GenQuiet from to King

                    if isLegal b gm
                    then do
                        M.unsafeWrite mv idx2 gm
                        return (idx2 + 1)
                    else return idx2
            foldBitboardM writeMove idx valid
    in foldBitboardM fillAcc startIdx bb

{-# INLINE fillPieceEvasions #-}
fillPieceEvasions :: Board -> PieceType -> Bitboard -> U.MVector s GenMove -> Int -> ST s Int
fillPieceEvasions b pt targetMask mv !startIdx =
    let c = getTurn (statePacked b)
        bb = pieceBitboard b c pt
        occ = occupiedTotal b
        friends = occupiedBy b c
        enemies = occupiedBy b (oppositeColor c)
        oppC = oppositeColor c
        getAttacks from = case pt of
             Knight -> knightAttacks from
             Bishop -> bishopAttacks from occ
             Rook   -> rookAttacks from occ
             Queen  -> bishopAttacks from occ .|. rookAttacks from occ
             _      -> 0
        fillAcc !idx from = do
            let att = getAttacks from
            let valid = att .&. complement friends .&. targetMask
            let writeMove idx2 to = do
                    let toI = unSquare to
                    let isCap = testBit enemies toI
                    let gm = if isCap
                             then GenCapture from to pt (findPieceType b oppC to)
                             else GenQuiet from to pt

                    if isLegal b gm
                    then do
                        M.unsafeWrite mv idx2 gm
                        return (idx2 + 1)
                    else return idx2
            foldBitboardM writeMove idx valid
    in foldBitboardM fillAcc startIdx bb

{-# INLINE fillPawnEvasionPromotions #-}
fillPawnEvasionPromotions :: Board -> Bitboard -> U.MVector s GenMove -> Int -> ST s Int
fillPawnEvasionPromotions b targetMask mv !startIdx =
    let c = getTurn (statePacked b)
        pawns = pieceBitboard b c Pawn
        occ = occupiedTotal b
        enemy = occupiedBy b (oppositeColor c)
        oppC = oppositeColor c

        fillMoves !idx from = do
            let i = unSquare from
            -- Quiets (Push)
            idx1 <- if c == White
               then do
                   let to8 = i + 8
                   if to8 >= 56 && not (testBit occ to8) && testBit targetMask to8
                   then do
                       let dest = Square to8
                       if isLegal b (GenPromotion from dest Queen) then do
                           M.unsafeWrite mv idx     (GenPromotion from dest Queen)
                           M.unsafeWrite mv (idx+1) (GenPromotion from dest Rook)
                           M.unsafeWrite mv (idx+2) (GenPromotion from dest Bishop)
                           M.unsafeWrite mv (idx+3) (GenPromotion from dest Knight)
                           return (idx + 4)
                       else return idx
                   else return idx
               else do
                   let to8 = i - 8
                   if to8 <= 7 && not (testBit occ to8) && testBit targetMask to8
                   then do
                       let dest = Square to8
                       if isLegal b (GenPromotion from dest Queen) then do
                           M.unsafeWrite mv idx     (GenPromotion from dest Queen)
                           M.unsafeWrite mv (idx+1) (GenPromotion from dest Rook)
                           M.unsafeWrite mv (idx+2) (GenPromotion from dest Bishop)
                           M.unsafeWrite mv (idx+3) (GenPromotion from dest Knight)
                           return (idx + 4)
                       else return idx
                   else return idx

            -- Captures
            let checkCapture !ix toSq = do
                    if testBit enemy (unSquare toSq) && testBit targetMask (unSquare toSq)
                    then do
                        let dest = toSq
                        if unSquare dest >= 56 || unSquare dest <= 7
                        then do -- Promotion Capture
                            let capPt = findPieceType b oppC dest
                            if isLegal b (GenPromotionCapture from dest Queen capPt) then do
                                M.unsafeWrite mv ix     (GenPromotionCapture from dest Queen capPt)
                                M.unsafeWrite mv (ix+1) (GenPromotionCapture from dest Rook capPt)
                                M.unsafeWrite mv (ix+2) (GenPromotionCapture from dest Bishop capPt)
                                M.unsafeWrite mv (ix+3) (GenPromotionCapture from dest Knight capPt)
                                return (ix + 4)
                            else return ix
                        else return ix
                    else return ix

            idx2 <- if c == White
                    then do
                        i2 <- if (i `mod` 8) /= 7 then checkCapture idx1 (Square (i+9)) else return idx1
                        if (i `mod` 8) /= 0 then checkCapture i2 (Square (i+7)) else return i2
                    else do
                        i2 <- if (i `mod` 8) /= 7 then checkCapture idx1 (Square (i-7)) else return idx1
                        if (i `mod` 8) /= 0 then checkCapture i2 (Square (i-9)) else return i2

            return idx2

    in foldBitboardM fillMoves startIdx pawns

{-# INLINE fillPawnEvasions #-}
fillPawnEvasions :: Board -> Bitboard -> U.MVector s GenMove -> Int -> ST s Int
fillPawnEvasions b targetMask mv !startIdx =
    let c = getTurn (statePacked b)
        pawns = pieceBitboard b c Pawn
        occ = occupiedTotal b
        enemies = occupiedBy b (oppositeColor c)
        oppC = oppositeColor c
        ep = getEpSquare (statePacked b)

        fillMoves !idx from = do
            let i = unSquare from
            -- Quiets
            idx1 <- if c == White
               then do
                   let to8 = i + 8
                   if to8 < 64 && not (testBit occ to8) && testBit targetMask to8
                   then do
                       if to8 >= 56
                       then do
                           let dest = Square to8
                           if isLegal b (GenPromotion from dest Queen) then do
                               M.unsafeWrite mv idx     (GenPromotion from dest Queen)
                               M.unsafeWrite mv (idx+1) (GenPromotion from dest Rook)
                               M.unsafeWrite mv (idx+2) (GenPromotion from dest Bishop)
                               M.unsafeWrite mv (idx+3) (GenPromotion from dest Knight)
                               return (idx + 4)
                           else return idx
                       else do
                           let gm = GenQuiet from (Square to8) Pawn
                           if isLegal b gm then do M.unsafeWrite mv idx gm; return (idx+1) else return idx
                   else return idx
               else do
                   let to8 = i - 8
                   if to8 >= 0 && not (testBit occ to8) && testBit targetMask to8
                   then do
                       if to8 <= 7
                       then do
                           let dest = Square to8
                           if isLegal b (GenPromotion from dest Queen) then do
                               M.unsafeWrite mv idx     (GenPromotion from dest Queen)
                               M.unsafeWrite mv (idx+1) (GenPromotion from dest Rook)
                               M.unsafeWrite mv (idx+2) (GenPromotion from dest Bishop)
                               M.unsafeWrite mv (idx+3) (GenPromotion from dest Knight)
                               return (idx + 4)
                           else return idx
                       else do
                           let gm = GenQuiet from (Square to8) Pawn
                           if isLegal b gm then do M.unsafeWrite mv idx gm; return (idx+1) else return idx
                   else return idx

            -- Double Push
            idx2 <- if c == White
               then do
                   let to8 = i + 8
                       to16 = i + 16
                   if i >= 8 && i <= 15 && not (testBit occ to8) && not (testBit occ to16) && testBit targetMask to16
                   then do
                       let gm = GenQuiet from (Square to16) Pawn
                       if isLegal b gm then do M.unsafeWrite mv idx1 gm; return (idx1+1) else return idx1
                   else return idx1
               else do
                   let to8 = i - 8
                       to16 = i - 16
                   if i >= 48 && i <= 55 && not (testBit occ to8) && not (testBit occ to16) && testBit targetMask to16
                   then do
                       let gm = GenQuiet from (Square to16) Pawn
                       if isLegal b gm then do M.unsafeWrite mv idx1 gm; return (idx1+1) else return idx1
                   else return idx1

            -- Captures
            let checkCapture !ix toSq = do
                    if testBit enemies (unSquare toSq) && testBit targetMask (unSquare toSq)
                    then do
                        let dest = toSq
                            capPt = findPieceType b oppC dest
                        if unSquare dest >= 56 || unSquare dest <= 7
                        then do -- Promotion Capture
                            if isLegal b (GenPromotionCapture from dest Queen capPt) then do
                                M.unsafeWrite mv ix     (GenPromotionCapture from dest Queen capPt)
                                M.unsafeWrite mv (ix+1) (GenPromotionCapture from dest Rook capPt)
                                M.unsafeWrite mv (ix+2) (GenPromotionCapture from dest Bishop capPt)
                                M.unsafeWrite mv (ix+3) (GenPromotionCapture from dest Knight capPt)
                                return (ix + 4)
                            else return ix
                        else do
                            let gm = GenCapture from dest Pawn capPt
                            if isLegal b gm then do M.unsafeWrite mv ix gm; return (ix+1) else return ix
                    else if ep /= NoSquare && toSq == ep && testBit targetMask (unSquare toSq)
                         then do
                             let gm = GenEnPassant from ep
                             if isLegal b gm then do M.unsafeWrite mv ix gm; return (ix+1) else return ix
                         else return ix

            idx3 <- if c == White
                    then do
                        i3 <- if (i `mod` 8) /= 7 then checkCapture idx2 (Square (i+9)) else return idx2
                        if (i `mod` 8) /= 0 then checkCapture i3 (Square (i+7)) else return i3
                    else do
                        i3 <- if (i `mod` 8) /= 7 then checkCapture idx2 (Square (i-7)) else return idx2
                        if (i `mod` 8) /= 0 then checkCapture i3 (Square (i-9)) else return i3

            return idx3

    in foldBitboardM fillMoves startIdx pawns

-- | Check if there is any legal move.
hasLegalMove :: Board -> Bool
hasLegalMove b =
    U.any (isLegal b) (pieceMoves b King) ||
    U.any (isLegal b) (pieceMoves b Knight) ||
    U.any (isLegal b) (pieceMoves b Bishop) ||
    U.any (isLegal b) (pieceMoves b Rook) ||
    U.any (isLegal b) (pieceMoves b Queen) ||
    U.any (isLegal b) (pawnMoves b) ||
    U.any (isLegal b) (castlingMoves b)

-- | Check if a move is legal.
isLegal :: Board -> GenMove -> Bool
isLegal b gm =
    let b' = applyMoveBoardFast b gm
        c = getTurn (statePacked b)
        kingSq' = kingSquare b' c
        isCastling = case gm of GenCastling _ _ -> True; _ -> False
    in case kingSq' of
        Nothing -> True
        Just k -> not (isAttackedBy b' (oppositeColor c) k) && (if isCastling then castlingSafe b gm else True)

    where
         castlingSafe :: Board -> GenMove -> Bool
         castlingSafe _ (GenCastling f t) =
                let c1 = getTurn (statePacked b)
                    step = (unSquare t - unSquare f) `div` 2
                    mid = Square (unSquare f + step)
                    startAttacked = isAttackedBy b (oppositeColor c1) f
                    midAttacked = isAttackedBy b (oppositeColor c1) mid
                in not startAttacked && not midAttacked
         castlingSafe _ _ = True

-- | Attempt to convert a Move to GenMove and check legality.
isLegalMove :: Board -> Move -> Bool
isLegalMove b m = case toGenMove b m of
    Just gm -> isLegal b gm
    Nothing -> False

-- | Attempt to convert a Move to GenMove.
toGenMove :: Board -> Move -> Maybe GenMove
toGenMove b (Move from to promo) =
    let c = getTurn (statePacked b)
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
toGenMove _ _ = Nothing

-- | Faster version of applyMoveBoard that avoids pieceAt lookups.
applyMoveBoardFast :: Board -> GenMove -> Board
applyMoveBoardFast b gm =
    let c = getTurn (statePacked b)
    in case gm of
        GenQuiet from to pt ->
            movePieceFast b from to c pt

        GenCapture from to pt capPt ->
            let b1 = unsafeRemovePiece b to (oppositeColor c) capPt
            in movePieceFast b1 from to c pt

        GenEnPassant from to ->
            let capSq = Square (unSquare to + (if c == White then -8 else 8))
                b1 = unsafeRemovePiece b capSq (oppositeColor c) Pawn
            in movePieceFast b1 from to c Pawn

        GenCastling from to ->
            let (rookFrom, rookTo) = castlingRookMove from to
                b1 = movePieceFast b from to c King
            in movePieceFast b1 rookFrom rookTo c Rook

        GenPromotion from to promoPt ->
            let b1 = unsafeRemovePiece b from c Pawn
            in unsafePutPiece b1 to (Piece c promoPt)

        GenPromotionCapture from to promoPt capPt ->
            let b1 = unsafeRemovePiece b from c Pawn
                b2 = unsafeRemovePiece b1 to (oppositeColor c) capPt
            in unsafePutPiece b2 to (Piece c promoPt)

movePieceFast :: Board -> Square -> Square -> Color -> PieceType -> Board
movePieceFast = unsafeMovePiece

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

pieceMoves :: Board -> PieceType -> U.Vector GenMove
pieceMoves b pt = U.create $ do
    let total = countPieceMoves b pt
    mv <- M.unsafeNew total
    _ <- fillPieceMoves b pt mv 0
    return mv

{-# INLINE countPieceMoves #-}
countPieceMoves :: Board -> PieceType -> Int
countPieceMoves b pt =
    let c = getTurn (statePacked b)
        bb = pieceBitboard b c pt
        occ = occupiedTotal b
        friends = occupiedBy b c
        getAttacks from = case pt of
             Knight -> knightAttacks from
             Bishop -> bishopAttacks from occ
             Rook   -> rookAttacks from occ
             Queen  -> bishopAttacks from occ .|. rookAttacks from occ
             King   -> kingAttacks from
             _      -> 0
        countMoves acc from = acc + popCount (getAttacks from .&. complement friends)
    in foldBitboard countMoves 0 bb

{-# INLINE fillPieceMoves #-}
fillPieceMoves :: Board -> PieceType -> U.MVector s GenMove -> Int -> ST s Int
fillPieceMoves b pt mv !startIdx =
    let c = getTurn (statePacked b)
        bb = pieceBitboard b c pt
        occ = occupiedTotal b
        friends = occupiedBy b c
        enemies = occupiedBy b (oppositeColor c)
        oppC = oppositeColor c
        getAttacks from = case pt of
             Knight -> knightAttacks from
             Bishop -> bishopAttacks from occ
             Rook   -> rookAttacks from occ
             Queen  -> bishopAttacks from occ .|. rookAttacks from occ
             King   -> kingAttacks from
             _      -> 0
        fillAcc !idx from = do
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
    in foldBitboardM fillAcc startIdx bb

pieceCaptures :: Board -> PieceType -> U.Vector GenMove
pieceCaptures b pt = U.create $ do
    let total = countPieceCaptures b pt
    mv <- M.unsafeNew total
    _ <- fillPieceCaptures b pt mv 0
    return mv

{-# INLINE countPieceCaptures #-}
countPieceCaptures :: Board -> PieceType -> Int
countPieceCaptures b pt =
    let c = getTurn (statePacked b)
        bb = pieceBitboard b c pt
        occ = occupiedTotal b
        enemies = occupiedBy b (oppositeColor c)
        getAttacks from = case pt of
             Knight -> knightAttacks from
             Bishop -> bishopAttacks from occ
             Rook   -> rookAttacks from occ
             Queen  -> bishopAttacks from occ .|. rookAttacks from occ
             King   -> kingAttacks from
             _      -> 0
        countMoves acc from = acc + popCount (getAttacks from .&. enemies)
    in foldBitboard countMoves 0 bb

{-# INLINE fillPieceCaptures #-}
fillPieceCaptures :: Board -> PieceType -> U.MVector s GenMove -> Int -> ST s Int
fillPieceCaptures b pt mv !startIdx =
    let c = getTurn (statePacked b)
        bb = pieceBitboard b c pt
        occ = occupiedTotal b
        enemies = occupiedBy b (oppositeColor c)
        oppC = oppositeColor c
        getAttacks from = case pt of
             Knight -> knightAttacks from
             Bishop -> bishopAttacks from occ
             Rook   -> rookAttacks from occ
             Queen  -> bishopAttacks from occ .|. rookAttacks from occ
             King   -> kingAttacks from
             _      -> 0
        fillAcc !idx from = do
            let att = getAttacks from
            let valid = att .&. enemies
            let writeMove idx2 to = do
                    let gm = GenCapture from to pt (findPieceType b oppC to)
                    M.unsafeWrite mv idx2 gm
                    return (idx2 + 1)
            foldBitboardM writeMove idx valid
    in foldBitboardM fillAcc startIdx bb

pieceQuiets :: Board -> PieceType -> U.Vector GenMove
pieceQuiets b pt = U.create $ do
    let total = countPieceQuiets b pt
    mv <- M.unsafeNew total
    _ <- fillPieceQuiets b pt mv 0
    return mv

{-# INLINE countPieceQuiets #-}
countPieceQuiets :: Board -> PieceType -> Int
countPieceQuiets b pt =
    let c = getTurn (statePacked b)
        bb = pieceBitboard b c pt
        occ = occupiedTotal b
        getAttacks from = case pt of
             Knight -> knightAttacks from
             Bishop -> bishopAttacks from occ
             Rook   -> rookAttacks from occ
             Queen  -> bishopAttacks from occ .|. rookAttacks from occ
             King   -> kingAttacks from
             _      -> 0
        countMoves acc from = acc + popCount (getAttacks from .&. complement occ)
    in foldBitboard countMoves 0 bb

{-# INLINE fillPieceQuiets #-}
fillPieceQuiets :: Board -> PieceType -> U.MVector s GenMove -> Int -> ST s Int
fillPieceQuiets b pt mv !startIdx =
    let c = getTurn (statePacked b)
        bb = pieceBitboard b c pt
        occ = occupiedTotal b
        getAttacks from = case pt of
             Knight -> knightAttacks from
             Bishop -> bishopAttacks from occ
             Rook   -> rookAttacks from occ
             Queen  -> bishopAttacks from occ .|. rookAttacks from occ
             King   -> kingAttacks from
             _      -> 0
        fillAcc !idx from = do
            let att = getAttacks from
            let valid = att .&. complement occ
            let writeMove idx2 to = do
                    let gm = GenQuiet from to pt
                    M.unsafeWrite mv idx2 gm
                    return (idx2 + 1)
            foldBitboardM writeMove idx valid
    in foldBitboardM fillAcc startIdx bb

pawnMoves :: Board -> U.Vector GenMove
pawnMoves b = U.concat [pawnQuiets b, pawnCaptures b, pawnPromotions b]

pawnQuiets :: Board -> U.Vector GenMove
pawnQuiets b = U.create $ do
    let total = countPawnQuiets b
    mv <- M.unsafeNew total
    _ <- fillPawnQuiets b mv 0
    return mv

{-# INLINE countPawnQuiets #-}
countPawnQuiets :: Board -> Int
countPawnQuiets b =
    let c = getTurn (statePacked b)
        pawns = pieceBitboard b c Pawn
        occ = occupiedTotal b
        countMoves acc from =
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
    in foldBitboard countMoves 0 pawns

{-# INLINE fillPawnQuiets #-}
fillPawnQuiets :: Board -> U.MVector s GenMove -> Int -> ST s Int
fillPawnQuiets b mv !startIdx =
    let c = getTurn (statePacked b)
        pawns = pieceBitboard b c Pawn
        occ = occupiedTotal b
        fillMoves !idx from =
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
    in foldBitboardM fillMoves startIdx pawns

pawnPromotions :: Board -> U.Vector GenMove
pawnPromotions b = U.create $ do
    let total = countPawnPromotions b
    mv <- M.unsafeNew total
    _ <- fillPawnPromotions b mv 0
    return mv

{-# INLINE countPawnPromotions #-}
countPawnPromotions :: Board -> Int
countPawnPromotions b =
    let c = getTurn (statePacked b)
        pawns = pieceBitboard b c Pawn
        occ = occupiedTotal b
        countMoves acc from =
            let i = unSquare from
            in if c == White
               then
                   let to8 = i + 8
                   in if not (testBit occ to8) && to8 >= 56 then acc + 4 else acc
               else
                   let to8 = i - 8
                   in if not (testBit occ to8) && to8 <= 7 then acc + 4 else acc
    in foldBitboard countMoves 0 pawns

{-# INLINE fillPawnPromotions #-}
fillPawnPromotions :: Board -> U.MVector s GenMove -> Int -> ST s Int
fillPawnPromotions b mv !startIdx =
    let c = getTurn (statePacked b)
        pawns = pieceBitboard b c Pawn
        occ = occupiedTotal b
        fillMoves !idx from =
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
    in foldBitboardM fillMoves startIdx pawns

pawnCaptures :: Board -> U.Vector GenMove
pawnCaptures b = U.create $ do
    let total = countPawnCaptures b
    mv <- M.unsafeNew total
    _ <- fillPawnCaptures b mv 0
    return mv

{-# INLINE countPawnCaptures #-}
countPawnCaptures :: Board -> Int
countPawnCaptures b =
    let c = getTurn (statePacked b)
        pawns = pieceBitboard b c Pawn
        enemy = occupiedBy b (oppositeColor c)
        ep = getEpSquare (statePacked b)
        epIdx = unSquare ep
        countMoves acc from =
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
    in foldBitboard countMoves 0 pawns

{-# INLINE fillPawnCaptures #-}
fillPawnCaptures :: Board -> U.MVector s GenMove -> Int -> ST s Int
fillPawnCaptures b mv !startIdx =
    let c = getTurn (statePacked b)
        pawns = pieceBitboard b c Pawn
        enemy = occupiedBy b (oppositeColor c)
        oppC = oppositeColor c
        ep = getEpSquare (statePacked b)
        epIdx = unSquare ep
        fillMoves !idx from =
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
    in foldBitboardM fillMoves startIdx pawns

castlingMoves :: Board -> U.Vector GenMove
castlingMoves b = U.create $ do
    let total = countCastlingMoves b
    mv <- M.unsafeNew total
    _ <- fillCastlingMoves b mv 0
    return mv

{-# INLINE countCastlingMoves #-}
countCastlingMoves :: Board -> Int
countCastlingMoves b =
    let c = getTurn (statePacked b)
        rank = if c == White then 0 else 7
        occ = occupiedTotal b
        kingsideClear =
            let f1 = Square (rank * 8 + 5)
                g1 = Square (rank * 8 + 6)
            in not (testBit occ (unSquare f1)) && not (testBit occ (unSquare g1))
        queensideClear =
            let d1 = Square (rank * 8 + 3)
                c1 = Square (rank * 8 + 2)
                b1 = Square (rank * 8 + 1)
            in not (testBit occ (unSquare d1)) && not (testBit occ (unSquare c1)) && not (testBit occ (unSquare b1))
        s = statePacked b
        hasKS = canCastleStandardKingside s c && kingsideClear
        hasQS = canCastleStandardQueenside s c && queensideClear
    in (if hasKS then 1 else 0) + (if hasQS then 1 else 0)

{-# INLINE fillCastlingMoves #-}
fillCastlingMoves :: Board -> U.MVector s GenMove -> Int -> ST s Int
fillCastlingMoves b mv !startIdx = do
    let c = getTurn (statePacked b)
        rank = if c == White then 0 else 7
        occ = occupiedTotal b
        kingSq = Square (rank * 8 + 4)

        kingsideClear =
            let f1 = Square (rank * 8 + 5)
                g1 = Square (rank * 8 + 6)
            in not (testBit occ (unSquare f1)) && not (testBit occ (unSquare g1))
        queensideClear =
            let d1 = Square (rank * 8 + 3)
                c1 = Square (rank * 8 + 2)
                b1 = Square (rank * 8 + 1)
            in not (testBit occ (unSquare d1)) && not (testBit occ (unSquare c1)) && not (testBit occ (unSquare b1))

        mkCastlingMove isKingside =
            let toFile = if isKingside then 6 else 2
                toSq = Square (rank * 8 + toFile)
            in GenCastling kingSq toSq

        s = statePacked b
        hasKS = canCastleStandardKingside s c && kingsideClear
        hasQS = canCastleStandardQueenside s c && queensideClear

    idx1 <- if hasKS
            then do
                M.unsafeWrite mv startIdx (mkCastlingMove True)
                return (startIdx + 1)
            else return startIdx

    idx2 <- if hasQS
            then do
                M.unsafeWrite mv idx1 (mkCastlingMove False)
                return (idx1 + 1)
            else return idx1
    return idx2

-- | Apply a move to the board (without updating game state like counters).
applyMoveBoard :: Board -> Move -> Board
applyMoveBoard b m =
    case toGenMove b m of
        Just gm -> applyMoveBoardFast b gm
        Nothing -> b

-- | Check if a move gives check without fully applying it.
-- This handles all move types efficiently.
givesCheck :: Board -> GenMove -> Bool
givesCheck b gm =
    let c = getTurn (statePacked b)
        oppC = oppositeColor c
        kingSq = case kingSquare b oppC of
                   Just k -> k
                   Nothing -> Square 0
    in case gm of
        GenQuiet from to pt ->
            givesCheckGeneric b c kingSq from to pt

        GenCapture from to pt _ ->
            givesCheckGeneric b c kingSq from to pt

        GenPromotion from to promoPt ->
            givesCheckGeneric b c kingSq from to promoPt

        GenPromotionCapture from to promoPt _ ->
            givesCheckGeneric b c kingSq from to promoPt

        GenEnPassant from to ->
            let
                occ = occupiedTotal b
                fromI = unSquare from
                toI = unSquare to
                capSqI = if c == White then toI - 8 else toI + 8
                -- Remove from, set to, remove captured pawn
                occ' = ((occ `clearBit` fromI) `setBit` toI) `clearBit` capSqI

                -- Magic Lookups from King (Symmetric)
                bAtt = bishopAttacks kingSq occ'
                rAtt = rookAttacks kingSq occ'

                -- 1. Direct Check from 'to' (Pawn)
                direct = testBit (pawnAttacks c to) (unSquare kingSq)

                -- 2. Discovered Check
                (fDiag, fOrth) = if c == White
                                 then (whiteDiagonal b, whiteOrthogonal b)
                                 else (blackDiagonal b, blackOrthogonal b)

                fDiag' = fDiag `clearBit` fromI
                fOrth' = fOrth `clearBit` fromI

                discovered = (bAtt .&. fDiag' /= 0) || (rAtt .&. fOrth' /= 0)

            in direct || discovered

        GenCastling _ _ ->
             let b' = applyMoveBoardFast b gm
             in isAttackedBy b' c kingSq

        _ -> False

{-# INLINE givesCheckGeneric #-}
givesCheckGeneric :: Board -> Color -> Square -> Square -> Square -> PieceType -> Bool
givesCheckGeneric b c kingSq from to pt =
    let
        occ = occupiedTotal b
        fromI = unSquare from
        toI = unSquare to
        -- Remove from, set to (overwrites capture if any)
        occ' = (occ `clearBit` fromI) `setBit` toI

        -- Magic Lookups from King (Symmetric)
        bAtt = bishopAttacks kingSq occ'
        rAtt = rookAttacks kingSq occ'

        -- 1. Direct Check from 'to'
        direct = case pt of
            Pawn -> testBit (pawnAttacks c to) (unSquare kingSq)
            Knight -> testBit (knightAttacks to) (unSquare kingSq)
            Bishop -> testBit bAtt toI
            Rook -> testBit rAtt toI
            Queen -> testBit bAtt toI || testBit rAtt toI
            King -> False

        -- 2. Discovered Check from other sliders
        (fDiag, fOrth) = if c == White
                         then (whiteDiagonal b, whiteOrthogonal b)
                         else (blackDiagonal b, blackOrthogonal b)

        -- Remove the moving piece from friendly sliders if it was one.
        -- We don't check if it was actually a slider, just clearing the bit is safe
        -- as long as 'from' is the moving piece's square.
        -- Note: If we promoted, 'pt' is the new piece, but 'from' held a Pawn (not a slider).
        -- If we captured, 'to' held an enemy.
        -- We only care about friendly sliders BEHIND 'from'.
        fDiag' = fDiag `clearBit` fromI
        fOrth' = fOrth `clearBit` fromI

        discovered = (bAtt .&. fDiag' /= 0) || (rAtt .&. fOrth' /= 0)

    in direct || discovered


-- List Adapters for Core
{-# INLINE pseudoLegalMovesList #-}
pseudoLegalMovesList :: Board -> [GenMove]
pseudoLegalMovesList b = U.toList (pseudoLegalMoves b)

{-# INLINE legalGenMovesList #-}
legalGenMovesList :: Board -> [GenMove]
legalGenMovesList b =
    let c = getTurn (statePacked b)
        occ = occupiedTotal b
        mbKing = kingSquare b c
        attackers = case mbKing of
            Nothing -> 0
            Just k -> attackersTo b k occ .&. occupiedBy b (oppositeColor c)
    in if attackers /= 0
       then U.toList (generateEvasions b)
       else
           let pinned = pinnedBits b c
               pseudo = pseudoLegalMoves b
               step gm acc = if isLegalSafe b pinned gm then gm : acc else acc
           in U.foldr step [] pseudo

{-# INLINE legalGenCapturesList #-}
legalGenCapturesList :: Board -> [GenMove]
legalGenCapturesList b =
    let c = getTurn (statePacked b)
        occ = occupiedTotal b
        mbKing = kingSquare b c
        attackers = case mbKing of
            Nothing -> 0
            Just k -> attackersTo b k occ .&. occupiedBy b (oppositeColor c)
    in if attackers /= 0
       then U.toList (generateEvasionCaptures b)
       else
           let pinned = pinnedBits b c
               pseudo = pseudoLegalCaptures b
               step gm acc = if isLegalSafe b pinned gm then gm : acc else acc
           in U.foldr step [] pseudo

{-# INLINE legalGenQuietsList #-}
legalGenQuietsList :: Board -> [GenMove]
legalGenQuietsList b =
    let c = getTurn (statePacked b)
        occ = occupiedTotal b
        mbKing = kingSquare b c
        attackers = case mbKing of
            Nothing -> 0
            Just k -> attackersTo b k occ .&. occupiedBy b (oppositeColor c)
    in if attackers /= 0
       then U.toList (generateEvasionQuiets b)
       else
           let pinned = pinnedBits b c
               pseudo = pseudoLegalQuiets b
               step gm acc = if isLegalSafe b pinned gm then gm : acc else acc
           in U.foldr step [] pseudo

{-# INLINE legalGenPromotionsList #-}
legalGenPromotionsList :: Board -> [GenMove]
legalGenPromotionsList b =
    let c = getTurn (statePacked b)
        occ = occupiedTotal b
        mbKing = kingSquare b c
        attackers = case mbKing of
            Nothing -> 0
            Just k -> attackersTo b k occ .&. occupiedBy b (oppositeColor c)
    in if attackers /= 0
       then U.toList (generateEvasionPromotions b)
       else
           let pinned = pinnedBits b c
               pseudo = pseudoLegalPromotions b
               step gm acc = if isLegalSafe b pinned gm then gm : acc else acc
           in U.foldr step [] pseudo

{-# INLINE pawnMovesList #-}
pawnMovesList :: Board -> [GenMove]
pawnMovesList b = U.toList (pawnMoves b)

{-# INLINE pieceMovesList #-}
pieceMovesList :: Board -> PieceType -> [GenMove]
pieceMovesList b pt = U.toList (pieceMoves b pt)

{-# INLINE castlingMovesList #-}
castlingMovesList :: Board -> [GenMove]
castlingMovesList b = U.toList (castlingMoves b)
