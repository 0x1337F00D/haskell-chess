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
import Control.Monad (liftM, unless, when)
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
import Chess.Internal.Builder

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
{-# COMPLETE GenQuiet, GenCapture, GenEnPassant, GenCastling, GenPromotion, GenPromotionCapture, GenDrop, GenCastling960 #-}

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
genMoveToMove _ = NullMove -- Should not happen in standard chess

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
                               isCompatible = if isOrth then testBit rooks (unSquare pinner) else testBit bishops (unSquare pinner)
                           in if not isCompatible then acc
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
isLegalSafe :: Board -> GameState -> Bitboard -> GenMove -> Bool
isLegalSafe b gs pinned gm = case gm of
    GenQuiet from to pt ->
        if pt == King then isLegal b gs gm
        else checkPinned from to
    GenCapture from to pt _ ->
        if pt == King then isLegal b gs gm
        else checkPinned from to
    GenPromotion from to _ -> checkPinned from to
    GenPromotionCapture from to _ _ -> checkPinned from to
    GenEnPassant _ _ -> isLegal b gs gm
    GenCastling _ _ -> isLegal b gs gm
    GenDrop _ _ -> True
    GenCastling960 _ _ -> isLegal b gs gm

  where
    c = turn gs
    kingSq = case kingSquare b c of Just k -> k; Nothing -> Square 0

    checkPinned from to =
        if not (testBit pinned (unSquare from))
        then True
        else areCollinear kingSq from to

-- | Iterate over a bitboard in a Builder Monad
{-# INLINE forBitboard #-}
forBitboard :: Monad m => Bitboard -> (Square -> m ()) -> m ()
forBitboard bb f = foldBitboardM (\_ sq -> f sq) () bb

-- | Generate all pseudo-legal moves.
pseudoLegalMoves :: Board -> GameState -> U.Vector GenMove
pseudoLegalMoves b gs = runBuilder256 $ do
       fillPawnQuiets     b gs
       fillPawnCaptures   b gs
       fillPawnPromotions b gs
       fillPieceMoves     b gs Knight
       fillPieceMoves     b gs Bishop
       fillPieceMoves     b gs Rook
       fillPieceMoves     b gs Queen
       fillPieceMoves     b gs King
       fillCastlingMoves  b gs

-- | Generate all legal moves.
legalMoves :: Board -> GameState -> [Move]
legalMoves b gs =
    let c = turn gs
        occ = occupiedTotal b
        mbKing = kingSquare b c
        attackers = case mbKing of
            Nothing -> 0
            Just k -> attackersTo b k occ .&. occupiedBy b (oppositeColor c)
    in if attackers /= 0
       then map genMoveToMove $ U.toList $ generateEvasions b gs
       else
           let pinned = pinnedBits b c
               pseudo = pseudoLegalMoves b gs
               step gm acc = if isLegalSafe b gs pinned gm then genMoveToMove gm : acc else acc
           in U.foldr step [] pseudo

-- | Generate all legal moves returning GenMove.
legalGenMoves :: Board -> GameState -> U.Vector GenMove
legalGenMoves b gs =
    let c = turn gs
        occ = occupiedTotal b
        mbKing = kingSquare b c
        attackers = case mbKing of
            Nothing -> 0
            Just k -> attackersTo b k occ .&. occupiedBy b (oppositeColor c)
    in if attackers /= 0
       then generateEvasions b gs
       else
           let pinned = pinnedBits b c
               pseudo = pseudoLegalMoves b gs
           in U.filter (isLegalSafe b gs pinned) pseudo

-- | Generate all pseudo-legal capture moves.
pseudoLegalCaptures :: Board -> GameState -> U.Vector GenMove
pseudoLegalCaptures b gs = runBuilder256 $ do
       fillPawnCaptures   b gs
       fillPieceCaptures  b gs Knight
       fillPieceCaptures  b gs Bishop
       fillPieceCaptures  b gs Rook
       fillPieceCaptures  b gs Queen
       fillPieceCaptures  b gs King

-- | Generate all legal capture moves.
legalCaptures :: Board -> GameState -> [Move]
legalCaptures b gs =
    let c = turn gs
        occ = occupiedTotal b
        mbKing = kingSquare b c
        attackers = case mbKing of
            Nothing -> 0
            Just k -> attackersTo b k occ .&. occupiedBy b (oppositeColor c)
    in if attackers /= 0
       then map genMoveToMove $ U.toList $ generateEvasionCaptures b gs
       else
           let pinned = pinnedBits b c
               pseudo = pseudoLegalCaptures b gs
               step gm acc = if isLegalSafe b gs pinned gm then genMoveToMove gm : acc else acc
           in U.foldr step [] pseudo

-- | Generate all legal capture moves returning GenMove.
legalGenCaptures :: Board -> GameState -> U.Vector GenMove
legalGenCaptures b gs =
    let c = turn gs
        occ = occupiedTotal b
        mbKing = kingSquare b c
        attackers = case mbKing of
            Nothing -> 0
            Just k -> attackersTo b k occ .&. occupiedBy b (oppositeColor c)
    in if attackers /= 0
       then generateEvasionCaptures b gs
       else
           let pinned = pinnedBits b c
               pseudo = pseudoLegalCaptures b gs
           in U.filter (isLegalSafe b gs pinned) pseudo

-- | Generate all pseudo-legal quiet moves.
pseudoLegalQuiets :: Board -> GameState -> U.Vector GenMove
pseudoLegalQuiets b gs = runBuilder256 $ do
       fillPawnQuiets     b gs
       fillPieceQuiets    b gs Knight
       fillPieceQuiets    b gs Bishop
       fillPieceQuiets    b gs Rook
       fillPieceQuiets    b gs Queen
       fillPieceQuiets    b gs King
       fillCastlingMoves  b gs

-- | Generate all legal quiet moves returning GenMove.
legalGenQuiets :: Board -> GameState -> U.Vector GenMove
legalGenQuiets b gs =
    let c = turn gs
        occ = occupiedTotal b
        mbKing = kingSquare b c
        attackers = case mbKing of
            Nothing -> 0
            Just k -> attackersTo b k occ .&. occupiedBy b (oppositeColor c)
    in if attackers /= 0
       then generateEvasionQuiets b gs
       else
           let pinned = pinnedBits b c
               pseudo = pseudoLegalQuiets b gs
           in U.filter (isLegalSafe b gs pinned) pseudo

-- | Generate all pseudo-legal promotion moves.
pseudoLegalPromotions :: Board -> GameState -> U.Vector GenMove
pseudoLegalPromotions b gs = pawnPromotions b gs

-- | Generate all legal promotion moves returning GenMove.
legalGenPromotions :: Board -> GameState -> U.Vector GenMove
legalGenPromotions b gs =
    let c = turn gs
        occ = occupiedTotal b
        mbKing = kingSquare b c
        attackers = case mbKing of
            Nothing -> 0
            Just k -> attackersTo b k occ .&. occupiedBy b (oppositeColor c)
    in if attackers /= 0
       then generateEvasionPromotions b gs
       else
           let pinned = pinnedBits b c
               pseudo = pseudoLegalPromotions b gs
           in U.filter (isLegalSafe b gs pinned) pseudo

-- | Generate only legal moves when the king is in check.
generateEvasions :: Board -> GameState -> U.Vector GenMove
generateEvasions b gs = runBuilder256 $ do
    let c = turn gs
        kingSq = case kingSquare b c of
                        Just k -> k
                        Nothing -> Square 0 -- Should not happen

        occ = occupiedTotal b
        enemies = occupiedBy b (oppositeColor c)

        attackers = attackersTo b kingSq occ .&. enemies
        numAttackers = popCount attackers

    unless (numAttackers == 0) $ do
        fillKingEvasions b gs (complement 0)
        unless (numAttackers > 1) $ do
             let attackerSq = Square (fromMaybe 0 (lsb attackers))
                 r = ray kingSq attackerSq
                 -- Target mask: capture attacker or block ray
                 targetMask = if r == 0 then bbFromSquare attackerSq else r
                 -- Handle En Passant
                 ep = epSquare gs
                 realTargetMask = case ep of
                        NoSquare -> targetMask
                        Square e ->
                             let captureSq = if c == White then Square (e - 8) else Square (e + 8)
                             in if captureSq == attackerSq
                                then targetMask `setBit` e
                                else targetMask

             fillPawnEvasions b gs realTargetMask
             fillPieceEvasions b gs Knight realTargetMask
             fillPieceEvasions b gs Bishop realTargetMask
             fillPieceEvasions b gs Rook realTargetMask
             fillPieceEvasions b gs Queen realTargetMask

generateEvasionCaptures :: Board -> GameState -> U.Vector GenMove
generateEvasionCaptures b gs = runBuilder256 $ do
    let c = turn gs
        kingSq = case kingSquare b c of Just k -> k; Nothing -> Square 0
        occ = occupiedTotal b
        enemies = occupiedBy b (oppositeColor c)
        attackers = attackersTo b kingSq occ .&. enemies
        numAttackers = popCount attackers

    unless (numAttackers == 0) $ do
        fillKingEvasions b gs enemies
        unless (numAttackers > 1) $ do
            let attackerSq = Square (fromMaybe 0 (lsb attackers))
                targetMask = bbFromSquare attackerSq
                ep = epSquare gs
                realTargetMask = case ep of
                    NoSquare -> targetMask
                    Square e ->
                         let captureSq = if c == White then Square (e - 8) else Square (e + 8)
                         in if captureSq == attackerSq
                            then targetMask `setBit` e
                            else targetMask

            fillPawnEvasions b gs realTargetMask
            fillPieceEvasions b gs Knight realTargetMask
            fillPieceEvasions b gs Bishop realTargetMask
            fillPieceEvasions b gs Rook realTargetMask
            fillPieceEvasions b gs Queen realTargetMask

generateEvasionQuiets :: Board -> GameState -> U.Vector GenMove
generateEvasionQuiets b gs = runBuilder256 $ do
    let c = turn gs
        kingSq = case kingSquare b c of Just k -> k; Nothing -> Square 0
        occ = occupiedTotal b
        enemies = occupiedBy b (oppositeColor c)
        attackers = attackersTo b kingSq occ .&. enemies
        numAttackers = popCount attackers

    unless (numAttackers == 0) $ do
        fillKingEvasions b gs (complement enemies)
        unless (numAttackers > 1) $ do
            let attackerSq = Square (fromMaybe 0 (lsb attackers))
                r = ray kingSq attackerSq
                targetMask = r

            unless (targetMask == 0) $ do
               fillPawnEvasions b gs targetMask
               fillPieceEvasions b gs Knight targetMask
               fillPieceEvasions b gs Bishop targetMask
               fillPieceEvasions b gs Rook targetMask
               fillPieceEvasions b gs Queen targetMask

generateEvasionPromotions :: Board -> GameState -> U.Vector GenMove
generateEvasionPromotions b gs = runBuilder256 $ do
    let c = turn gs
        kingSq = case kingSquare b c of Just k -> k; Nothing -> Square 0
        occ = occupiedTotal b
        enemies = occupiedBy b (oppositeColor c)
        attackers = attackersTo b kingSq occ .&. enemies
        numAttackers = popCount attackers

    unless (numAttackers > 1) $ do
         let attackerSq = Square (fromMaybe 0 (lsb attackers))
             r = ray kingSq attackerSq
             targetMask = if r == 0 then bbFromSquare attackerSq else r
         fillPawnEvasionPromotions b gs targetMask

{-# INLINE fillKingEvasions #-}
fillKingEvasions :: Board -> GameState -> Bitboard -> Builder s GenMove ()
fillKingEvasions b gs targetMask = do
    let c = turn gs
        bb = pieceBitboard b c King
        friends = occupiedBy b c
    forBitboard bb $ \from -> do
            let att = kingAttacks from
            let valid = att .&. complement friends .&. targetMask
            forBitboard valid $ \to -> do
                    let toI = unSquare to
                    let isCap = testBit (occupiedBy b (oppositeColor c)) toI
                    let gm = if isCap
                             then GenCapture from to King (findPieceType b (oppositeColor c) to)
                             else GenQuiet from to King

                    -- Check legality
                    when (isLegal b gs gm) $ emit gm

{-# INLINE fillPieceEvasions #-}
fillPieceEvasions :: Board -> GameState -> PieceType -> Bitboard -> Builder s GenMove ()
fillPieceEvasions b gs pt targetMask = do
    let c = turn gs
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
    forBitboard bb $ \from -> do
            let att = getAttacks from
            let valid = att .&. complement friends .&. targetMask
            forBitboard valid $ \to -> do
                    let toI = unSquare to
                    let isCap = testBit enemies toI
                    let gm = if isCap
                             then GenCapture from to pt (findPieceType b oppC to)
                             else GenQuiet from to pt

                    when (isLegal b gs gm) $ emit gm

{-# INLINE fillPawnEvasionPromotions #-}
fillPawnEvasionPromotions :: Board -> GameState -> Bitboard -> Builder s GenMove ()
fillPawnEvasionPromotions b gs targetMask = do
    let c = turn gs
        pawns = pieceBitboard b c Pawn
        occ = occupiedTotal b
        enemy = occupiedBy b (oppositeColor c)
        oppC = oppositeColor c

    forBitboard pawns $ \from -> do
            let i = unSquare from
            -- Quiets (Push)
            if c == White
               then do
                   let to8 = i + 8
                   when (to8 >= 56 && not (testBit occ to8) && testBit targetMask to8) $ do
                       let dest = Square to8
                       when (isLegal b gs (GenPromotion from dest Queen)) $ do
                           emit (GenPromotion from dest Queen)
                           emit (GenPromotion from dest Rook)
                           emit (GenPromotion from dest Bishop)
                           emit (GenPromotion from dest Knight)
               else do
                   let to8 = i - 8
                   when (to8 <= 7 && not (testBit occ to8) && testBit targetMask to8) $ do
                       let dest = Square to8
                       when (isLegal b gs (GenPromotion from dest Queen)) $ do
                           emit (GenPromotion from dest Queen)
                           emit (GenPromotion from dest Rook)
                           emit (GenPromotion from dest Bishop)
                           emit (GenPromotion from dest Knight)

            -- Captures
            let checkCapture toSq = do
                    when (testBit enemy (unSquare toSq) && testBit targetMask (unSquare toSq)) $ do
                        let dest = toSq
                        if unSquare dest >= 56 || unSquare dest <= 7
                        then do -- Promotion Capture
                            let capPt = findPieceType b oppC dest
                            when (isLegal b gs (GenPromotionCapture from dest Queen capPt)) $ do
                                emit (GenPromotionCapture from dest Queen capPt)
                                emit (GenPromotionCapture from dest Rook capPt)
                                emit (GenPromotionCapture from dest Bishop capPt)
                                emit (GenPromotionCapture from dest Knight capPt)
                        else return ()

            if c == White
            then do
                when ((i `mod` 8) /= 7) $ checkCapture (Square (i+9))
                when ((i `mod` 8) /= 0) $ checkCapture (Square (i+7))
            else do
                when ((i `mod` 8) /= 7) $ checkCapture (Square (i-7))
                when ((i `mod` 8) /= 0) $ checkCapture (Square (i-9))


{-# INLINE fillPawnEvasions #-}
fillPawnEvasions :: Board -> GameState -> Bitboard -> Builder s GenMove ()
fillPawnEvasions b gs targetMask = do
    let c = turn gs
        pawns = pieceBitboard b c Pawn
        occ = occupiedTotal b
        enemies = occupiedBy b (oppositeColor c)
        oppC = oppositeColor c
        ep = epSquare gs

    forBitboard pawns $ \from -> do
            let i = unSquare from
            -- Quiets
            if c == White
               then do
                   let to8 = i + 8
                   when (to8 < 64 && not (testBit occ to8) && testBit targetMask to8) $ do
                       -- Promotion?
                       if to8 >= 56
                       then do
                           let dest = Square to8
                           when (isLegal b gs (GenPromotion from dest Queen)) $ do
                               emit (GenPromotion from dest Queen)
                               emit (GenPromotion from dest Rook)
                               emit (GenPromotion from dest Bishop)
                               emit (GenPromotion from dest Knight)
                       else do
                           let gm = GenQuiet from (Square to8) Pawn
                           when (isLegal b gs gm) $ emit gm
               else do
                   let to8 = i - 8
                   when (to8 >= 0 && not (testBit occ to8) && testBit targetMask to8) $ do
                       if to8 <= 7
                       then do
                           let dest = Square to8
                           when (isLegal b gs (GenPromotion from dest Queen)) $ do
                               emit (GenPromotion from dest Queen)
                               emit (GenPromotion from dest Rook)
                               emit (GenPromotion from dest Bishop)
                               emit (GenPromotion from dest Knight)
                       else do
                           let gm = GenQuiet from (Square to8) Pawn
                           when (isLegal b gs gm) $ emit gm

            -- Double Push
            if c == White
               then do
                   let to8 = i + 8
                       to16 = i + 16
                   when (i >= 8 && i <= 15 && not (testBit occ to8) && not (testBit occ to16) && testBit targetMask to16) $ do
                       let gm = GenQuiet from (Square to16) Pawn
                       when (isLegal b gs gm) $ emit gm
               else do
                   let to8 = i - 8
                   let to16 = i - 16
                   when (i >= 48 && i <= 55 && not (testBit occ to8) && not (testBit occ to16) && testBit targetMask to16) $ do
                       let gm = GenQuiet from (Square to16) Pawn
                       when (isLegal b gs gm) $ emit gm

            -- Captures
            let checkCapture toSq = do
                    if testBit enemies (unSquare toSq) && testBit targetMask (unSquare toSq)
                    then do
                        let dest = toSq
                            capPt = findPieceType b oppC dest
                        if unSquare dest >= 56 || unSquare dest <= 7
                        then do -- Promotion Capture
                            when (isLegal b gs (GenPromotionCapture from dest Queen capPt)) $ do
                                emit (GenPromotionCapture from dest Queen capPt)
                                emit (GenPromotionCapture from dest Rook capPt)
                                emit (GenPromotionCapture from dest Bishop capPt)
                                emit (GenPromotionCapture from dest Knight capPt)
                        else do
                            let gm = GenCapture from dest Pawn capPt
                            when (isLegal b gs gm) $ emit gm
                    else when (ep /= NoSquare && toSq == ep && testBit targetMask (unSquare toSq)) $ do
                             let gm = GenEnPassant from ep
                             when (isLegal b gs gm) $ emit gm

            if c == White
            then do
                when ((i `mod` 8) /= 7) $ checkCapture (Square (i+9))
                when ((i `mod` 8) /= 0) $ checkCapture (Square (i+7))
            else do
                when ((i `mod` 8) /= 7) $ checkCapture (Square (i-7))
                when ((i `mod` 8) /= 0) $ checkCapture (Square (i-9))

-- | Check if there is any legal move.
hasLegalMove :: Board -> GameState -> Bool
hasLegalMove b gs =
    U.any (isLegal b gs) (pieceMoves b gs King) ||
    U.any (isLegal b gs) (pieceMoves b gs Knight) ||
    U.any (isLegal b gs) (pieceMoves b gs Bishop) ||
    U.any (isLegal b gs) (pieceMoves b gs Rook) ||
    U.any (isLegal b gs) (pieceMoves b gs Queen) ||
    U.any (isLegal b gs) (pawnMoves b gs) ||
    U.any (isLegal b gs) (castlingMoves b gs)

-- | Check if a move is legal.
isLegal :: Board -> GameState -> GenMove -> Bool
isLegal b gs gm =
    let b' = applyMoveBoardFast b gs gm
        c = turn gs
        kingSq' = kingSquare b' c
        isCastling = case gm of GenCastling _ _ -> True; _ -> False
    in case kingSq' of
        Nothing -> True
        Just k -> not (isAttackedBy b' (oppositeColor c) k) && (if isCastling then castlingSafe b gs gm else True)

    where
         castlingSafe :: Board -> GameState -> GenMove -> Bool
         castlingSafe _ _ (GenCastling f t) =
                let c1 = turn gs
                    step = (unSquare t - unSquare f) `div` 2
                    mid = Square (unSquare f + step)
                    startAttacked = isAttackedBy b (oppositeColor c1) f
                    midAttacked = isAttackedBy b (oppositeColor c1) mid
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

        _ -> b -- Unsupported move type (e.g. GenDrop)

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

pieceMoves :: Board -> GameState -> PieceType -> U.Vector GenMove
pieceMoves b gs pt = runBuilder256 $ fillPieceMoves b gs pt

{-# INLINE fillPieceMoves #-}
fillPieceMoves :: Board -> GameState -> PieceType -> Builder s GenMove ()
fillPieceMoves b gs pt = do
    let c = turn gs
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
    forBitboard bb $ \from -> do
            let att = getAttacks from
            let valid = att .&. complement friends
            forBitboard valid $ \to -> do
                    let toI = unSquare to
                    let isCap = testBit enemies toI
                    let gm = if isCap
                             then GenCapture from to pt (findPieceType b oppC to)
                             else GenQuiet from to pt
                    emit gm

pieceCaptures :: Board -> GameState -> PieceType -> U.Vector GenMove
pieceCaptures b gs pt = runBuilder256 $ fillPieceCaptures b gs pt

{-# INLINE fillPieceCaptures #-}
fillPieceCaptures :: Board -> GameState -> PieceType -> Builder s GenMove ()
fillPieceCaptures b gs pt = do
    let c = turn gs
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
    forBitboard bb $ \from -> do
            let att = getAttacks from
            let valid = att .&. enemies
            forBitboard valid $ \to -> do
                    let gm = GenCapture from to pt (findPieceType b oppC to)
                    emit gm

pieceQuiets :: Board -> GameState -> PieceType -> U.Vector GenMove
pieceQuiets b gs pt = runBuilder256 $ fillPieceQuiets b gs pt

{-# INLINE fillPieceQuiets #-}
fillPieceQuiets :: Board -> GameState -> PieceType -> Builder s GenMove ()
fillPieceQuiets b gs pt = do
    let c = turn gs
        bb = pieceBitboard b c pt
        occ = occupiedTotal b
        getAttacks from = case pt of
             Knight -> knightAttacks from
             Bishop -> bishopAttacks from occ
             Rook   -> rookAttacks from occ
             Queen  -> bishopAttacks from occ .|. rookAttacks from occ
             King   -> kingAttacks from
             _      -> 0
    forBitboard bb $ \from -> do
            let att = getAttacks from
            let valid = att .&. complement occ
            forBitboard valid $ \to -> do
                    emit (GenQuiet from to pt)

pawnMoves :: Board -> GameState -> U.Vector GenMove
pawnMoves b gs = runBuilder256 $ do
       fillPawnQuiets     b gs
       fillPawnCaptures   b gs
       fillPawnPromotions b gs

pawnQuiets :: Board -> GameState -> U.Vector GenMove
pawnQuiets b gs = runBuilder256 $ fillPawnQuiets b gs

{-# INLINE fillPawnQuiets #-}
fillPawnQuiets :: Board -> GameState -> Builder s GenMove ()
fillPawnQuiets b gs = do
    let c = turn gs
        pawns = pieceBitboard b c Pawn
        occ = occupiedTotal b
    forBitboard pawns $ \from -> do
            let i = unSquare from
            if c == White
               then do
                   let to8 = i + 8
                   unless (testBit occ to8) $ do
                          unless (to8 >= 56) $ do
                                      emit (GenQuiet from (Square to8) Pawn)

                          let to16 = i + 16
                          when (i >= 8 && i <= 15 && not (testBit occ to16)) $ do
                              emit (GenQuiet from (Square to16) Pawn)
               else do
                   let to8 = i - 8
                   unless (testBit occ to8) $ do
                          unless (to8 <= 7) $ do
                                      emit (GenQuiet from (Square to8) Pawn)

                          let to16 = i - 16
                          when (i >= 48 && i <= 55 && not (testBit occ to16)) $ do
                              emit (GenQuiet from (Square to16) Pawn)

pawnPromotions :: Board -> GameState -> U.Vector GenMove
pawnPromotions b gs = runBuilder256 $ fillPawnPromotions b gs

{-# INLINE fillPawnPromotions #-}
fillPawnPromotions :: Board -> GameState -> Builder s GenMove ()
fillPawnPromotions b gs = do
    let c = turn gs
        pawns = pieceBitboard b c Pawn
        occ = occupiedTotal b
    forBitboard pawns $ \from -> do
            let i = unSquare from
            if c == White
               then do
                   let to8 = i + 8
                       dest = Square to8
                   when (not (testBit occ to8) && to8 >= 56) $ do
                          emit (GenPromotion from dest Queen)
                          emit (GenPromotion from dest Rook)
                          emit (GenPromotion from dest Bishop)
                          emit (GenPromotion from dest Knight)
               else do
                   let to8 = i - 8
                       dest = Square to8
                   when (not (testBit occ to8) && to8 <= 7) $ do
                          emit (GenPromotion from dest Queen)
                          emit (GenPromotion from dest Rook)
                          emit (GenPromotion from dest Bishop)
                          emit (GenPromotion from dest Knight)

pawnCaptures :: Board -> GameState -> U.Vector GenMove
pawnCaptures b gs = runBuilder256 $ fillPawnCaptures b gs

{-# INLINE fillPawnCaptures #-}
fillPawnCaptures :: Board -> GameState -> Builder s GenMove ()
fillPawnCaptures b gs = do
    let c = turn gs
        pawns = pieceBitboard b c Pawn
        enemy = occupiedBy b (oppositeColor c)
        oppC = oppositeColor c
        ep = epSquare gs
        epIdx = unSquare ep

    forBitboard pawns $ \from -> do
            let i = unSquare from
            if c == White then do
                -- EP
                when (ep /= NoSquare) $ do
                     if (i + 7) == epIdx && (i `mod` 8) /= 0
                     then emit (GenEnPassant from ep)
                     else when ((i + 9) == epIdx && (i `mod` 8) /= 7) $ do
                          emit (GenEnPassant from ep)

                -- Right Capture (i+9)
                when ((i `mod` 8) /= 7) $ do
                        let to9 = i + 9
                        when (testBit enemy to9) $ do
                                let dest = Square to9
                                    capPt = findPieceType b oppC dest
                                if to9 >= 56
                                then do
                                    emit (GenPromotionCapture from dest Queen capPt)
                                    emit (GenPromotionCapture from dest Rook capPt)
                                    emit (GenPromotionCapture from dest Bishop capPt)
                                    emit (GenPromotionCapture from dest Knight capPt)
                                else do
                                    emit (GenCapture from dest Pawn capPt)

                -- Left Capture (i+7)
                when ((i `mod` 8) /= 0) $ do
                        let to7 = i + 7
                        when (testBit enemy to7) $ do
                                let dest = Square to7
                                    capPt = findPieceType b oppC dest
                                if to7 >= 56
                                then do
                                    emit (GenPromotionCapture from dest Queen capPt)
                                    emit (GenPromotionCapture from dest Rook capPt)
                                    emit (GenPromotionCapture from dest Bishop capPt)
                                    emit (GenPromotionCapture from dest Knight capPt)
                                else do
                                    emit (GenCapture from dest Pawn capPt)

            else do -- Black
                -- EP
                when (ep /= NoSquare) $ do
                     if (i - 9) == epIdx && (i `mod` 8) /= 0
                     then emit (GenEnPassant from ep)
                     else when ((i - 7) == epIdx && (i `mod` 8) /= 7) $ do
                          emit (GenEnPassant from ep)

                -- Right Capture (i-7)
                when ((i `mod` 8) /= 7) $ do
                        let to7 = i - 7
                        when (testBit enemy to7) $ do
                                let dest = Square to7
                                    capPt = findPieceType b oppC dest
                                if to7 <= 7
                                then do
                                    emit (GenPromotionCapture from dest Queen capPt)
                                    emit (GenPromotionCapture from dest Rook capPt)
                                    emit (GenPromotionCapture from dest Bishop capPt)
                                    emit (GenPromotionCapture from dest Knight capPt)
                                else do
                                    emit (GenCapture from dest Pawn capPt)

                -- Left Capture (i-9)
                when ((i `mod` 8) /= 0) $ do
                        let to9 = i - 9
                        when (testBit enemy to9) $ do
                                let dest = Square to9
                                    capPt = findPieceType b oppC dest
                                if to9 <= 7
                                then do
                                    emit (GenPromotionCapture from dest Queen capPt)
                                    emit (GenPromotionCapture from dest Rook capPt)
                                    emit (GenPromotionCapture from dest Bishop capPt)
                                    emit (GenPromotionCapture from dest Knight capPt)
                                else do
                                    emit (GenCapture from dest Pawn capPt)

castlingMoves :: Board -> GameState -> U.Vector GenMove
castlingMoves b gs = runBuilder256 $ fillCastlingMoves b gs

{-# INLINE fillCastlingMoves #-}
fillCastlingMoves :: Board -> GameState -> Builder s GenMove ()
fillCastlingMoves b gs = do
    let c = turn gs
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

        hasKS = canCastleStandardKingside gs c && kingsideClear
        hasQS = canCastleStandardQueenside gs c && queensideClear

    when hasKS $ emit (mkCastlingMove True)
    when hasQS $ emit (mkCastlingMove False)

-- | Apply a move to the board (without updating game state like counters).
applyMoveBoard :: Board -> GameState -> Move -> Board
applyMoveBoard b gs m =
    case toGenMove b gs m of
        Just gm -> applyMoveBoardFast b gs gm
        Nothing -> b

-- | Check if a move gives check without fully applying it.
-- This handles all move types efficiently.
givesCheck :: Board -> GameState -> GenMove -> Bool
givesCheck b gs gm =
    let c = turn gs
        oppC = oppositeColor c
        kingSq = case kingSquare b oppC of
                   Just k -> k
                   Nothing -> Square 0
    in case gm of
        GenQuiet from to pt ->
            givesCheckGeneric b gs c kingSq from to pt

        GenCapture from to pt _ ->
            givesCheckGeneric b gs c kingSq from to pt

        GenPromotion from to promoPt ->
            givesCheckGeneric b gs c kingSq from to promoPt

        GenPromotionCapture from to promoPt _ ->
            givesCheckGeneric b gs c kingSq from to promoPt

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
             let b' = applyMoveBoardFast b gs gm
             in isAttackedBy b' c kingSq

        _ -> False -- Unsupported move type

{-# INLINE givesCheckGeneric #-}
givesCheckGeneric :: Board -> GameState -> Color -> Square -> Square -> Square -> PieceType -> Bool
givesCheckGeneric b _ c kingSq from to pt =
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
pseudoLegalMovesList :: Board -> GameState -> [GenMove]
pseudoLegalMovesList b gs = U.toList (pseudoLegalMoves b gs)

{-# INLINE legalGenMovesList #-}
legalGenMovesList :: Board -> GameState -> [GenMove]
legalGenMovesList b gs =
    let c = turn gs
        occ = occupiedTotal b
        mbKing = kingSquare b c
        attackers = case mbKing of
            Nothing -> 0
            Just k -> attackersTo b k occ .&. occupiedBy b (oppositeColor c)
    in if attackers /= 0
       then U.toList (generateEvasions b gs)
       else
           let pinned = pinnedBits b c
               pseudo = pseudoLegalMoves b gs
               step gm acc = if isLegalSafe b gs pinned gm then gm : acc else acc
           in U.foldr step [] pseudo

{-# INLINE legalGenCapturesList #-}
legalGenCapturesList :: Board -> GameState -> [GenMove]
legalGenCapturesList b gs =
    let c = turn gs
        occ = occupiedTotal b
        mbKing = kingSquare b c
        attackers = case mbKing of
            Nothing -> 0
            Just k -> attackersTo b k occ .&. occupiedBy b (oppositeColor c)
    in if attackers /= 0
       then U.toList (generateEvasionCaptures b gs)
       else
           let pinned = pinnedBits b c
               pseudo = pseudoLegalCaptures b gs
               step gm acc = if isLegalSafe b gs pinned gm then gm : acc else acc
           in U.foldr step [] pseudo

{-# INLINE legalGenQuietsList #-}
legalGenQuietsList :: Board -> GameState -> [GenMove]
legalGenQuietsList b gs =
    let c = turn gs
        occ = occupiedTotal b
        mbKing = kingSquare b c
        attackers = case mbKing of
            Nothing -> 0
            Just k -> attackersTo b k occ .&. occupiedBy b (oppositeColor c)
    in if attackers /= 0
       then U.toList (generateEvasionQuiets b gs)
       else
           let pinned = pinnedBits b c
               pseudo = pseudoLegalQuiets b gs
               step gm acc = if isLegalSafe b gs pinned gm then gm : acc else acc
           in U.foldr step [] pseudo

{-# INLINE legalGenPromotionsList #-}
legalGenPromotionsList :: Board -> GameState -> [GenMove]
legalGenPromotionsList b gs =
    let c = turn gs
        occ = occupiedTotal b
        mbKing = kingSquare b c
        attackers = case mbKing of
            Nothing -> 0
            Just k -> attackersTo b k occ .&. occupiedBy b (oppositeColor c)
    in if attackers /= 0
       then U.toList (generateEvasionPromotions b gs)
       else
           let pinned = pinnedBits b c
               pseudo = pseudoLegalPromotions b gs
               step gm acc = if isLegalSafe b gs pinned gm then gm : acc else acc
           in U.foldr step [] pseudo

{-# INLINE pawnMovesList #-}
pawnMovesList :: Board -> GameState -> [GenMove]
pawnMovesList b gs = U.toList (pawnMoves b gs)

{-# INLINE pieceMovesList #-}
pieceMovesList :: Board -> GameState -> PieceType -> [GenMove]
pieceMovesList b gs pt = U.toList (pieceMoves b gs pt)

{-# INLINE castlingMovesList #-}
castlingMovesList :: Board -> GameState -> [GenMove]
castlingMovesList b gs = U.toList (castlingMoves b gs)

-- | Generate legal moves assuming the king is not in check (Safe).
-- This skips the expensive attackers check and uses isLegalSafe directly.
{-# INLINE legalGenMovesSafeList #-}
legalGenMovesSafeList :: Board -> GameState -> [GenMove]
legalGenMovesSafeList b gs =
    let c = turn gs
        pinned = pinnedBits b c
        pseudo = pseudoLegalMoves b gs
        step gm acc = if isLegalSafe b gs pinned gm then gm : acc else acc
    in U.foldr step [] pseudo

{-# INLINE legalGenCapturesSafeList #-}
legalGenCapturesSafeList :: Board -> GameState -> [GenMove]
legalGenCapturesSafeList b gs =
    let c = turn gs
        pinned = pinnedBits b c
        pseudo = pseudoLegalCaptures b gs
        step gm acc = if isLegalSafe b gs pinned gm then gm : acc else acc
    in U.foldr step [] pseudo

{-# INLINE legalGenQuietsSafeList #-}
legalGenQuietsSafeList :: Board -> GameState -> [GenMove]
legalGenQuietsSafeList b gs =
    let c = turn gs
        pinned = pinnedBits b c
        pseudo = pseudoLegalQuiets b gs
        step gm acc = if isLegalSafe b gs pinned gm then gm : acc else acc
    in U.foldr step [] pseudo

{-# INLINE legalGenPromotionsSafeList #-}
legalGenPromotionsSafeList :: Board -> GameState -> [GenMove]
legalGenPromotionsSafeList b gs =
    let c = turn gs
        pinned = pinnedBits b c
        pseudo = pseudoLegalPromotions b gs
        step gm acc = if isLegalSafe b gs pinned gm then gm : acc else acc
    in U.foldr step [] pseudo
