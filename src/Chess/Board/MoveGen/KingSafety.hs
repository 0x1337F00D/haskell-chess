{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE BangPatterns #-}

module Chess.Board.MoveGen.KingSafety where

import Data.Bits

import Chess.Types
import Chess.Bitboard
import Chess.Board.Base
import Chess.Board.GameState hiding (unpackCastling)
import Chess.Board.MoveGen.Common

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

-- | Iterate over a bitboard in a Builder Monad (or any Monad)
{-# INLINE forBitboard #-}
forBitboard :: Monad m => Bitboard -> (Square -> m ()) -> m ()
forBitboard bb f = foldBitboardM (\_ sq -> f sq) () bb

-- | Check if a move is legal.
isLegal :: Board -> GameState -> GenMove -> Bool
isLegal b gs gm = case gm of
    GenQuiet {} -> isLegalFast b gs gm
    GenCapture {} -> isLegalFast b gs gm
    GenPromotion {} -> isLegalFast b gs gm
    GenPromotionCapture {} -> isLegalFast b gs gm
    GenEnPassant {} -> isLegalFast b gs gm
    _ -> isLegalSlow b gs gm

{-# INLINE isLegalFast #-}
isLegalFast :: Board -> GameState -> GenMove -> Bool
isLegalFast b gs gm = case gm of
    GenQuiet from to pt ->
        let fromI = unSquare from
            toI = unSquare to
            -- Remove from, set to.
            occ = (occupiedTotal b `clearBit` fromI) `setBit` toI
            c = turn gs
            opp = oppositeColor c
        in if pt == King
           then not (isAttackedByOptimized b opp to occ 0)
           else case kingSquare b c of
                  Nothing -> True
                  Just k -> not (isAttackedByOptimized b opp k occ 0)

    GenCapture from to pt _ ->
        let fromI = unSquare from
            -- 'to' is occupied by enemy. Captured piece is at 'to'.
            -- We move to 'to'. Occupancy at 'to' stays 1.
            -- 'from' becomes 0.
            occ = occupiedTotal b `clearBit` fromI
            c = turn gs
            opp = oppositeColor c
            ignored = bit (unSquare to)
        in if pt == King
           then not (isAttackedByOptimized b opp to occ ignored)
           else case kingSquare b c of
                  Nothing -> True
                  Just k -> not (isAttackedByOptimized b opp k occ ignored)

    GenPromotion from to _ ->
        let fromI = unSquare from
            toI = unSquare to
            occ = (occupiedTotal b `clearBit` fromI) `setBit` toI
            c = turn gs
            opp = oppositeColor c
        in case kingSquare b c of
             Nothing -> True
             Just k -> not (isAttackedByOptimized b opp k occ 0)

    GenPromotionCapture from to _ _ ->
        let fromI = unSquare from
            occ = occupiedTotal b `clearBit` fromI
            c = turn gs
            opp = oppositeColor c
            ignored = bit (unSquare to)
        in case kingSquare b c of
             Nothing -> True
             Just k -> not (isAttackedByOptimized b opp k occ ignored)

    GenEnPassant from to ->
        let fromI = unSquare from
            toI = unSquare to
            c = turn gs
            opp = oppositeColor c
            capSqI = if c == White then toI - 8 else toI + 8
            occ = ((occupiedTotal b `clearBit` fromI) `setBit` toI) `clearBit` capSqI
            ignored = bit capSqI
        in case kingSquare b c of
             Nothing -> True
             Just k -> not (isAttackedByOptimized b opp k occ ignored)

    _ -> True -- Should be handled by isLegalSlow

isLegalSlow :: Board -> GameState -> GenMove -> Bool
isLegalSlow b gs gm =
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
