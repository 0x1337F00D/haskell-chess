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

-- | Discovery Candidates Calculation
-- Returns a bitboard of friendly pieces that, if moved, *could* reveal a check.
-- These are friendly pieces blocking a ray from a FRIENDLY slider to the ENEMY king.
{-# INLINE discoveryCandidates #-}
discoveryCandidates :: Board -> Color -> Bitboard
discoveryCandidates b c =
    let oppC = oppositeColor c
    in case kingSquare b oppC of
        Nothing -> 0
        Just enemyKingSq ->
            let occ = occupiedTotal b
                friends = occupiedBy b c

                -- Friendly sliders (R, B, Q)
                myRooks = pieceBitboard b c Rook .|. pieceBitboard b c Queen
                myBishops = pieceBitboard b c Bishop .|. pieceBitboard b c Queen
                mySliders = myRooks .|. myBishops

                checkSlider acc slider =
                    let r = ray enemyKingSq slider
                    in if r == 0
                       then acc
                       else
                           -- Check alignment compatibility
                           let isOrth = squareFile enemyKingSq == squareFile slider || squareRank enemyKingSq == squareRank slider
                               isCompatible = if isOrth then testBit myRooks (unSquare slider) else testBit myBishops (unSquare slider)
                           in if not isCompatible then acc
                              else
                                  let blockers = between enemyKingSq slider .&. occ
                                  -- If exactly one blocker and it is ours, it's a discovery candidate
                                  in if popCount blockers == 1 && (blockers .&. friends /= 0)
                                     then acc .|. blockers
                                     else acc
            in foldBitboard checkSlider 0 mySliders

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

-- | Optimized givesCheck that leverages precalculated discovery candidates.
-- dcBitboard: Bitboard of friendly pieces that are strictly blocking a friendly slider from attacking enemy king.
{-# INLINE givesCheckOptimized #-}
givesCheckOptimized :: Board -> GameState -> Bitboard -> GenMove -> Bool
givesCheckOptimized b gs dcBitboard gm =
    let c = turn gs
        oppC = oppositeColor c
        kingSq = case kingSquare b oppC of
                   Just k -> k
                   Nothing -> Square 0
    in case gm of
        GenQuiet from to pt ->
            -- If 'from' is in discovery candidates, fall back to slow check.
            -- Otherwise, perform Direct Check only.
            if testBit dcBitboard (unSquare from)
            then givesCheckGeneric b gs c kingSq from to pt
            else givesCheckDirect b gs c kingSq from to pt

        GenCapture from to pt _ ->
            if testBit dcBitboard (unSquare from)
            then givesCheckGeneric b gs c kingSq from to pt
            else givesCheckDirect b gs c kingSq from to pt

        GenPromotion from to promoPt ->
            if testBit dcBitboard (unSquare from)
            then givesCheckGeneric b gs c kingSq from to promoPt
            else givesCheckDirect b gs c kingSq from to promoPt

        GenPromotionCapture from to promoPt _ ->
            if testBit dcBitboard (unSquare from)
            then givesCheckGeneric b gs c kingSq from to promoPt
            else givesCheckDirect b gs c kingSq from to promoPt

        -- Fallback for complicated moves
        GenEnPassant {} -> givesCheck b gs gm
        GenCastling {} -> givesCheck b gs gm
        _ -> False

{-# INLINE givesCheckDirect #-}
givesCheckDirect :: Board -> GameState -> Color -> Square -> Square -> Square -> PieceType -> Bool
givesCheckDirect b _ c kingSq from to pt =
    case pt of
        Pawn -> testBit (pawnAttacks c to) (unSquare kingSq)
        Knight -> testBit (knightAttacks to) (unSquare kingSq)
        King -> False -- King moving (direct check by King is impossible in Chess)
        -- Sliders: Need to check blockers
        Bishop -> givesCheckSlider b kingSq from to True
        Rook -> givesCheckSlider b kingSq from to False
        Queen -> givesCheckSlider b kingSq from to True || givesCheckSlider b kingSq from to False

{-# INLINE givesCheckSlider #-}
givesCheckSlider :: Board -> Square -> Square -> Square -> Bool -> Bool
givesCheckSlider b kingSq from to isDiag =
    let occ = occupiedTotal b
        -- Occupancy update: Remove 'from', Add 'to' (we assume 'to' is empty or capture doesn't matter for blockage of THIS ray)
        -- Wait, if 'to' is on the ray, it matters.
        -- Standard slider logic:
        -- Get attack ray from 'to' on the empty board.
        -- Intersect with occupied.
        -- Find first blocker.
        -- If blocker is King, check!

        -- Optimized:
        -- 1. Check alignment.
        r = ray kingSq to
    in if r == 0
       then False
       else
           -- 2. Check if compatible (Diag/Orth)
           let isOrth = squareFile kingSq == squareFile to || squareRank kingSq == squareRank to
               -- If we want Diag (isDiag=True), but it is Orth, fail.
               -- If we want Orth (isDiag=False), but it is Diag, fail.
               compatible = if isDiag
                            then not isOrth -- If isDiag is True (Bishop), we want Diagonal (not Orth)
                            else isOrth     -- If isDiag is False (Rook), we want Orth
           in if not compatible then False
              else
                   -- 3. Check Blockers
                   -- We need updated occupancy.
                   -- Remove 'from'. Add 'to'.
                   -- But 'from' is usually not on the ray between 'to' and 'King' (unless moving along ray away?)
                   -- If moving away on same ray, it would be a discovery, handled by dcBitboard!
                   -- So 'from' is NOT on the ray between 'to' and 'King'.
                   -- So we just need to check if existing occupancy (minus 'to' if capture? No, 'to' IS the piece now)
                   -- blocks the ray.
                   -- The ray 'between' does NOT include endpoints.
                   -- So 'to' is not in 'between'. 'King' is not in 'between'.
                   -- So we just check 'between kingSq to' against 'occ'.
                   -- Wait, we must REMOVE 'from' from occ?
                   -- If 'from' was blocking the ray from 'to' to 'King'?
                   -- Impossible, 'to' is the new position. 'from' is the old.
                   -- If 'from' was between 'to' and 'King', then we are moving TOWARDS the king or AWAY?
                   -- If we move from 'from' to 'to', and 'from' was between 'to' and 'King'.
                   -- That means 'to' is behind 'from' relative to King.
                   -- Then 'from' is no longer there.
                   -- But wait, this is covered by discovery logic?
                   -- No, discovery logic covers friendly sliders BEHIND 'from'.
                   -- Here 'to' IS the slider.
                   -- So we assume 'from' is NOT on the segment (to, kingSq).
                   -- Unless we move e.g. Rook A1 to A2. King at A8.
                   -- from=A1, to=A2. King=A8.
                   -- Ray A2->A8. A1 is not on it.
                   -- Rook A2 to A1. King A8.
                   -- Ray A1->A8. A2 is on it.
                   -- But 'from' is A2. 'to' is A1.
                   -- So 'from' is on the ray. But 'from' is EMPTY after move.
                   -- So we must ensure 'from' is NOT treated as a blocker.
                   -- So: (between kingSq to) .&. (occ `clearBit` from) == 0
                   let blockers = between kingSq to .&. occ
                       realBlockers = blockers `clearBit` (unSquare from)
                   in realBlockers == 0
