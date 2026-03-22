{-# LANGUAGE FlexibleContexts #-}
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
import Chess.Board.MoveGen.Internal

-- | Pinned bitboard calculation
{-# INLINE pinnedBits #-}
pinnedBits :: Board -> Color -> Bitboard
pinnedBits b c =
    if not (hasKing b c) then 0 -- No King, no pins (e.g. Horde White)
    else
        let kingSq = kingSquareFast b c
            occ = occupiedTotal b
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
                           -- Bolt: replaced `squareFile a == squareFile b || squareRank a == squareRank b`
                           -- with O(1) bitwise array lookup `isOrthogonallyAligned`. Avoids coordinate ops.
                           let isOrth = isOrthogonallyAligned kingSq pinner
                               isCompatible = if isOrth then testBit rooks (unSquare pinner) else testBit bishops (unSquare pinner)
                           in if not isCompatible then acc
                              else
                                  let blockers = between kingSq pinner .&. occ
                                  in if (blockers .&. friends /= 0) && (blockers .&. (blockers - 1)) == 0
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
    in if not (hasKing b oppC) then 0
       else
        let enemyKingSq = kingSquareFast b oppC
            occ = occupiedTotal b
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
                           -- Bolt: replaced `squareFile a == squareFile b || squareRank a == squareRank b`
                           -- with O(1) bitwise array lookup `isOrthogonallyAligned`. Avoids coordinate ops.
                           let isOrth = isOrthogonallyAligned enemyKingSq slider
                               isCompatible = if isOrth then testBit myRooks (unSquare slider) else testBit myBishops (unSquare slider)
                           in if not isCompatible then acc
                              else
                                  let blockers = between enemyKingSq slider .&. occ
                                  -- If exactly one blocker and it is ours, it's a discovery candidate
                                  in if (blockers .&. friends /= 0) && (blockers .&. (blockers - 1)) == 0
                                     then acc .|. blockers
                                     else acc
            in foldBitboard checkSlider 0 mySliders

-- | Check if three squares are collinear.
-- | Context-Aware Legality Check
{-# INLINE isLegalSafe #-}
isLegalSafe :: Board -> GameState -> Bitboard -> GenMove -> Bool
isLegalSafe b gs pinned gm =
    let t = getTag gm
    in case () of
        _ | t == tagQuiet || t == tagCapture ->
            let pt = getPiece gm
            in if pt == King then isLegal b gs gm
               else checkPinned (getFrom gm) (getTo gm)
          | t == tagPromotion || t == tagPromotionCapture ->
            checkPinned (getFrom gm) (getTo gm)
          | t == tagEnPassant || t == tagCastling || t == tagCastling960 ->
            isLegal b gs gm
          | t == tagDrop -> True
          | otherwise -> isLegal b gs gm
  where
    c = turn gs
    kingSq = if hasKing b c then kingSquareFast b c else Square 0

    checkPinned from to =
        if not (testBit pinned (unSquare from))
        then True
        else isCollinear kingSq from to

-- | Iterate over a bitboard in a Builder Monad (or any Monad)
{-# INLINE forBitboard #-}
forBitboard :: Monad m => Bitboard -> (Square -> m ()) -> m ()
forBitboard bb f = foldBitboardM (\_ sq -> f sq) () bb

-- | Check if a move is legal.
isLegal :: Board -> GameState -> GenMove -> Bool
isLegal b gs gm =
    let t = getTag gm
    in if t == tagQuiet || t == tagCapture || t == tagPromotion || t == tagPromotionCapture || t == tagEnPassant
       then isLegalFast b gs gm
       else isLegalSlow b gs gm

{-# INLINE isLegalFast #-}
isLegalFast :: Board -> GameState -> GenMove -> Bool
isLegalFast b gs gm =
    let t = getTag gm
        from = getFrom gm
        to = getTo gm
        fromI = unSquare from
        toI = unSquare to
        c = turn gs
        opp = oppositeColor c
        pt = getPiece gm
    in case () of
        _ | t == tagQuiet ->
            let occ = (occupiedTotal b `clearBit` fromI) `setBit` toI
            in if pt == King
               then not (isAttackedByOptimized b opp to occ 0)
               else if not (hasKing b c) then True
               else not (isAttackedByOptimized b opp (kingSquareFast b c) occ 0)

          | t == tagCapture ->
            let occ = occupiedTotal b `clearBit` fromI
                ignored = bit toI
            in if pt == King
               then not (isAttackedByOptimized b opp to occ ignored)
               else if not (hasKing b c) then True
               else not (isAttackedByOptimized b opp (kingSquareFast b c) occ ignored)

          | t == tagPromotion ->
            let occ = (occupiedTotal b `clearBit` fromI) `setBit` toI
            in if not (hasKing b c) then True
               else not (isAttackedByOptimized b opp (kingSquareFast b c) occ 0)

          | t == tagPromotionCapture ->
            let occ = occupiedTotal b `clearBit` fromI
                ignored = bit toI
            in if not (hasKing b c) then True
               else not (isAttackedByOptimized b opp (kingSquareFast b c) occ ignored)

          | t == tagEnPassant ->
            let capSqI = if c == White then toI - 8 else toI + 8
                occ = ((occupiedTotal b `clearBit` fromI) `setBit` toI) `clearBit` capSqI
                ignored = bit capSqI
            in if not (hasKing b c) then True
               else not (isAttackedByOptimized b opp (kingSquareFast b c) occ ignored)

          | otherwise -> True -- Should be handled by isLegalSlow

isLegalSlow :: Board -> GameState -> GenMove -> Bool
isLegalSlow b gs gm =
    let b' = applyMoveBoardFast b gs gm
        c = turn gs
        isCastling = getTag gm == tagCastling
    in if not (hasKing b' c) then True
       else
        let k = kingSquareFast b' c
        in not (isAttackedBy b' (oppositeColor c) k) && (if isCastling then castlingSafe b gs gm else True)

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
    let t = getTag gm
        from = getFrom gm
        to = getTo gm
        pt = getPiece gm
        c = turn gs
    in case () of
        _ | t == tagQuiet ->
            movePieceFast b from to c pt

          | t == tagCapture ->
            let capPt = maybe Pawn id (getCapturedPiece gm)
                b1 = unsafeRemovePiece b to (oppositeColor c) capPt
            in movePieceFast b1 from to c pt

          | t == tagEnPassant ->
            let capSq = Square (unSquare to + (if c == White then -8 else 8))
                b1 = unsafeRemovePiece b capSq (oppositeColor c) Pawn
            in movePieceFast b1 from to c Pawn

          | t == tagCastling ->
            let (rookFrom, rookTo) = castlingRookMove from to
                b1 = movePieceFast b from to c King
            in movePieceFast b1 rookFrom rookTo c Rook

          | t == tagPromotion ->
            let b1 = unsafeRemovePiece b from c Pawn
            in unsafePutPiece b1 to (Piece c pt)

          | t == tagPromotionCapture ->
            let capPt = maybe Pawn id (getCapturedPiece gm)
                b1 = unsafeRemovePiece b from c Pawn
                b2 = unsafeRemovePiece b1 to (oppositeColor c) capPt
            in unsafePutPiece b2 to (Piece c pt)

          | otherwise -> b -- Unsupported move type (e.g. GenDrop)

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

{-# INLINE hasKing #-}
hasKing :: Board -> Color -> Bool
hasKing b c = pieceBitboard b c King /= 0

-- | Unsafe: Must only be called if hasKing is true.
{-# INLINE kingSquareFast #-}
kingSquareFast :: Board -> Color -> Square
kingSquareFast b c = Square (fromIntegral (countTrailingZeros (pieceBitboard b c King)))

-- | Safe wrapper for finding the king's square.
{-# DEPRECATED kingSquare "Use hasKing and kingSquareFast instead for performance" #-}
kingSquare :: Board -> Color -> Maybe Square
kingSquare b c =
    if hasKing b c
    then Just (kingSquareFast b c)
    else Nothing

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
        kingSq = if hasKing b oppC then kingSquareFast b oppC else Square 0
        t = getTag gm
        from = getFrom gm
        to = getTo gm
        pt = getPiece gm
    in case () of
        _ | t == tagQuiet || t == tagCapture || t == tagPromotion || t == tagPromotionCapture ->
            givesCheckGeneric b gs c kingSq from to pt

          | t == tagEnPassant ->
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

          | t == tagCastling ->
             let b' = applyMoveBoardFast b gs gm
             in isAttackedBy b' c kingSq

          | otherwise -> False -- Unsupported move type

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
        kingSq = if hasKing b oppC then kingSquareFast b oppC else Square 0
        t = getTag gm
        from = getFrom gm
        to = getTo gm
        pt = getPiece gm
    in case () of
        _ | t == tagQuiet || t == tagCapture || t == tagPromotion || t == tagPromotionCapture ->
            if testBit dcBitboard (unSquare from)
            then givesCheckGeneric b gs c kingSq from to pt
            else givesCheckDirect b gs c kingSq from to pt

          | t == tagEnPassant || t == tagCastling -> givesCheck b gs gm
          | otherwise -> False

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
    let aligned = if isDiag
                  then isDiagonallyAligned kingSq to
                  else isOrthogonallyAligned kingSq to
    in if not aligned
       then False
       else
           let occ = occupiedTotal b
               blockers = between kingSq to .&. occ
               realBlockers = blockers `clearBit` unSquare from
           in realBlockers == 0
