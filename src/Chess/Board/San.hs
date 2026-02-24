{-# LANGUAGE PatternSynonyms #-}
module Chess.Board.San where

import Data.List (find)
import Data.Maybe (isJust)
import Data.Bits ((.&.), complement, (.|.), testBit)

import Chess.Types
import Chess.Bitboard (bbFromSquare, pattern BB_A1, pattern BB_H1, pattern BB_A8, pattern BB_H8, scanForward, pawnAttacks)
import Chess.Board.Base
import Chess.Board.GameState
import Chess.Board.MoveGen (isLegal, applyMoveBoardFast, pattern GenQuiet, pattern GenCapture, pattern GenEnPassant, pattern GenCastling, pattern GenPromotion, pattern GenPromotionCapture, GenMove)
import Chess.Board.Validation (isCheck, isCheckmate)

-- | Convert a move to Standard Algebraic Notation (SAN).
san :: Board -> Move -> String
san board move@(Move from to promo) =
    let p = pieceAt board from
        c = getTurn (statePacked board)
        isCapture = isJust (pieceAt board to) || isEpCapture board move
    in case p of
        Nothing -> ""
        Just (Piece _ pt) ->
            if pt == King && abs (squareFile from - squareFile to) > 1 then
                if squareFile to > squareFile from then "O-O" else "O-O-O"
            else
                let
                    base = case pt of
                        Pawn ->
                            if isCapture
                            then [fileChar (squareFile from)] ++ "x" ++ squareName to
                            else squareName to
                        _ ->
                            let sym = [pieceSymbol pt]
                                candidates = getCandidates board (Piece c pt) to
                                disamb = disambiguate from candidates
                                capt = if isCapture then "x" else ""
                            in sym ++ disamb ++ capt ++ squareName to

                    promStr = case promo of
                        Just ppt -> "=" ++ [pieceSymbol ppt]
                        Nothing -> ""

                    nextB = applyMove board move
                    suffix = if isCheckmate nextB then "#"
                             else if isCheck nextB then "+"
                             else ""
                in base ++ promStr ++ suffix
san _ _ = ""

-- | Apply move to board and state (minimal version for check detection).
-- Does not update Zobrist or full counters, just enough for validation.
applyMove :: Board -> Move -> Board
applyMove b m@(Move from to _ ) =
    let b' = applyMoveBoardFast b (case mkGenMove b m of Just gm -> gm; Nothing -> GenQuiet from to Pawn) -- Fallback should not happen
        -- We need GenMove. Re-deriving it.

        -- Better: use standard derivation.
        -- But applyMoveBoardFast logic is:
        -- applyMoveBoardFast b gm
        -- It returns b with OLD state.
        -- We need to update state (turn, ep).

        s = statePacked b
        c = getTurn s

        -- Simplified update: Toggle turn.
        -- EP Square update?
        ep = if isDoublePush b from to
             then midSquare from to
             else NoSquare

        s' = mkStatePacked (oppositeColor c) (getCastlingRights s) ep (getHalfmoveClock s) (getFullmoveNumber s)

    in b' { statePacked = s' }
applyMove b _ = b

mkGenMove :: Board -> Move -> Maybe GenMove
mkGenMove b (Move from to promo) =
    let s = statePacked b
        ep = getEpSquare s
        isEp = ep == to && pieceAt b from == Just (Piece (getTurn s) Pawn)

        capPt = fmap pieceType (pieceAt b to)
        pt = fmap pieceType (pieceAt b from)

    in case pt of
        Just pType ->
             case promo of
                Just p -> case capPt of
                            Just cp -> Just (GenPromotionCapture from to p cp)
                            Nothing -> Just (GenPromotion from to p)
                Nothing ->
                    if isEp then Just (GenEnPassant from to)
                    else if pType == King && abs (squareFile from - squareFile to) > 1 then Just (GenCastling from to)
                    else case capPt of
                            Just cp -> Just (GenCapture from to pType cp)
                            Nothing -> Just (GenQuiet from to pType)
        Nothing -> Nothing

isDoublePush :: Board -> Square -> Square -> Bool
isDoublePush b f t =
    let p = pieceAt b f
    in fmap pieceType p == Just Pawn && abs (squareRank f - squareRank t) == 2

midSquare :: Square -> Square -> Square
midSquare f t = Square ((unSquare f + unSquare t) `div` 2)

-- | Optimized candidate finder using bitboards.
getCandidates :: Board -> Piece -> Square -> [Square]
getCandidates b (Piece c pt) target =
    let candidates =
            case pt of
                Pawn -> getPawnCandidates
                _    -> getPieceCandidates
    in filter canMoveTo candidates
  where
    getPieceCandidates = map Square (scanForward (pieceBitboard b c pt))

    getPawnCandidates = map Square (scanForward (pieceBitboard b c Pawn))

    canMoveTo :: Square -> Bool
    canMoveTo from =
        let pseudo = case pt of
                Pawn -> isPawnMove from
                _    -> testBit (attacks b from) (unSquare target)
        in pseudo && case mkGenMove b (Move from target promo) of
                       Just gm -> isLegal b gm
                       Nothing -> False

    promo = if pt == Pawn && isPromotionRank target then Just Queen else Nothing
    isPromotionRank s = (c == White && squareRank s == 7) || (c == Black && squareRank s == 0)

    gs = statePacked b

    isPawnMove :: Square -> Bool
    isPawnMove from =
        -- Capture
        (testBit (pawnAttacks c from) (unSquare target) &&
           (isEpSquare target || (isJust (pieceAt b target) && colorAt b target /= Just c))) ||
        -- Push
        (squareFile from == squareFile target &&
         not (isJust (pieceAt b target)) &&
         (oneStep from == target ||
          (twoStep from == target && isStartRank from && not (isJust (pieceAt b (oneStep from))))))

    oneStep from = Square (unSquare from + (if c == White then 8 else -8))
    twoStep from = Square (unSquare from + (if c == White then 16 else -16))

    isStartRank s = (c == White && squareRank s == 1) || (c == Black && squareRank s == 6)

    isEpSquare t = getEpSquare gs == t

disambiguate :: Square -> [Square] -> String
disambiguate src candidates
    | length candidates == 1 = ""
    | otherwise =
        let sameFile = filter (\s -> squareFile s == squareFile src) candidates
            sameRank = filter (\s -> squareRank s == squareRank src) candidates
        in if length sameFile == 1 then [fileChar (squareFile src)]
           else if length sameRank == 1 then [rankChar (squareRank src)]
           else [fileChar (squareFile src), rankChar (squareRank src)]

fileChar :: Int -> Char
fileChar f = fileNames !! f

rankChar :: Int -> Char
rankChar r = rankNames !! r

isEpCapture :: Board -> Move -> Bool
isEpCapture b (Move from to _ ) =
    case pieceAt b from of
        Just (Piece _ Pawn) ->
             case pieceAt b to of
                 Nothing -> squareFile from /= squareFile to
                 _ -> False
        _ -> False
isEpCapture _ _ = False

-- | Parse SAN string to Move.
parseSan :: Board -> String -> Maybe Move
parseSan b str =
    let cleanStr = filter (`notElem` "+#") str
        c = getTurn (statePacked b)

        -- Helper to check legality of a Move (converting to GenMove first)
        checkLegal m =
            case mkGenMove b m of
                Just gm -> isLegal b gm
                Nothing -> False

        findMatch candidates = find (\m -> checkLegal m && (san b m == str || san b m == cleanStr)) candidates

        rank = if c == White then 0 else 7
        kingSq = Square (rank * 8 + 4)

    in case cleanStr of
        "O-O" ->
            let dest = Square (rank * 8 + 6)
                m = Move kingSq dest Nothing
            in findMatch [m]
        "O-O-O" ->
            let dest = Square (rank * 8 + 2)
                m = Move kingSq dest Nothing
            in findMatch [m]
        _ ->
            let (baseStr, promoStr) = break (== '=') cleanStr
                -- Handle promotion
                promo = if null promoStr then Nothing
                        else charToPieceType (head (tail promoStr))

                (pt, targetStr) =
                     case baseStr of
                        (ch:rest) | ch `elem` "NBRQK" -> (charToPieceType ch, rest)
                        _ -> (Just Pawn, baseStr)

                (targetS) =
                     if length targetStr >= 2
                     then snd (splitAt (length targetStr - 2) targetStr)
                     else targetStr

            in case (pt, parseSquare targetS) of
                (Just pType, Just target) ->
                    let candidates = getCandidates b (Piece c pType) target
                        moves = map (\from -> Move from target promo) candidates
                    in findMatch moves

                _ -> Nothing
