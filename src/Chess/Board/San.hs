{-# LANGUAGE PatternSynonyms #-}
module Chess.Board.San where

import Data.List (find)
import Data.Maybe (isJust)
import Data.Bits ((.&.), complement, (.|.), testBit)

import Chess.Types
import Chess.Bitboard (bbFromSquare, pattern BB_A1, pattern BB_H1, pattern BB_A8, pattern BB_H8, scanForward, pawnAttacks)
import Chess.Board.Base
import Chess.Board.GameState
import Chess.Board.MoveGen (isLegal, applyMoveBoard, pattern GenQuiet, pattern GenCapture, pattern GenEnPassant, pattern GenCastling, pattern GenPromotion, pattern GenPromotionCapture)
import Chess.Board.Validation (isCheck, isCheckmate)

-- | Convert a move to Standard Algebraic Notation (SAN).
san :: Board -> GameState -> Move -> String
san board gs move@(Move from to promo) =
    let p = pieceAt board from
        c = turn gs
        isCapture = isJust (pieceAt board to) || isEpCapture board gs move
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
                                candidates = getCandidates board gs (Piece c pt) to
                                disamb = disambiguate from candidates
                                capt = if isCapture then "x" else ""
                            in sym ++ disamb ++ capt ++ squareName to

                    promStr = case promo of
                        Just ppt -> "=" ++ [pieceSymbol ppt]
                        Nothing -> ""

                    (nextB, nextGS) = applyMove board gs move
                    suffix = if isCheckmate nextB nextGS then "#"
                             else if isCheck nextB nextGS then "+"
                             else ""
                in base ++ promStr ++ suffix
san _ _ _ = ""

-- | Apply move to board and state (minimal version for check detection).
applyMove :: Board -> GameState -> Move -> (Board, GameState)
applyMove b gs m@(Move from to _ ) =
    let b' = applyMoveBoard b gs m
        p = pieceAt b from
        c = turn gs

        cr = castlingRights gs
        cr1 = case p of
            Just (Piece _ King) ->
                 let mask = if c == White then (BB_A1 .|. BB_H1) else (BB_A8 .|. BB_H8)
                 in cr .&. complement mask
            Just (Piece _ Rook) ->
                 cr .&. complement (bbFromSquare from)
            _ -> cr

        captured = pieceAt b to
        cr2 = case captured of
            Just (Piece _ Rook) -> cr1 .&. complement (bbFromSquare to)
            _ -> cr1

        ep = if isDoublePush b from to
             then midSquare from to
             else NoSquare

        gs' = gs
            { turn = oppositeColor c
            , castlingRights = cr2
            , epSquare = ep
            }
    in (b', gs')
applyMove b gs _ = (b, gs)

isDoublePush :: Board -> Square -> Square -> Bool
isDoublePush b f t =
    let p = pieceAt b f
    in fmap pieceType p == Just Pawn && abs (squareRank f - squareRank t) == 2

midSquare :: Square -> Square -> Square
midSquare f t = Square ((unSquare f + unSquare t) `div` 2)

-- | Optimized candidate finder using bitboards.
getCandidates :: Board -> GameState -> Piece -> Square -> [Square]
getCandidates b gs (Piece c pt) target =
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
        in pseudo && isLegal b gs (mkGenMove from)

    mkGenMove from =
        let isEp = isEpSquare target
            capPt = fmap pieceType (pieceAt b target)
        in case promo of
            Just p -> case capPt of
                        Just cp -> GenPromotionCapture from target p cp
                        Nothing -> GenPromotion from target p
            Nothing ->
                if isEp then GenEnPassant from target
                else case capPt of
                        Just cp -> GenCapture from target pt cp
                        Nothing -> GenQuiet from target pt

    promo = if pt == Pawn && isPromotionRank target then Just Queen else Nothing
    isPromotionRank s = (c == White && squareRank s == 7) || (c == Black && squareRank s == 0)

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

    isEpSquare t = epSquare gs == t

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

isEpCapture :: Board -> GameState -> Move -> Bool
isEpCapture b _ (Move from to _ ) =
    case pieceAt b from of
        Just (Piece _ Pawn) ->
             case pieceAt b to of
                 Nothing -> squareFile from /= squareFile to
                 _ -> False
        _ -> False
isEpCapture _ _ _ = False

-- | Parse SAN string to Move.
parseSan :: Board -> GameState -> String -> Maybe Move
parseSan b gs str =
    let cleanStr = filter (`notElem` "+#") str
        c = turn gs

        -- Helper to check legality of a Move (converting to GenMove first)
        checkLegal m@(Move from to promo) =
            let p = pieceAt b from
                pt = maybe Pawn pieceType p

                isEp = pt == Pawn && isEpCapture b gs m
                isCastling = pt == King && abs (unSquare from - unSquare to) == 2

                capturedPt = fmap pieceType (pieceAt b to)

                gm = if isCastling then GenCastling from to
                     else case promo of
                        Just ppt ->
                            case capturedPt of
                                Just cp -> GenPromotionCapture from to ppt cp
                                Nothing -> GenPromotion from to ppt
                        Nothing ->
                            if isEp then GenEnPassant from to
                            else case capturedPt of
                                Just cp -> GenCapture from to pt cp
                                Nothing -> GenQuiet from to pt
            in isLegal b gs gm
        checkLegal _ = False

        findMatch candidates = find (\m -> checkLegal m && (san b gs m == str || san b gs m == cleanStr)) candidates

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
                    let candidates = getCandidates b gs (Piece c pType) target
                        moves = map (\from -> Move from target promo) candidates
                    in findMatch moves

                _ -> Nothing
