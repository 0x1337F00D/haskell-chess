{-# LANGUAGE PatternSynonyms #-}
module Chess.Board.San where

import Data.List (find)
import Data.Maybe (isJust)
import Data.Bits ((.&.), complement, (.|.))

import Chess.Types
import Chess.Bitboard (bbFromSquare, pattern BB_A1, pattern BB_H1, pattern BB_A8, pattern BB_H8)
import Chess.Board.Base
import Chess.Board.GameState
import Chess.Board.MoveGen
import Chess.Board.Validation (isCheck, isCheckmate)

-- | Convert a move to Standard Algebraic Notation (SAN).
san :: Board -> GameState -> Move -> String
san board gs move@(Move (Just from) (Just to) promo _) =
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
applyMove b gs m@(Move (Just from) (Just to) _ _) =
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
             then Just (midSquare from to)
             else Nothing

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

getCandidates :: Board -> GameState -> Piece -> Square -> [Square]
getCandidates b gs p target =
    let moves = legalMoves b gs
    in [ f | Move (Just f) (Just t) _ _ <- moves, t == target, pieceAt b f == Just p ]

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
isEpCapture b gs (Move (Just from) (Just to) _ _) =
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
    let legal = legalMoves b gs
        cleanStr = filter (`notElem` "+#") str
    in find (\m -> san b gs m == str || san b gs m == cleanStr) legal
