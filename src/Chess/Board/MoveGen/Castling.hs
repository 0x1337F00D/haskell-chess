{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE BangPatterns #-}

module Chess.Board.MoveGen.Castling where

import Data.Bits
import Control.Monad (when)
import qualified Data.Vector.Unboxed as U

import Chess.Types
import Chess.Board.Base
import Chess.Board.GameState
import Chess.Internal.Builder
import Chess.Board.MoveGen.Common

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
