module Chess.NNUE.FeatureDelta
  ( AccDelta(..)
  , generateDelta
  ) where

import Chess.Types
import Chess.Board.Base
import Chess.NNUE.Feature (featureIndex, Perspective(..))
import Data.Bits (countTrailingZeros)
import Chess.Board.MoveGen.Common
import Data.Maybe (fromMaybe)

data AccDelta = AccDelta
  { fullRefresh :: !Bool
  , removedW    :: ![Int]
  , addedW      :: ![Int]
  , removedB    :: ![Int]
  , addedB      :: ![Int]
  } deriving (Show, Eq)

-- | Generates an `AccDelta` for a given move.
-- If the move is a King move or Castling, we must fully refresh since HalfKP indices depend on the King square.
generateDelta :: Board -> GenMove -> AccDelta
generateDelta b m =
  let
    wKs = Square (countTrailingZeros (whiteKings b))
    bKs = Square (countTrailingZeros (blackKings b))
  in case m of
    GenQuiet fromSq toSq pt ->
      if pt == King then AccDelta True [] [] [] []
      else
        let c = fromMaybe White (colorAt b fromSq)
            remW = [featureIndex WhiteP wKs c pt fromSq]
            addW = [featureIndex WhiteP wKs c pt toSq]
            remB = [featureIndex BlackP bKs c pt fromSq]
            addB = [featureIndex BlackP bKs c pt toSq]
        in AccDelta False remW addW remB addB

    GenCapture fromSq toSq pt capPt ->
      if pt == King then AccDelta True [] [] [] []
      else
        let c = fromMaybe White (colorAt b fromSq)
            capC = oppositeColor c
            remW = [featureIndex WhiteP wKs c pt fromSq, featureIndex WhiteP wKs capC capPt toSq]
            addW = [featureIndex WhiteP wKs c pt toSq]
            remB = [featureIndex BlackP bKs c pt fromSq, featureIndex BlackP bKs capC capPt toSq]
            addB = [featureIndex BlackP bKs c pt toSq]
        in AccDelta False remW addW remB addB

    GenEnPassant fromSq toSq ->
      let pt = Pawn
          c = fromMaybe White (colorAt b fromSq)
          capC = oppositeColor c
          capSq = Square (if c == White then unSquare toSq - 8 else unSquare toSq + 8)
          remW = [featureIndex WhiteP wKs c pt fromSq, featureIndex WhiteP wKs capC Pawn capSq]
          addW = [featureIndex WhiteP wKs c pt toSq]
          remB = [featureIndex BlackP bKs c pt fromSq, featureIndex BlackP bKs capC Pawn capSq]
          addB = [featureIndex BlackP bKs c pt toSq]
      in AccDelta False remW addW remB addB

    _ -> AccDelta True [] [] [] []
