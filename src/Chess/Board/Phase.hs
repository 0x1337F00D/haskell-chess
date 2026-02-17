{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}

module Chess.Board.Phase where

import Data.Kind (Type)
import Data.Bits (popCount)
import qualified Chess.Board.Base as Base
import Chess.Board (Board(..), ValidatedBoard, getBoard)
import Chess.Board.GameState (GameState(..), fullmoveNumber)
import Chess.Types (FullmoveNumber(..))

-- | Game Phases
data Phase = Opening | Middlegame | Endgame
  deriving (Show, Eq)

-- | A Phase-Indexed Position
newtype Position (p :: Phase) = Position ValidatedBoard
  deriving (Show, Eq)

-- | Singleton for Phase
data SPhase (p :: Phase) where
  SOpening :: SPhase 'Opening
  SMiddlegame :: SPhase 'Middlegame
  SEndgame :: SPhase 'Endgame

deriving instance Show (SPhase p)
deriving instance Eq (SPhase p)

-- | Existential wrapper for a Position with its Phase
data SomePhase where
  SomePhase :: SPhase p -> Position p -> SomePhase

-- | Classify the position into a phase.
classifyPhase :: ValidatedBoard -> SomePhase
classifyPhase vb =
    let b = getBoard vb
        base = pieces b
        gs = state b

        -- Phase weights (from Tapered Evaluation)
        phaseKnight = 1
        phaseBishop = 1
        phaseRook = 2
        phaseQueen = 4
        -- totalPhase = 24

        wn = popCount (Base.whiteKnights base)
        wb = popCount (Base.whiteBishops base)
        wr = popCount (Base.whiteRooks base)
        wq = popCount (Base.whiteQueens base)
        bn = popCount (Base.blackKnights base)
        bb = popCount (Base.blackBishops base)
        br = popCount (Base.blackRooks base)
        bq = popCount (Base.blackQueens base)

        phase = wn * phaseKnight + wb * phaseBishop + wr * phaseRook + wq * phaseQueen +
                bn * phaseKnight + bb * phaseBishop + br * phaseRook + bq * phaseQueen

        fm = unFullmoveNumber (fullmoveNumber gs)

    in if fm < 10
       then SomePhase SOpening (Position vb)
       else if phase < 10
            then SomePhase SEndgame (Position vb) -- Phase < 10 is deep endgame
            else SomePhase SMiddlegame (Position vb)
