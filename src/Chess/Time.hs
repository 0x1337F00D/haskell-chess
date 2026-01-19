module Chess.Time
    ( Ms
    , TimeControl(..)
    , Clock(..)
    , mkClock
    , updateClock
    , isFlagged
    , clockOutcome
    ) where

import Chess.Types (Color(..), Outcome(..), Termination(..))

-- | Time in milliseconds.
type Ms = Int

-- | Configuration for time controls.
data TimeControl
    = Infinite
    | Standard
        { tcInitial   :: !Ms
        , tcIncrement :: !Ms
        }
    | Delay
        { tcInitial   :: !Ms
        , tcDelay     :: !Ms
        , tcIncrement :: !Ms
        }
    | MoveTime
        { tcPerMove   :: !Ms
        }
    deriving (Show, Eq)

-- | State of the clock.
data Clock = Clock
    { cWhiteTime :: !Ms
    , cBlackTime :: !Ms
    , cConfig    :: !TimeControl
    } deriving (Show, Eq)

-- | Initialize a clock from a time control configuration.
mkClock :: TimeControl -> Clock
mkClock tc = case tc of
    Infinite     -> Clock 0 0 Infinite
    Standard t _ -> Clock t t tc
    Delay t _ _  -> Clock t t tc
    MoveTime t   -> Clock t t tc

-- | Update the clock after a move by the given color.
-- 'elapsed' is the time spent on the move in milliseconds.
-- Returns the updated clock.
updateClock :: Clock -> Color -> Ms -> Clock
updateClock c@(Clock _ _ Infinite) _ _ = c
updateClock c@(Clock w b tc@(Standard _ inc)) col elapsed =
    case col of
        White -> c { cWhiteTime = w - elapsed + inc }
        Black -> c { cBlackTime = b - elapsed + inc }
updateClock c@(Clock w b tc@(Delay _ delay inc)) col elapsed =
    let effectiveElapsed = max 0 (elapsed - delay)
    in case col of
        White -> c { cWhiteTime = w - effectiveElapsed + inc }
        Black -> c { cBlackTime = b - effectiveElapsed + inc }
updateClock c@(Clock w b tc@(MoveTime perMove)) col elapsed =
    let
        oldTime = case col of White -> w; Black -> b
        remaining = oldTime - elapsed
    in
        if remaining < 0
        then case col of
                White -> c { cWhiteTime = remaining }
                Black -> c { cBlackTime = remaining }
        else case col of
                White -> c { cWhiteTime = perMove }
                Black -> c { cBlackTime = perMove }

-- | Check if a color has flagged (ran out of time).
isFlagged :: Clock -> Color -> Bool
isFlagged (Clock _ _ Infinite) _ = False
isFlagged (Clock w _ _) White = w < 0
isFlagged (Clock _ b _) Black = b < 0

-- | Get outcome based on clock.
clockOutcome :: Clock -> Color -> Maybe Outcome
clockOutcome c col =
    if isFlagged c col
    then Just (Outcome Timeout (Just (oppositeColor col)))
    else Nothing

oppositeColor :: Color -> Color
oppositeColor White = Black
oppositeColor Black = White
