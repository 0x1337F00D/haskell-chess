module Chess.Core.Rules
  ( module Chess.Core.Rules.Class
  , module Chess.Core.Rules.Common
  , module Chess.Core.Rules.Standard
  , module Chess.Core.Rules.Crazyhouse
  , module Chess.Core.Rules.FischerRandom
  ) where

import Chess.Core.Rules.Class
import Chess.Core.Rules.Common
import Chess.Core.Rules.Standard
import Chess.Core.Rules.Crazyhouse
import Chess.Core.Rules.FischerRandom

-- Import variants for instances
import Chess.Core.Rules.ThreeCheck ()
import Chess.Core.Rules.Atomic ()
import Chess.Core.Rules.KingOfTheHill ()
import Chess.Core.Rules.RacingKings ()
