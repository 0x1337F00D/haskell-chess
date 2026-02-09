{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Chess.Core.Game
  ( -- * Phases & Variants
    Phase(..)
  , Variant(..)
  , CheckStatus(..)
  , Outcome(..)
  , CastlingRights(..)
    -- * Game State
  , ActiveGame -- Opaque
  , Game -- Opaque
  ) where

import Chess.Types (CheckStatus(..))
import Chess.Core.Game.Internal
