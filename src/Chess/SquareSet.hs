{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Chess.SquareSet where

import Data.Bits (Bits, (.|.), (.&.), setBit, clearBit, testBit, popCount)
import qualified Data.List as L
import Chess.Types (Square(..))
import qualified Chess.Bitboard as BB

-- | A set of squares.
newtype SquareSet = SquareSet { unSquareSet :: BB.Bitboard }
  deriving stock (Eq, Ord, Show)
  deriving newtype (Bits)

instance Semigroup SquareSet where
  (<>) = union

instance Monoid SquareSet where
  mempty = empty

-- | An empty set of squares.
empty :: SquareSet
empty = SquareSet BB.BB_EMPTY

-- | A set containing all squares.
full :: SquareSet
full = SquareSet BB.BB_ALL

-- | Create a set containing a single square.
singleton :: Square -> SquareSet
singleton s = SquareSet (BB.bbFromSquare s)

-- | Create a set from a list of squares.
fromList :: [Square] -> SquareSet
fromList = L.foldl' (flip insert) empty

-- | Convert a set to a list of squares in ascending order.
toList :: SquareSet -> [Square]
toList (SquareSet bb) = map Square (BB.scanForward bb)

-- | Insert a square into the set.
insert :: Square -> SquareSet -> SquareSet
insert s (SquareSet bb) = SquareSet (bb `setBit` (unSquare s))

-- | Delete a square from the set.
delete :: Square -> SquareSet -> SquareSet
delete s (SquareSet bb) = SquareSet (bb `clearBit` (unSquare s))

-- | Check if a square is in the set.
member :: Square -> SquareSet -> Bool
member s (SquareSet bb) = bb `testBit` (unSquare s)

-- | Union of two sets.
union :: SquareSet -> SquareSet -> SquareSet
union (SquareSet a) (SquareSet b) = SquareSet (a .|. b)

-- | Intersection of two sets.
intersection :: SquareSet -> SquareSet -> SquareSet
intersection (SquareSet a) (SquareSet b) = SquareSet (a .&. b)

-- | Difference of two sets (elements in first but not in second).
difference :: SquareSet -> SquareSet -> SquareSet
difference (SquareSet a) (SquareSet b) = SquareSet (a BB..&~. b)

-- | Number of squares in the set.
size :: SquareSet -> Int
size (SquareSet bb) = popCount bb

-- | Check if the set is empty.
null :: SquareSet -> Bool
null (SquareSet bb) = bb == 0

-- | Check if the first set is a subset of the second.
isSubsetOf :: SquareSet -> SquareSet -> Bool
isSubsetOf (SquareSet a) (SquareSet b) = (a .&. b) == a
