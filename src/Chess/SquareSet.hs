module Chess.SquareSet
  ( SquareSet
  , empty
  , full
  , fromList
  , toList
  , fromBitboard
  , toBitboard
  , member
  , insert
  , delete
  , union
  , intersection
  , difference
  , isSubsetOf
  , null
  , size
  , mirror
  , carryRipplerSquares
  , popSquare
  , ray
  , between
  , fromSquare
  ) where

import Prelude hiding (null)

import Data.Bits
import Data.List (foldl')

import Chess.Types (Square(..), squareMirror)
import Chess.Bitboard
  ( Bitboard
  , BB_EMPTY
  , BB_ALL
  , bbFromSquare
  , lsb
  , scanForward
  , popcount
  , ray as bbRay
  , between as bbBetween
  )

newtype SquareSet = SquareSet { unSquareSet :: Bitboard }
  deriving (Eq)

instance Show SquareSet where
  show ss = show (toList ss)

instance Semigroup SquareSet where
  (<>) = union

instance Monoid SquareSet where
  mempty = empty
  mappend = (<>)

empty :: SquareSet
empty = SquareSet BB_EMPTY

full :: SquareSet
full = SquareSet BB_ALL

fromBitboard :: Bitboard -> SquareSet
fromBitboard = SquareSet

toBitboard :: SquareSet -> Bitboard
toBitboard (SquareSet bb) = bb

fromList :: [Square] -> SquareSet
fromList = SquareSet . foldl' (.|.) 0 . map bbFromSquare

toList :: SquareSet -> [Square]
toList (SquareSet bb) = map Square (scanForward bb)

member :: Square -> SquareSet -> Bool
member (Square i) (SquareSet bb) = testBit bb i

insert :: Square -> SquareSet -> SquareSet
insert (Square i) (SquareSet bb) = SquareSet (setBit bb i)

delete :: Square -> SquareSet -> SquareSet
delete (Square i) (SquareSet bb) = SquareSet (clearBit bb i)

union :: SquareSet -> SquareSet -> SquareSet
union (SquareSet a) (SquareSet b) = SquareSet (a .|. b)

intersection :: SquareSet -> SquareSet -> SquareSet
intersection (SquareSet a) (SquareSet b) = SquareSet (a .&. b)

difference :: SquareSet -> SquareSet -> SquareSet
difference (SquareSet a) (SquareSet b) = SquareSet (a .&. complement b)

isSubsetOf :: SquareSet -> SquareSet -> Bool
isSubsetOf (SquareSet a) (SquareSet b) = (a .&. complement b) == 0

null :: SquareSet -> Bool
null (SquareSet bb) = bb == BB_EMPTY

size :: SquareSet -> Int
size (SquareSet bb) = popcount bb

mirror :: SquareSet -> SquareSet
mirror (SquareSet bb) = fromList $ map (squareMirror . Square) (scanForward bb)

carryRipplerSquares :: SquareSet -> [SquareSet]
carryRipplerSquares (SquareSet bb) = map SquareSet (go bb)
  where
    go 0 = [0]
    go s = s : go ((s - 1) .&. bb)

popSquare :: SquareSet -> Maybe (Square, SquareSet)
popSquare (SquareSet bb) = do
  i <- lsb bb
  let bb' = clearBit bb i
  return (Square i, SquareSet bb')

ray :: Square -> Square -> SquareSet
ray a b = SquareSet (bbRay a b)

between :: Square -> Square -> SquareSet
between a b = SquareSet (bbBetween a b)

fromSquare :: Square -> SquareSet
fromSquare sq = SquareSet (bbFromSquare sq)

