{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Chess.Core.Rules.Perft where

import Chess.Core.Board.Internal (Color(..), KnownColor(..), SColor(..), sColor)
import Chess.Core.Game.Internal (ActiveGame, Variant)
import Chess.Core.Move.Internal (MoveResult(..))
import Chess.Core.Rules.Color (Opposite)
import Chess.Core.Rules.Execution (VariantMoveExecute(..))
import Chess.Core.Rules.Generation (VariantMoveGen(..))
import Control.Parallel.Strategies (parMap, rseq)
import qualified Data.List as List

-- | Perft capability for a chess variant.
class (VariantMoveGen v, VariantMoveExecute v) => VariantPerft (v :: Variant) where
  -- | Perft (Performance Test) for this variant.
  -- Returns the number of leaf nodes at the given depth.
  -- Has a default implementation using perftExecuteMove, but can be optimized.
  perftVariant :: (KnownColor c, KnownColor (Opposite c)) => Int -> ActiveGame v c s -> Int
  default perftVariant :: forall c s. (KnownColor c, KnownColor (Opposite c)) => Int -> ActiveGame v c s -> Int
  perftVariant depth game = case sColor @c of
    SWhite -> perftWhite depth game
    SBlack -> perftBlack depth game

perftWhite :: VariantPerft v => Int -> ActiveGame v 'White s -> Int
perftWhite depth game
  | depth == 0 = 1
  | depth == 1 = countMoves game
  | depth >= 3 = sum $ parMap rseq go (generateMoves game)
  | otherwise = List.foldl' (\acc m -> acc + go m) 0 (generateMoves game)
  where
    go m = case perftExecuteMove m game of
             Continue nextGame -> perftBlack (depth - 1) nextGame
             _ -> 0

perftBlack :: VariantPerft v => Int -> ActiveGame v 'Black s -> Int
perftBlack depth game
  | depth == 0 = 1
  | depth == 1 = countMoves game
  | depth >= 3 = sum $ parMap rseq go (generateMoves game)
  | otherwise = List.foldl' (\acc m -> acc + go m) 0 (generateMoves game)
  where
    go m = case perftExecuteMove m game of
             Continue nextGame -> perftWhite (depth - 1) nextGame
             _ -> 0
