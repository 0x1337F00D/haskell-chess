## 2024-05-24 – [Perft Leaf Node Move Counting Optimization]
**Learning:** `perftWhite` and `perftBlack` functions in `Chess.Core.Rules.Class` were evaluating `length (generateMoves game)` for leaf nodes at `depth == 1`. In Standard chess variant, `generateMoves` constructs the full list of moves before computing its length, causing an $O(N)$ allocation of list nodes and boxed moves.
**Action:** Introduced a `countMoves` method to the `ChessVariant` typeclass to bypass generating the full list where possible. Implemented `countMoves` for `Standard` variant to directly return the number of legal moves using `countLegalGenMovesSafe` (or `countLegalGenMoves` when in check), bypassing list allocation entirely for depth 1 leaf node counting.
**Impact:** Minor allocation reduction. On `bench-core` KiwiPete Depth 4: NPS increased slightly.

## 2024-05-25 – [TT Fast Probing to eliminate Maybe Boxing]
**Learning:** `probeTT` was returning `Maybe (Move, Int, Depth, TTFlag)`. In hot search paths, returning this complex boxed type and immediately unpacking it causes significant memory allocation and garbage collection overhead. GHC fails to fully unbox the `Maybe` wrapper across module boundaries despite `{-# INLINE #-}`.
**Action:** Introduced `probeTTFast` returning an unboxed/inlined tuple `(Word64, Word64)` of `(Key, Data)`. Replaced `probeTT` calls in `AlphaBeta.hs` and `Quiescence.hs` with `probeTTFast`, and manually unpacked with `unpackDataFast` only when `Key == hash`.
**Impact:** `bench-search` on KiwiPete Depth 6 time reduced from 7.20s to 4.11s (~43% faster elapsed time). Total heap allocations slightly reduced, but max pause and avg pause in GC improved significantly.
