## 2024-05-24 – [Perft Leaf Node Move Counting Optimization]
**Learning:** `perftWhite` and `perftBlack` functions in `Chess.Core.Rules.Class` were evaluating `length (generateMoves game)` for leaf nodes at `depth == 1`. In Standard chess variant, `generateMoves` constructs the full list of moves before computing its length, causing an $O(N)$ allocation of list nodes and boxed moves.
**Action:** Introduced a `countMoves` method to the `ChessVariant` typeclass to bypass generating the full list where possible. Implemented `countMoves` for `Standard` variant to directly return the number of legal moves using `countLegalGenMovesSafe` (or `countLegalGenMoves` when in check), bypassing list allocation entirely for depth 1 leaf node counting.
**Impact:** Minor allocation reduction. On `bench-core` KiwiPete Depth 4: NPS increased slightly.
## 2024-05-24 - [TT Fast Probe Optimization]
**Learning:** `probeTT` was allocating a `Maybe` for every Transposition Table hit/miss, causing significant garbage collection overhead and boxing overhead in the hottest code path of the search loop.
**Action:** Introduced `probeTTFast` in `Chess.Engine.TT` which returns an unboxed `Word64` containing the packed TT data, using `maxBound` as a sentinel value for a cache miss. Updated the search hot paths (`AlphaBeta.hs` and `Quiescence.hs`) to use `probeTTFast` and avoid the boxed `Maybe` tuple allocations entirely.
**Impact:** Reduced intermediate allocations in AlphaBeta search, improving Search nodes-per-second.
