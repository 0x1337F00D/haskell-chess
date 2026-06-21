## 2024-05-24 – [Perft Leaf Node Move Counting Optimization]
**Learning:** `perftWhite` and `perftBlack` functions in `Chess.Core.Rules.Class` were evaluating `length (generateMoves game)` for leaf nodes at `depth == 1`. In Standard chess variant, `generateMoves` constructs the full list of moves before computing its length, causing an $O(N)$ allocation of list nodes and boxed moves.
**Action:** Introduced a `countMoves` method to the `ChessVariant` typeclass to bypass generating the full list where possible. Implemented `countMoves` for `Standard` variant to directly return the number of legal moves using `countLegalGenMovesSafe` (or `countLegalGenMoves` when in check), bypassing list allocation entirely for depth 1 leaf node counting.
**Impact:** Minor allocation reduction. On `bench-core` KiwiPete Depth 4: NPS increased slightly.
## 2024-05-24 - [Avoid TT Probe Boxed Tuples Allocations in Hot Path]
**Learning:** `probeTT` was unconditionally allocating a boxed tuple `Just (Move, Int, Depth, TTFlag)` regardless of whether the key matched or if it would be used, causing significant GC pressure during search loops.
**Action:** Introduced `probeTTFast` to return the raw `(Word64, Word64)` table entry directly (inlined to unboxed fields), and moved the unpacking to `unpackDataFast` that is called only upon an explicit key match, reducing boxed tuples in hot paths.
**Impact:** Reduced allocations from 6,282,374,016 bytes to 6,123,930,488 bytes (~158 MB saved per test depth 5), lowering GC copying time and slightly increasing performance.
