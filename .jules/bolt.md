## 2024-05-24 – [Perft Leaf Node Move Counting Optimization]
**Learning:** `perftWhite` and `perftBlack` functions in `Chess.Core.Rules.Class` were evaluating `length (generateMoves game)` for leaf nodes at `depth == 1`. In Standard chess variant, `generateMoves` constructs the full list of moves before computing its length, causing an $O(N)$ allocation of list nodes and boxed moves.
**Action:** Introduced a `countMoves` method to the `ChessVariant` typeclass to bypass generating the full list where possible. Implemented `countMoves` for `Standard` variant to directly return the number of legal moves using `countLegalGenMovesSafe` (or `countLegalGenMoves` when in check), bypassing list allocation entirely for depth 1 leaf node counting.
**Impact:** Minor allocation reduction. On `bench-core` KiwiPete Depth 4: NPS increased slightly.
## 2024-06-24 – [Eliminate Maybe Allocations in TT Probing]
**Learning:** Returning `Maybe (Move, Int, Depth, TTFlag)` from `probeTT` was forcing heap allocation (boxing) of the 4-tuple and the `Just` constructor on every Transposition Table hit, which is a very hot path during search. Unpacking multi-element tuples across module boundaries causes GC pressure.
**Action:** Replaced `probeTT` with `probeTTFast` which returns an unboxed tuple `(Word64, Word64)` containing the raw TT entry key and data. The caller now checks `entryKey == hash` and manually extracts the needed fields using `unpackData`.
**Impact:** Reduced total memory allocation by ~400MB during `bench-search` on KiwiPete Depth 6, directly decreasing GC pressure.
