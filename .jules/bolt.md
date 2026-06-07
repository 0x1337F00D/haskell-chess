## 2024-05-24 – [Perft Leaf Node Move Counting Optimization]
**Learning:** `perftWhite` and `perftBlack` functions in `Chess.Core.Rules.Class` were evaluating `length (generateMoves game)` for leaf nodes at `depth == 1`. In Standard chess variant, `generateMoves` constructs the full list of moves before computing its length, causing an $O(N)$ allocation of list nodes and boxed moves.
**Action:** Introduced a `countMoves` method to the `ChessVariant` typeclass to bypass generating the full list where possible. Implemented `countMoves` for `Standard` variant to directly return the number of legal moves using `countLegalGenMovesSafe` (or `countLegalGenMoves` when in check), bypassing list allocation entirely for depth 1 leaf node counting.
**Impact:** Minor allocation reduction. On `bench-core` KiwiPete Depth 4: NPS increased slightly.

## 2026-06-06 – [Unboxed TT Probe]
**Learning:** `probeTT` returning `Maybe (Move, Int, Depth, TTFlag)` causes allocation in the hot search path. Unpacking the 64-bit value inside `probeTT` forces GHC to allocate a boxed `Maybe` and a tuple on the heap.
**Action:** Changed `probeTT` to `probeTTFast`, returning the unboxed `Word64` TT data directly. Used `maxBound` as a sentinel for a cache miss. In the search hot loop (`alphaBetaBody` and `quiescence`), check for `maxBound` and then selectively unpack if necessary, avoiding boxed tuple and `Maybe` allocations on cache misses, which heavily dominate.
