## 2024-05-24 – [Perft Leaf Node Move Counting Optimization]
**Learning:** `perftWhite` and `perftBlack` functions in `Chess.Core.Rules.Class` were evaluating `length (generateMoves game)` for leaf nodes at `depth == 1`. In Standard chess variant, `generateMoves` constructs the full list of moves before computing its length, causing an $O(N)$ allocation of list nodes and boxed moves.
**Action:** Introduced a `countMoves` method to the `ChessVariant` typeclass to bypass generating the full list where possible. Implemented `countMoves` for `Standard` variant to directly return the number of legal moves using `countLegalGenMovesSafe` (or `countLegalGenMoves` when in check), bypassing list allocation entirely for depth 1 leaf node counting.
**Impact:** Minor allocation reduction. On `bench-core` KiwiPete Depth 4: NPS increased slightly.

## 2024-05-20 - Unboxed Fast Path for Transposition Table Probes
**Learning:** In highly trafficked search functions (e.g., `alphaBetaBody`), returning a boxed `Maybe (Move, Int, Depth, TTFlag)` from `probeTT` forces heap allocations and GC pressure for every node visited. Since `packData` reliably produces a non-zero `Word64` for valid TT entries, `0` can serve as an unboxed sentinel value indicating a cache miss.
**Action:** Replace `probeTT` calls in search hot paths with an `INLINE` `probeTTFast :: TT -> Word64 -> IO Word64` that returns `0` on miss. Explicitly check for `0` before applying `unpackData` to eliminate `Maybe` allocation overhead and significantly reduce garbage collector pauses.
