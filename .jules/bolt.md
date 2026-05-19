## 2024-05-24 – [Perft Leaf Node Move Counting Optimization]
**Learning:** `perftWhite` and `perftBlack` functions in `Chess.Core.Rules.Class` were evaluating `length (generateMoves game)` for leaf nodes at `depth == 1`. In Standard chess variant, `generateMoves` constructs the full list of moves before computing its length, causing an $O(N)$ allocation of list nodes and boxed moves.
**Action:** Introduced a `countMoves` method to the `ChessVariant` typeclass to bypass generating the full list where possible. Implemented `countMoves` for `Standard` variant to directly return the number of legal moves using `countLegalGenMovesSafe` (or `countLegalGenMoves` when in check), bypassing list allocation entirely for depth 1 leaf node counting.
**Impact:** Minor allocation reduction. On `bench-core` KiwiPete Depth 4: NPS increased slightly.

## 2024-05-24 – [Inline Struct-Like Bit-Packing in Hot Paths]
**Learning:** Functions like `packData` and `unpackData` used in `probeTT` and `storeTT` for struct-like bit-packing operations can build up thunks or allocate if not rigorously inlined, especially when used within inner loops or deeply nested recursive search functions like Alpha-Beta search.
**Action:** Consistently apply `{-# INLINE #-}` pragmas on bit-packing/unpacking utility functions (like `packData` and `unpackData`) and the TT operations themselves (`probeTT`, `storeTT`) to ensure GHC generates unboxed, alloc-free operations and prevents GC-heavy thunk buildups of intermediate bitwise calculations in search hot paths.
