## 2024-05-24 – [Perft Leaf Node Move Counting Optimization]
**Learning:** `perftWhite` and `perftBlack` functions in `Chess.Core.Rules.Class` were evaluating `length (generateMoves game)` for leaf nodes at `depth == 1`. In Standard chess variant, `generateMoves` constructs the full list of moves before computing its length, causing an $O(N)$ allocation of list nodes and boxed moves.
**Action:** Introduced a `countMoves` method to the `ChessVariant` typeclass to bypass generating the full list where possible. Implemented `countMoves` for `Standard` variant to directly return the number of legal moves using `countLegalGenMovesSafe` (or `countLegalGenMoves` when in check), bypassing list allocation entirely for depth 1 leaf node counting.
**Impact:** Minor allocation reduction. On `bench-core` KiwiPete Depth 4: NPS increased slightly.

## 2025-03-26 - Strictness and Inlining in TT Packing/Unpacking
**Learning:** Functions like `packData` and `unpackData` inside hot search paths (`probeTT`, `storeTT`) benefit greatly from `INLINE` pragmas and strictness bang patterns. Without them, GHC might allocate thunks for intermediate `Word64` bitwise operations, putting unnecessary pressure on the GC and slightly increasing pause times in the alpha-beta search loop.
**Action:** When working with struct-like bit-packing operations in Haskell, consistently apply `{-# INLINE #-}` pragmas and use strict let bindings (`let !x = ...`) to ensure the compiler generates unboxed, alloc-free operations.
