## 2025-05-20 – [Attack Table Vectorization]
**Learning:** Found that attack tables (knight, king, pawn) were implemented as standard Haskell lists `[Bitboard]`. Lookup was `!!` (O(n)). This is a catastrophic bottleneck for chess engines where attack lookups happen millions of times per second.
**Action:** Always check `O(1)` access structures for static lookup tables. Replaced with `Data.Vector.Unboxed`. Measured ~100x speedup on lookups (4.15s -> 0.04s for 10M lookups).

## 2026-01-18 – [Precomputed Sliding Attacks]
**Learning:** Sliding piece attacks (Bishop/Rook) were using an iterative `attackRay` function that scanned the board bit by bit. This caused excessive allocations (40% of total) due to `setBit` and recursion.
**Action:** Implemented **Precomputed Ray Lookups**. Pre-calculated bitboards for all rays from all squares. Replaced iteration with O(1) bitwise logic: `attacks = ray(sq, dir) `xor` ray(blocker, dir)`. Reduced perft allocations by ~36%. Always prefer precomputed masks + bitwise ops over iteration for bitboards.

## 2026-01-26 – [Incremental Occupancy Updates]
**Learning:** `putPiece` was calling `updateOccupancy`, which reconstructed all occupancy bitboards from scratch (12 bitwise ORs) for every piece placement. This occurred inside the `isLegal` check for every pseudo-legal move, creating significant overhead (~12 * 35 ops per node).
**Action:** Replaced full reconstruction with incremental updates using `setBit`. Since we know which piece is added, we only update the relevant piece bitboard and the occupancy bitboards. Improved perft speed by ~3.3%. Avoid full state reconstruction in hot paths when incremental updates are possible.

## 2026-01-27 – [Zero-Allocation Evaluation Loop]
**Learning:** `evalPSTO` used `scanForward` (which allocates `[Int]`) and a list comprehension (which allocates `[Score]`) to sum values from a `Vector`. This allocated ~64 list nodes (2.3KB) per evaluation. In a search with millions of nodes, this generated gigabytes of garbage.
**Action:** Replaced the list-based fold with a custom recursive loop using `countTrailingZeros` and `clearBit`. This reduced allocations for the evaluation function by 99.98% and improved evaluation speed by 2.2x.
