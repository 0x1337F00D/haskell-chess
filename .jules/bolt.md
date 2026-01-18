## 2025-05-20 – [Attack Table Vectorization]
**Learning:** Found that attack tables (knight, king, pawn) were implemented as standard Haskell lists `[Bitboard]`. Lookup was `!!` (O(n)). This is a catastrophic bottleneck for chess engines where attack lookups happen millions of times per second.
**Action:** Always check `O(1)` access structures for static lookup tables. Replaced with `Data.Vector.Unboxed`. Measured ~100x speedup on lookups (4.15s -> 0.04s for 10M lookups).

## 2026-01-18 – [Precomputed Sliding Attacks]
**Learning:** Sliding piece attacks (Bishop/Rook) were using an iterative `attackRay` function that scanned the board bit by bit. This caused excessive allocations (40% of total) due to `setBit` and recursion.
**Action:** Implemented **Precomputed Ray Lookups**. Pre-calculated bitboards for all rays from all squares. Replaced iteration with O(1) bitwise logic: `attacks = ray(sq, dir) `xor` ray(blocker, dir)`. Reduced perft allocations by ~36%. Always prefer precomputed masks + bitwise ops over iteration for bitboards.
