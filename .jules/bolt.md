## 2025-05-20 – [Attack Table Vectorization]
**Learning:** Found that attack tables (knight, king, pawn) were implemented as standard Haskell lists `[Bitboard]`. Lookup was `!!` (O(n)). This is a catastrophic bottleneck for chess engines where attack lookups happen millions of times per second.
**Action:** Always check `O(1)` access structures for static lookup tables. Replaced with `Data.Vector.Unboxed`. Measured ~100x speedup on lookups (4.15s -> 0.04s for 10M lookups).
