## 2025-05-20 – [Attack Table Vectorization]
**Learning:** Found that attack tables (knight, king, pawn) were implemented as standard Haskell lists `[Bitboard]`. Lookup was `!!` (O(n)). This is a catastrophic bottleneck for chess engines where attack lookups happen millions of times per second.
**Action:** Always check `O(1)` access structures for static lookup tables. Replaced with `Data.Vector.Unboxed`. Measured ~100x speedup on lookups (4.15s -> 0.04s for 10M lookups).

## 2026-01-18 – [Precomputed Sliding Attacks]
**Learning:** Sliding piece attacks (Bishop/Rook) were using an iterative `attackRay` function that scanned the board bit by bit. This caused excessive allocations (40% of total) due to `setBit` and recursion.
**Action:** Implemented **Precomputed Ray Lookups**. Pre-calculated bitboards for all rays from all squares. Replaced iteration with O(1) bitwise logic: `attacks = ray(sq, dir) `xor` ray(blocker, dir)`. Reduced perft allocations by ~36%. Always prefer precomputed masks + bitwise ops over iteration for bitboards.

## 2026-01-26 – [Incremental Occupancy Updates]
**Learning:** `putPiece` was calling `updateOccupancy`, which reconstructed all occupancy bitboards from scratch (12 bitwise ORs) for every piece placement. This occurred inside the `isLegal` check for every pseudo-legal move, creating significant overhead (~12 * 35 ops per node).
**Action:** Replaced full reconstruction with incremental updates using `setBit`. Since we know which piece is added, we only update the relevant piece bitboard and the occupancy bitboards. Improved perft speed by ~3.3%. Avoid full state reconstruction in hot paths when incremental updates are possible.

## 2026-01-27 – [Unsafe Vector Indexing]
**Learning:** `Data.Vector.Unboxed.!` performs bounds checking. In hot paths like evaluation (PST lookup) and move generation (attack lookups), inputs are guaranteed to be valid `Square`s (0-63). The bounds checks added measurable overhead.
**Action:** Replaced `!` with `unsafeIndex` in `Chess.Bitboard` (attack tables) and `Chess.Engine.Evaluation` (PST). Verified ~3% speedup on search benchmark. Use `unsafeIndex` when index validity is structurally guaranteed.

## 2026-01-28 – [GenMove Propagation]
**Learning:** `Search` was calling `legalMoves` (returning `[Move]`) which stripped `PieceType` and capture info. `orderMoves` then re-calculated `isCapture` using `pieceAt` (slow). `applyMove` then re-calculated `findPieceType` (slow).
**Action:** Propagated `GenMove` (which contains `PieceType` and `Maybe CapturedPieceType`) all the way from Move Generation to Search and Move Application. Avoid re-resolving piece info in hot paths. Improved search speed by ~7.5%.

## 2026-01-29 – [Board Update & Square Cache]
**Learning:** `applyMoveBase` was using `removePieceAt` which clears all 15 bitboards, performing redundant operations when the piece type is known. Also, `fromSquare` allocated new `Square` objects (ADT) for every move generation, increasing GC pressure.
**Action:** Optimized `applyMoveBase` to use `unsafeRemovePiece` (reducing bitwise ops from ~15 to ~3) and implemented `fromSquare` memoization using a static `Vector` to return shared `Square` references. Measured ~1.2% speedup on KiwiPete perft.

## 2026-01-29 – [ApplyMove Optimization]
**Learning:** `Chess.Core.Rules.Common.applyMoveBase` was performing two bitboard updates (remove + put) for every move, even for quiet moves. Each update involved multiple bitwise operations and record updates. `Chess.Board.MoveGen` already had a fast path (`movePieceFast`) using XOR masks.
**Action:** Implemented `unsafeMovePiece` in `Chess.Board.Base` using XOR masks to perform move updates in a single pass. Refactored `applyMoveBase` to use this for `QuietMove` and `CaptureMove`. Measured ~4.5% speedup on KiwiPete perft in `bench-core`.

## 2026-01-30 – [Attack Detection Optimization]
**Learning:** `isAttackedBy` (hot path for move legality) was accessing `pieceBitboard` 5-10 times per call. `pieceBitboard` performed a case analysis on `PieceType` and `Color` every time. Additionally, `occupiedTotal` and queen bitboards were recomputed or re-accessed repeatedly within the logical OR chain.
**Action:** Refactored `isAttackedBy` to use top-level pattern matching on `Color` and direct record field access. Factored out `occupiedTotal` and `queen` bitboard lookups. Reduced dispatch overhead and redundant memory accesses. Measured ~5.5% speedup on KiwiPete perft and ~8.8% speedup on Search.
