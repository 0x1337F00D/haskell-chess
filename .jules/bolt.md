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

## 2026-01-31 – [Manual Record Fusion vs Straight-line Updates]
**Learning:** Attempted to fuse 'remove piece' and 'move piece' operations in 'applyMoveBoardFast' to reduce 'Board' allocations. The resulting function required complex conditional dispatch (12 checks) to construct the record in one go. This proved *slower* (Time doubled) and increased allocation slightly (intermediates?), compared to the original sequential updates. Straight-line updates with simple logic are often better optimized by GHC.
**Action:** Avoid manual fusion of record updates if it introduces significant branching. Trust GHC to optimize sequential updates or accept the small allocation cost for simpler control flow.

## 2026-02-01 – [Bit-Packed GenMove]
**Learning:** `GenMove` (sum type with 6 constructors) was a major source of allocation (~1.6GB) in move generation. Replaced it with a bit-packed `newtype GenMove = MkGenMove Word64` using `PatternSynonyms`. This reduces heap object size significantly (5 words -> 2 words) and enables `Unboxed Vector` storage. However, observed ~15% regression in raw perft NPS due to bitwise packing/unpacking overhead in tight loops.
**Action:** Use bit-packing for data that is stored in bulk (like Move lists), but be aware of the CPU cost of unpacking. Essential for enabling `Unbox` instances to remove list structure overhead in future steps.

## 2026-02-06 – [EpSquare Unboxing]
**Learning:** `GameState` contained `Maybe Square` for `epSquare`. This introduced boxing (pointer indirection + heap object) for every `GameState` created (which is every node in the search tree).
**Action:** Replaced `Maybe Square` with strict `Square` and a sentinel value (`NoSquare = Square 64`). This flattens `GameState` to be purely unboxed fields, reducing GC pressure and memory traffic. Verified ~1.1% speedup on KiwiPete perft.

## 2026-02-06 – [Pawn Move Vectorization]
**Learning:** `pawnMoves` helpers (`pawnQuiets`, `pawnCaptures`, etc.) were using list comprehensions to generate moves before converting to `U.Vector`. This created millions of short-lived list nodes (cons cells + boxed GenMoves) per second, dominating GC.
**Action:** Replaced list comprehensions with `U.create` and a two-pass "count-then-fill" strategy using direct bitwise logic and `M.unsafeWrite`. This eliminates the intermediate list allocation entirely for pawns.
**Impact:** Allocations reduced by ~16.2% (7.6GB saved on benchmark run). Runtime improved by ~9.3%.

## 2026-02-12 – [Unboxed Magic Bitboards]
**Learning:** `Chess.Bitboard` stored Magic Bitboard constants in a boxed `V.Vector Magic`. This caused an extra pointer dereference and cache miss for *every* sliding piece attack lookup (millions/sec). The `Magic` struct itself was small (4 words) but boxed.
**Action:** Defined `Unbox` instance for `Magic` and switched `bbBishopMagics`/`bbRookMagics` to `U.Vector Magic`. This packs the constants contiguously in memory, removing indirection.
**Impact:** `bench-magic` speedup from 52.8 M/s to 68.0 M/s (~28.8%).

## 2026-02-01 – Magic Bitboard Initialization
**Learning:** Initializing Magic Bitboards via brute force with list-based verification (`!!`) is (N^2)$ per trial, leading to massive startup overhead (350s).
**Action:** Always use O(1) random access structures (like Unboxed Vectors) for inner loops in initialization code, even if it runs "only once". "Only once" can be 5 minutes.

## 2026-02-18 – [King Safety Optimization]
**Learning:** `evalKingSafety` was returning a tuple `(Score, Score)` which GHC didn't always unbox, causing heap allocation per node. Also `kingSafety` loop used bounds-checked `!` on a 100-element table.
**Action:** Added `{-# INLINE evalKingSafety #-}` to force unboxing of the tuple via case-of-known-constructor. Refactored `kingSafety` to unpack bitboards directly (avoiding 4 helper calls) and used `unsafeIndex` for table lookup.
**Impact:** Minor/Neutral speedup (~1.03 MNPS -> 1.02 MNPS), but ensures no heap allocation for safety scores. Important: Over-inlining (inlining the loop body `kingSafety` itself) caused code bloat and regression, so only the wrapper was inlined.

## 2026-02-19 – [Fast Legality Check]
**Learning:** `MoveGen.isLegal` was allocating a full `Board` structure (~22 words + heap overhead) via `applyMoveBoardFast` for every pseudo-legal move validation. This happens millions of times per second in search and perft.
**Action:** Implemented `isLegalOptimized` which validates moves by calculating updated occupancy and checking attacks directly on bitboards, masking out captured pieces. This avoids the `Board` allocation entirely for standard moves (Quiet, Capture, EP, Promotion).
**Impact:** `bench-core` speedup: Start (Depth 5) 2.96 -> 3.76 MNPS (+27%), KiwiPete (Depth 4) 5.08 -> 8.10 MNPS (+59%).

## 2026-02-23 – [Flattened PST Tables & Material Merger]
**Learning:** `pstValue` and `packedMaterialValue` introduced significant branching (12 branches) and arithmetic overhead (`score + val + mat`) in the hot `applyMove` loop.
**Action:** Flattened the 12 PST vectors into a single `globalPstTable` (size 768) and merged material values into the table during initialization. This replaced branching with index arithmetic and removed `mat` addition in incremental updates.
**Impact:** `bench-core` speedup: KiwiPete (Depth 4) 33.0 MNPS -> 41.7 MNPS (+26%). Atomic Start (Depth 4) 1.58 MNPS -> 2.20 MNPS (+39%).

## 2026-03-07 – [Discovery Candidates Propagation]
**Learning:** Even though `discoveryCandidates` was precalculated inside `alphaBetaBody`, the top-level search loop (`alphaBetaRoot`) and the parallel worker loop within it were still using the slow `givesCheck` for the first move evaluated and all root moves in the worker. The calculation of `discoveryCandidates` per move in the hot path misses the optimization of performing it once per board state.
**Action:** Hoisted the computation of `dcBitboard` (via `KingSafety.discoveryCandidates`) inside `alphaBetaRoot` and within its parallel worker loops so that all `applyLegalMoveValidated` calls benefit from the `givesCheckOptimized` code path.
**Impact:** Re-ran benchmarks and maintained equivalent/slightly better search NPS with functionally zero additional overhead for the root move computations.

## 2024-05-15 – [Fast Bitwise Single-Bit Check over popCount]
**Learning:** `popCount x == 1` relies on the `popcnt` instruction or its software fallback, which can be unexpectedly slow in a tight loop. Using the bitwise trick `(x .&. friends /= 0) && (x .&. (x - 1)) == 0` (where `x` is the bitboard) is significantly faster (3-4x) and avoids the `popcnt` overhead, relying purely on simple integer ALU operations that fuse better in GHC Core.
**Action:** When checking if a bitboard has exactly one bit set (especially on a hot path like move generation, `pinnedBits`, or `discoveryCandidates`), prefer the bitwise expression `x /= 0 && (x .&. (x - 1)) == 0` over `popCount x == 1`.
