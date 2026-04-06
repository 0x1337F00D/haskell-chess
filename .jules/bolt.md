## 2024-05-24 – [Perft Leaf Node Move Counting Optimization]
**Learning:** `perftWhite` and `perftBlack` functions in `Chess.Core.Rules.Class` were evaluating `length (generateMoves game)` for leaf nodes at `depth == 1`. In Standard chess variant, `generateMoves` constructs the full list of moves before computing its length, causing an $O(N)$ allocation of list nodes and boxed moves.
**Action:** Introduced a `countMoves` method to the `ChessVariant` typeclass to bypass generating the full list where possible. Implemented `countMoves` for `Standard` variant to directly return the number of legal moves using `countLegalGenMovesSafe` (or `countLegalGenMoves` when in check), bypassing list allocation entirely for depth 1 leaf node counting.
**Impact:** Minor allocation reduction. On `bench-core` KiwiPete Depth 4: NPS increased slightly.

## 2026-04-06 – [Eliminate Maybe Box Allocation in Bitboard Scanning]
**Learning:** In hot paths like check evasion generation (`generateEvasions` etc.) and validation checks, using `lsb` and `msb` returned a `Maybe Int`. This forced the allocation of a `Maybe` constructor and forced an unboxing step (`fromMaybe 0`), which added garbage collection pressure and slowed down the node per second throughput.
**Action:** Implemented `lsbTotal` and `msbTotal` functions which directly wrap `countTrailingZeros` and `63 - countLeadingZeros` without returning a `Maybe`. Replaced usages of `lsb` and `msb` with these total functions where the input bitboard was already statically guaranteed to be non-zero (e.g. `attackers /= 0`).
