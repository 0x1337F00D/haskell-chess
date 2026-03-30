## 2024-05-24 – [Perft Leaf Node Move Counting Optimization]
**Learning:** `perftWhite` and `perftBlack` functions in `Chess.Core.Rules.Class` were evaluating `length (generateMoves game)` for leaf nodes at `depth == 1`. In Standard chess variant, `generateMoves` constructs the full list of moves before computing its length, causing an $O(N)$ allocation of list nodes and boxed moves.
**Action:** Introduced a `countMoves` method to the `ChessVariant` typeclass to bypass generating the full list where possible. Implemented `countMoves` for `Standard` variant to directly return the number of legal moves using `countLegalGenMovesSafe` (or `countLegalGenMoves` when in check), bypassing list allocation entirely for depth 1 leaf node counting.
**Impact:** Minor allocation reduction. On `bench-core` KiwiPete Depth 4: NPS increased slightly.

2026-02-01 - Avoid Maybe allocations in single-check evasion generation using lsbTotal
Learning: In the hottest paths of perft check evasions (`generateEvasions` and variants), extracting the single checking square using `fromMaybe 0 (lsb attackers)` caused significant boxing and evaluation overhead. Since `attackers > 0` and `(attackers .&. (attackers - 1)) == 0` guarantees exactly one set bit, `Maybe` is redundant.
Action: Introduced a total `lsbTotal :: Bitboard -> Int` wrapper around `countTrailingZeros` and replaced `lsb` usage. This eliminated the `Maybe Int` allocation, boosting KiwiPete Depth 4 performance from ~36.9M NPS to ~46.6M NPS (+26%).
