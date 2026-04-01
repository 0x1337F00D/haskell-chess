## 2024-05-24 – [Perft Leaf Node Move Counting Optimization]
**Learning:** `perftWhite` and `perftBlack` functions in `Chess.Core.Rules.Class` were evaluating `length (generateMoves game)` for leaf nodes at `depth == 1`. In Standard chess variant, `generateMoves` constructs the full list of moves before computing its length, causing an $O(N)$ allocation of list nodes and boxed moves.
**Action:** Introduced a `countMoves` method to the `ChessVariant` typeclass to bypass generating the full list where possible. Implemented `countMoves` for `Standard` variant to directly return the number of legal moves using `countLegalGenMovesSafe` (or `countLegalGenMoves` when in check), bypassing list allocation entirely for depth 1 leaf node counting.
**Impact:** Minor allocation reduction. On `bench-core` KiwiPete Depth 4: NPS increased slightly.

## 2024-05-24 – [Avoid `Maybe` allocations in hot bitboard operations]
**Learning:** `lsb` returns `Maybe Int` because a bitboard can be 0. In highly optimized paths (like `generateEvasions` or array scanning), if the bitboard is strictly proven to be non-zero (e.g., `attackers` are non-zero if we're generating evasions), unpacking the `Maybe` with `fromMaybe 0 (lsb ...)` allocates a `Maybe` thunk anyway and forces branch prediction on it.
**Action:** Created `lsbTotal` which simply wraps `countTrailingZeros` and returns an unboxed `Int` immediately. Refactored `fromMaybe 0 (lsb ...)` calls in `Core.hs` to use `lsbTotal` where the mask is guaranteed to be non-zero, saving boxing allocations and a predictable branch check on `Maybe` types.
