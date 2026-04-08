## 2024-05-24 – [Perft Leaf Node Move Counting Optimization]
**Learning:** `perftWhite` and `perftBlack` functions in `Chess.Core.Rules.Class` were evaluating `length (generateMoves game)` for leaf nodes at `depth == 1`. In Standard chess variant, `generateMoves` constructs the full list of moves before computing its length, causing an $O(N)$ allocation of list nodes and boxed moves.
**Action:** Introduced a `countMoves` method to the `ChessVariant` typeclass to bypass generating the full list where possible. Implemented `countMoves` for `Standard` variant to directly return the number of legal moves using `countLegalGenMovesSafe` (or `countLegalGenMoves` when in check), bypassing list allocation entirely for depth 1 leaf node counting.
**Impact:** Minor allocation reduction. On `bench-core` KiwiPete Depth 4: NPS increased slightly.

## 2024-05-24 – [Common Expression Extraction on Hot Paths]
**Learning:** `applyMoveBase` in `Chess.Core.Rules.Common` is evaluated on every move during `perft` and search. Inside its `case` statement for `Move`, it redundantly computed `let c = toColor (colorVal @c)` and `oppC = Base.oppositeColor c` in almost every branch.
**Action:** Extracting `c` and `oppC` to the top level of the function via a `let ... in case m of ...` allows GHC to share these bounds, slightly reducing allocations and overhead on the most critical engine paths.
