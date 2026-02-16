# Architectural Optimization: Embedded Incremental Evaluation Context

## The Problem: The O(N) Evaluation Bottleneck

Currently, the engine's evaluation function (`evaluate` in `Chess.Engine.Evaluation`) operates in **O(N)** time, where N is the number of pieces on the board. For every leaf node in the search tree (and every node in Quiescence Search), the engine iterates over all 12 piece bitboards to recalculate:
1.  **Material Score**: Summing values of all pieces.
2.  **Piece-Square Table (PST) Score**: Summing positional values for every piece.
3.  **Game Phase**: Calculating non-pawn material for tapered evaluation.

This "stateless evaluation" approach forces the CPU to recompute information that is implicitly available during the state transition. In a high-performance engine executing millions of nodes per second, this redundant computation consumes a massive portion of the total runtime (often >50%), creating a hard ceiling on search depth.

## The Solution: Embedded Evaluation Context

I propose **embedding the evaluation context directly into the `Board` data structure** and maintaining it incrementally.

### 1. Data Structure Augmentation
Extend `Chess.Board.Base.Board` with `{-# UNPACK #-}`ed fields to store the current evaluation state. This ensures the board is always "aware" of its own static value.

Extend the `Chess.Board.Base.Board` constructor with strict, unpacked fields to store the current evaluation state:
*   `materialScore`: A packed integer storing both Middlegame and Endgame material scores.
*   `pstScore`: A packed integer storing both Middlegame and Endgame Piece-Square Table scores.
*   `phase`: An integer representing the game phase for tapered evaluation.

### 2. Incremental Updates
Modify `Chess.Board.MoveGen` and `Chess.Board.Base` (specifically `applyMove` and `unsafePutPiece`/`unsafeRemovePiece`) to update these fields incrementally.
*   **Move**: Subtract old PST value, add new PST value.
*   **Capture**: Subtract captured piece's Material and PST values.
*   **Promotion**: Subtract Pawn Material/PST, add Promoted Material/PST.

### 3. O(1) Evaluation
Refactor `Chess.Engine.Evaluation.evaluate` to simply read these pre-calculated fields and apply the phase tapering formula. This transforms the evaluation from a linear scan O(N) to a constant time lookup O(1).

## How It Leverages Haskell's Strengths

*   **Strictness and Unboxing**: By adding strict, unpacked fields to the `Board` constructor, we ensure that the evaluation state is computed strictly during move generation (the "producer") and stored without boxing or thunks. This aligns with GHC's strength in optimizing flat, unboxed data structures.
*   **Immutable State Invariants**: Haskell's immutability ensures that valid `Board` states always carry their correct score. We enforce the invariant "Score matches Position" at the constructor level. There is no risk of "dirty flags" or state drift that plagues mutable C++ implementations.
*   **Information Locality**: All data required to evaluate a position is now resident in the `Board` struct itself (likely in CPU cache), eliminating the need to fetch external tables or iterate over disjoint memory regions (bitboards) during the critical evaluation step.

## Why This Improves Speed and Design

*   **Speed (Work Removal)**: We replace a loop of ~30 iterations (bit scans + table lookups + additions) with ~2 integer operations per move. The net reduction in CPU cycles for the search loop is massive.
*   **Speed (Quiescence Search)**: QS relies heavily on "Stand Pat" (static evaluation) to prune nodes. Making this check O(1) significantly speeds up the most volatile part of the search.
*   **Design (Cohesion)**: The `Board` becomes a self-contained "Position" rather than just a "Piece Layout". It encapsulates the *value* of the position along with its *structure*.
*   **Design (Totality)**: It eliminates the class of bugs where the evaluation function might be out of sync with the board representation (e.g., if bitboards are updated but eval tables are not), as the updates are coupled in the move application logic.

## Trade-offs and Limitations

*   **Copy Overhead**: The `Board` structure grows by ~16-24 bytes. Copying the board (during recursive search calls) becomes slightly more expensive. However, this is negligible compared to the O(N) evaluation savings.
*   **Complexity**: The `applyMove` logic becomes more complex, as it must now handle score arithmetic. This logic must be meticulously tested to ensure correctness (e.g., via Perft-like consistency checks for scores).
*   **Dependency Management**: Move generation logic now depends on evaluation constants (Piece Values, PSTs). These constants must be moved to a shared module (e.g., `Chess.Data.Constants` or `Chess.Types`) to avoid circular dependencies between `Chess.Board` and `Chess.Engine.Evaluation`.

## Architectural Placement

*   **`Chess.Types` / `Chess.Data.Constants`**: Define `PackedScore` and move material/PST constants here.
*   **`Chess.Board.Base`**: Add `materialScore`, `pstScore`, and `phase` fields to `Board`.
*   **`Chess.Board.MoveGen`**: Update `applyMoveBoardFast` and helpers to maintain scores.
*   **`Chess.Engine.Evaluation`**: Rewrite `evaluate` to use the embedded fields.
