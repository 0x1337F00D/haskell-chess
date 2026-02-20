# Architectural Optimization: Incremental Check Tracking via Extended `givesCheck`

## The Problem

The current engine architecture relies on **Global Check Detection** (`isCheck` in `Chess.Board.Validation`) to determine the legality of moves and the state of the game. This function performs an O(N) scan of the board, iterating over all enemy pieces to see if they attack the King.

This expensive operation is invoked redundantly in critical paths:
1.  **Search**: `applyLegalMove` (used by Alpha-Beta) calls `trustBoard`, which calls `isCheck` to determine if the resulting position is `InCheck` or `NotInCheck`. This happens for *every* node visited in the search tree.
2.  **Perft**: The `perft` function often re-calculates legality or check status for every move, or falls back to slower methods when in check.
3.  **Game Loop**: `genericExecuteMove` calls `isCheck` to update the high-level `ActiveGame` status.

Despite the fact that `ActiveGame` and `ValidatedBoard` carry a `CheckStatus` type index, this index is populated by expensive runtime checks rather than by construction.

## The Solution

**Integrate Incremental Check Detection into State Transitions.**

Instead of querying the global state of the *new* board ("Is the King attacked?"), we should query the local property of the *move* ("Does this move give check?").

We propose to:
1.  **Extend `givesCheck`**: Complete the `givesCheck` function in `Chess.Board.MoveGen` to handle all move types (Captures, Promotions, En Passant, Castling). Currently, it is only partial (for Quiet Moves).
2.  **Augment Transitions**: Modify `applyLegalMove` (and potentially `applyMove`) to return the `CheckStatus` of the resulting board by calling `givesCheck` on the *current* board and move.
    *   `givesCheck` is O(1) (or close to it), checking only direct attacks from the moving piece and discovered checks from sliders.
    *   It avoids scanning unrelated pieces or squares.
3.  **Propagate Trust**: Use this incrementally computed status to construct `ValidatedBoard` and `ActiveGame` instances directly, bypassing `isCheck` entirely in the main search loop.

## Why It Improvements Speed and Design

### Speed
*   **O(1) vs O(N)**: Replacing a full board scan with a local bitwise check reduces the overhead of move application significantly. In a search exploring millions of nodes, this removes a massive amount of redundant work.
*   **Cache Locality**: `givesCheck` operates on the board state already loaded in CPU registers/L1 cache for move generation. `isCheck` forces a scan of the *new* board state, which may be colder or require more cache lines.

### Design
*   **Information Flow**: The "Check" status is naturally a property of the *transition* (the move). Capturing it at the source allows the type system (`ValidatedBoard 'InCheck`) to carry the proof forward without re-discovery.
*   **Single Source of Truth**: Centralizing check logic in `MoveGen` (generation and detection) reduces the risk of divergence between "what moves are generated" and "what state is detected".

## Trade-offs and Limitations

*   **Implementation Complexity**: `givesCheck` must be implemented carefully to be 100% correct for all edge cases (e.g., En Passant discovery). A bug here would corrupt the search state.
*   **Strictness**: We must ensure `givesCheck` is computed strictly when needed (during move application) but not eagerly for all generated moves (which would be wasteful if they are pruned).

## Guidance

*   **Location**: Implement the extended logic in `src/Chess/Board/MoveGen.hs`.
*   **Integration**: Update `applyLegalMove` in `src/Chess/Board.hs` to use `givesCheck`.
*   **Verification**: Add a test suite comparing `givesCheck` results against `isCheck` for millions of random positions to ensure absolute correctness before enabling it in the engine.
