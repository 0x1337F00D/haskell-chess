# Architectural Optimization: Context-Aware Legality Verification

## The Problem: Redundant Legality Checks in Safe Positions
The engine currently suffers from a "pessimistic verification" bottleneck. In the `isLegal` function (located in `src/Chess/Board/MoveGen.hs`), every pseudo-legal move undergoes a full legality check via `isLegalOptimized`. This function performs an O(N) board scan (`isAttackedByOptimized`) to verify that the King is not left in check.

This check is applied indiscriminately, even when the board is known to be in a `Safe` (NotInCheck) state. In a typical search, ~90-95% of positions are `Safe`. For these positions, the King cannot be captured directly; he can only be exposed if a **pinned piece** moves off its pin ray, or via an En Passant capture. By treating `Safe` positions as potentially `InCheck`, the engine wastes significant cycles re-verifying the safety of unpinned pieces.

## Haskell Strength: Type-Indexed Invariants & Bitboards
We can leverage the existing `ValidatedBoard` GADT (which encodes the `CheckStatus` at the type level) to enforce a more efficient verification strategy by construction.
*   **Proof of Safety**: The `ValidatedBoard 'NotInCheck` type provides a compile-time guarantee that the King is not currently attacked. We can pass this proof to the move generator.
*   **Bitwise Masking**: Instead of simulating moves and scanning for attacks (O(N)), we can pre-calculate a `pinned` bitboard (O(N) once per position) and use simple bitwise masking (O(1)) to validate moves.

## The Improvement: `isLegalSafe`
We propose replacing the generic `isLegal` check with a specialized `isLegalSafe` function for `NotInCheck` contexts.

**Algorithm (`isLegalSafe`):**
1.  **King Moves**: Must still check `isAttackedBy` on the target square (unchanged, but few moves).
2.  **En Passant**: Must still use the special rank check (unchanged, rare).
3.  **Other Moves (The 90% case)**:
    *   Check if the moving piece's bit is set in the `pinned` bitboard.
    *   **If Not Pinned**: The move is **automatically legal**. No board scan required.
    *   **If Pinned**: The move is legal *only if* the target square lies on the ray between the King and the pinner. This is a fast bitwise AND check (`(1 << to) & pinRay`).

**Impact:**
This transforms the legality verification complexity from **O(M * N)** (checking every move against every slider direction) to **O(N + M)** (one pass to find pins, then constant time per move). In a perft or search generating millions of nodes, this massive reduction in memory access and branch misprediction yields significant speedups.

## Trade-offs and Limitations
*   **Implementation Complexity**: Correctly calculating the `pinned` bitboard and pin rays requires careful bitboard arithmetic (X-ray attacks).
*   **Strictness**: The `pinned` mask calculation must be strict and efficient. If done lazily or inefficiently, it could outweigh the savings for nodes with few moves.

## Guidance
*   **Location**: Implement `isLegalSafe` and `pinnedBitboard` in `src/Chess/Board/MoveGen.hs`.
*   **Integration**: Update the `MoveGenerator 'NotInCheck` instance in `src/Chess/Board.hs` to call a new `legalGenMovesSafe` function that utilizes `isLegalSafe`.
