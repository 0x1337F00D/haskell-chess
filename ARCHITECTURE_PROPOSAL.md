# Architectural Optimization Proposal: Shadow Board Removal

## Executive Summary

**Proposal:** Eliminate the dual board representation in the `ActiveGame` type by removing the high-level `gameBoard` (Map-based) field and establishing `internalBoard` (Bitboard-based) as the single authoritative source of truth.

**Impact:** significantly reduces memory allocation and update overhead in the engine's hot loop, enforcing a single source of truth while leveraging Haskell's type system to maintain safety.

---

## 1. The Architecture Gap

The current `ActiveGame` definition maintains two synchronized representations of the chess board:

1.  **High-Level (`gameBoard`)**: A `Map`-based structure (`Chess.Core.Board.Internal.Board`) that encodes pieces with strong types (e.g., `MajorMinorPiece 'White`). It allows for type-safe pattern matching but incurs $O(\log N)$ or worse overhead for every update and relies on heap allocation.
2.  **Low-Level (`internalBoard`)**: A `Bitboard`-based structure (`Chess.Board.Base.Board`) optimized for $O(1)$ updates and unboxed storage.

This duality violates the **Single Source of Truth** principle. The engine logic (`Chess.Core.Rules`) primarily utilizes the bitboard representation for move generation and validation, yet it pays the penalty of updating the persistent `Map` structure at every ply. The `gameBoard` acts as a "shadow" state—expensive to maintain, redundant for logic, and serving primarily as a view.

## 2. The Solution: Unifying State

We propose removing the `gameBoard` field from `ActiveGame`. The `ActiveGame` type should rely exclusively on `internalBoard` for state tracking.

### Leveraging Haskell's Strengths

*   **Type-Level Refinement**: Instead of relying on a structural `Map Square (Piece c)` to enforce color invariants, we leverage `ActiveGame`'s type indices (`v :: Variant`, `turn :: Color`) and the `KnownColor` typeclass. We can interpret the raw bits of `Base.Board` safely (e.g., "if it is White's turn, the `whiteKings` bitboard is the valid King") without needing a runtime structure to prove it.
*   **Laziness & Views**: The high-level `Board` structure should be demoted to a **View**. We can provide a function `viewBoard :: ActiveGame v c s -> Board` that reconstructs the Map-based board *on demand*. Haskell's laziness ensures this reconstruction only happens when strictly necessary (e.g., for IO, serialization, or UI rendering), completely removing the cost from the search loop.
*   **Data Representation**: Stripping the `Map` fields allows `ActiveGame` to become a thin wrapper around unboxed `Word64`s (bitboards) and strict primitive counters. This enables GHC to unbox the game state more aggressively, keeping data in registers and reducing pointer chasing / GC pressure.

## 3. Implementation Guidance

1.  **Modify `ActiveGame`**: Remove `gameBoard :: Board` from the record definition in `Chess.Core.Game.Internal`.
2.  **Update Rule Logic**: Refactor `Chess.Core.Rules` to perform all move execution logic on `internalBoard`. Currently, `toCoreMove` looks up pieces in `gameBoard` to determine the move type (e.g. castling vs standard). This must be changed to query `Base.pieceAt` or `Base.findPieceType` directly.
3.  **Provide a View**: Implement `getBoard` (or `toBoard`) to allow backward compatibility for consumers that expect the high-level `Board` type.

## 4. Trade-offs

*   **API Visibility**: `ActiveGame` becomes opaque and "raw". Debugging via `Show` will display bitmasks unless a custom instance using the view is defined.
*   **Reconstruction Cost**: Reconstructing the full `Board` becomes an $O(N)$ operation. This is acceptable as it moves cost from the critical path (move generation) to the edge (IO).
*   **Complexity**: Logic that relies on easy pattern matching (e.g. `case getPieceAt ...`) will need to use bitboard queries, which are more verbose but significantly faster.

## Implementation Checklist

- [x] **Refactor ActiveGame**
    - [x] Remove `gameBoard` from `ActiveGame` definition in `src/Chess/Core/Game/Internal.hs`
    - [x] Add `viewBoard` function to reconstruction `Board` from `internalBoard`

- [x] **Update Core Rules**
    - [x] Update `initialGame` to not use `gameBoard`
    - [x] Update `gameFromFEN` to not use `gameBoard`
    - [x] Update `toCoreMove` to use `Base.Board`
    - [x] Update `generateMoves` for Standard
    - [x] Update `executeMove` for Standard
    - [x] Update `generateMoves` for Variants (Atomic, ThreeCheck, KOTH, RacingKings, Crazyhouse)
    - [x] Update `executeMove` for Variants

- [x] **Update Tests**
    - [x] Update `test/CoreSpec.hs` to use `viewBoard`
