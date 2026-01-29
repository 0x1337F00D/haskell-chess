# Architectural Optimization: Typed Move Generation

## The Problem: Boolean Blindness in Move Representation

The current architecture uses a generic product type `GenMove` to represent all generated moves. This structure bundles a `Move`, a `PieceType` (moving), and a `Maybe PieceType` (captured) into a single "One Size Fits All" record.

This design forces downstream consumers—specifically the critical `applyMove` function and the `toCoreMove` bridge—to structurally re-discover the nature of the move at runtime. For example, to execute a Castling move, the engine must currently:
1.  Inspect the moving piece type.
2.  Calculate the arithmetic distance between the `from` and `to` squares.
3.  Branch on these boolean conditions.

This constitutes "illegal work": the move generator *knew* it was generating a Castling move, but discarded that structural information by flattening it into a generic representation. The system pays a performance tax to lose information, and another tax to reconstruct it in the hottest loops of the engine (search and perft).

## The Solution: Categorical Sum Types

The recommendation is to refactor the intermediate move representation (`GenMove`) from a product type into a **Sum Type** that explicitly mirrors the semantic categories of chess moves.

Instead of a single struct with optional fields and implicit flags, `GenMove` should define distinct constructors for:
*   **Quiet Moves** (carrying `from`, `to`, `movingPiece`)
*   **Captures** (carrying `from`, `to`, `movingPiece`, and strictly `capturedPiece`)
*   **Castling** (carrying the King's move)
*   **En Passant** (carrying the Pawn's move)
*   **Promotions** (carrying promotion choice and capture status)

## Architectural Benefits

### 1. Performance (Speed)
*   **Eliminates Branching**: The `applyMove` function can dispatch immediately on the constructor tag (utilizing efficient jump tables) rather than evaluating a sequence of boolean guards (`isCastling`, `isEP`, `case captured of ...`).
*   **Removes Indirection**: Replacing `Maybe PieceType` with specific constructors eliminates a layer of pointer indirection and allocation. Capture moves store the victim piece type strictly and directly.
*   **Optimizes Verification**: The `isLegal` check can be specialized per constructor. A `GenQuiet` move, for instance, requires fewer checks than a `GenCapture` move, allowing the compiler to generate tighter code for the most common case.

### 2. Correctness (Design)
*   **Aligns with Core Invariants**: The Core layer (`Chess.Core.Move`) already utilizes a GADT to enforce move semantics. This change aligns the Engine layer's output with the Core's input, making the `toCoreMove` conversion a direct, safe, and cost-free O(1) mapping.
*   **Eliminates Impossible States**: It becomes representably impossible to construct invalid states such as "Castling with a captured piece" or "En Passant with a non-Pawn piece." The type system enforces these invariants at the generation site.

## Trade-offs and Limitations

*   **Refactoring Cost**: `GenMove` is pervasive in the move generation logic. Transitioning to a Sum Type requires updating every generator function (e.g., `pawnMoves`, `castlingMoves`) to produce the specific constructor, and every consumer (e.g., `applyMove`, `isLegal`) to pattern match on them. This is a non-trivial, albeit mechanical, refactor.
*   **Memory Layout**: While Sum Types are generally efficient, a constructor with many fields (e.g., `GenCapture` with 4 fields) might be larger than a specialized product type if not carefully unpacked. However, removing the `Maybe` wrapper (2 words + pointer chasing) usually offsets this cost.
*   **List Homogeneity**: In a list `[GenMove]`, the varying sizes of constructors *might* affect cache locality compared to a uniform struct, although GHC handles this well for small sums.

## Implementation Guidance

This change primarily affects **`Chess.Board.MoveGen`** (the producer) and its consumers in **`Chess.Board`** and **`Chess.Core.Rules`**. It is an internal architectural strengthening that requires no changes to the public API or the fundamental bitboard representation, yet it significantly reduces the runtime overhead of the system's core logic.
