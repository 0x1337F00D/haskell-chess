# Architectural Optimization: Consolidated State Representation

## The Problem

The current "Hybrid Architecture" suffers from a **state representation disconnect** between the Safe Core (`Chess.Core`) and the Fast Engine (`Chess.Board`).

1.  **Redundant State**: The `ActiveGame` type (the Safe Core's primary state) manually tracks game counters (`halfMoveClock`, `fullMoveNumber`), castling rights, and en passant targets as loose fields. These exact same properties are tracked by `Chess.Board.GameState` (the Fast Engine's state struct).
2.  **Conversion Overhead**: To perform any move generation or validation, the Core must convert its state into the Engine's format. This is done via `toGameState`, which allocates a new `GameState` object and performs bitwise calculations to translate `Word8` castling rights to `Bitboard` rights. This conversion happens O(N) times (where N is the number of nodes visited), creating unnecessary allocation pressure and CPU work in the hot loop.
3.  **Representation Mismatch**: The Core uses a simplified `Word8` to store castling rights (4 bits: KQkq). This is insufficient for correctly representing all Fischer Random (Chess960) states, where castling rights are tied to specific rook files. The Engine already uses a superior `Bitboard` representation, but the Core cannot leverage it without conversion.

## The Solution

**Embed the Engine's State directly into the Core's Safe Wrapper.**

Refactor the `ActiveGame` type to replace its redundant fields with a single `internalState` field of type `Chess.Board.GameState`.

The `ActiveGame` type remains an opaque, type-indexed wrapper, but its internal storage becomes fully aligned with the performance layer. The phantom types (`turn`, `CheckStatus`) continue to enforce invariants at the type level, but they now index a raw state object that is "ready to run".

## Why It Improves Speed and Design

*   **Zero-Cost Interoperability**: The `toGameState` projection becomes a zero-cost field accessor. Move generation functions (which expect `GameState`) can be called directly on the internal state without allocation or translation.
*   **Reduced Allocation**: Eliminating the short-lived `GameState` allocations in `generateMoves` reduces GC pressure, which is critical for search performance.
*   **Correctness by Definition**: By adopting the `Bitboard` representation for castling rights (already present in `GameState`), the Core gains full support for Fischer Random castling logic without complex mapping functions.
*   **Feature Parity**: The Core automatically gains access to Zobrist hashing (maintained by `GameState`), enabling O(1) checks for threefold repetition and transposition table lookups, which are currently absent or expensive in the Core layer.

## Trade-offs and Limitations

*   **Redundant Turn Information**: `GameState` stores the turn dynamically (as a value), while `ActiveGame` stores it statically (as a type index). We introduce a redundancy that must be kept synchronized. This is managed by the trusted kernel (constructors and move application functions), ensuring the runtime value always matches the type index.
*   **Opaque Data Dependency**: `Chess.Core` becomes more tightly coupled to `Chess.Board.GameState`. Changes to the `GameState` layout will directly affect `ActiveGame`'s memory layout (which is generally desired for performance but increases coupling).

## Implementation Strategy

This optimization belongs in `Chess.Core.Game.Internal` and should follow these steps:

1.  **Refactor ActiveGame**:
    Modify the `ActiveGame` data definition to remove `castlingRights`, `enPassantTarget`, `halfMoveClock`, and `fullMoveNumber`. Replace them with `internalState :: !Chess.Board.GameState.GameState`.

2.  **Update Constructors**:
    Update `initialGame` and `gameFromFEN` in `Chess.Core.Rules.Standard` (and other variants) to initialize the `internalState`.

3.  **Maintain Synchronization**:
    Update `genericApplyMove` in `Chess.Core.Rules.Common` to ensure that when a move is applied, the new `ActiveGame`'s phantom type index matches the new `internalState`'s turn.

4.  **Remove Legacy Conversion**:
    Delete the `toGameState` function and replace its usage with a direct accessor to `internalState`.

5.  **Fix Fischer Random**:
    Update `FischerRandom` rules to read castling rights directly from the bitboards in `internalState`, removing the `Word8` translation layer.
