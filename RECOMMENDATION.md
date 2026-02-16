# Architectural Optimization: Zero-Allocation Game State

## The Problem: Heap-Allocated State
The current engine architecture suffers from a fundamental performance bottleneck: the `GameState` record (containing `turn`, `epSquare`, `halfmoveClock`, `fullmoveNumber`, and `castlingRights`) is heap-allocated for every single move application. In a high-throughput search (millions of nodes/sec), this creates immense pressure on the Garbage Collector, saturating the nursery and forcing frequent collections. Furthermore, accessing these fields involves pointer indirection, which degrades CPU cache efficiency.

## The Solution: Bit-Packed & Unboxed State
Replace the `GameState` record with a flattened, bit-packed representation embedded directly into the `Board` structure.

1.  **Bit-Packing Scalar Fields**: Compress the following fields into a single `Word64` ("Info Word"):
    -   **Turn** (1 bit): White/Black.
    -   **En Passant Target** (7 bits): 0-63 for square, 64 for none.
    -   **Halfmove Clock** (10 bits): Sufficient for 50-move rule (max ~1024).
    -   **Fullmove Number** (16 bits): Sufficient for any practical game (max 65535).
    -   *Total*: ~34 bits, fitting comfortably within a 64-bit word.

2.  **Unboxing in Board**: Modify the `Board` data type to store the state as three unboxed `Word64` fields instead of a pointer to a `GameState` object:
    -   `gsInfo :: {-# UNPACK #-} !Word64` (Packed scalars)
    -   `gsCastling :: {-# UNPACK #-} !Word64` (Castling Rights Bitboard)
    -   `gsHash :: {-# UNPACK #-} !Word64` (Zobrist Hash)

3.  **Pattern Synonyms**: Maintain the existing high-level `GameState` API using bidirectional Pattern Synonyms. This allows the rest of the engine (Move Generation, Search) to interact with logical types (`Color`, `Square`) while the internal representation remains optimized.

## Why It Improves Speed and Design
*   **Speed (Zero Allocation)**: Eliminates the `GameState` heap allocation entirely. Move application becomes a series of register-based bitwise operations, significantly increasing nodes per second (NPS).
*   **Speed (Cache Locality)**: The `Board` struct becomes a flat, contiguous block of memory (pointers to `Bitboards` + 3 `Word64`s). This fits efficiently into CPU cache lines.
*   **Design (Value Semantics)**: Treats the game state as a primitive value rather than a reference type, aligning with the immutable nature of the chess position.
*   **Correctness**: Enforces domain constraints (e.g., max halfmove clock) at the representation level.

## Trade-offs and Limitations
*   **Implementation Complexity**: Requires manual bit-twiddling logic for getters/setters in the `GameState` module.
*   **Field Limits**: Imposes strict bit-width limits on counters (e.g., 65k moves), though these far exceed any legal chess game length.
*   **Coupling**: The `Board` structure becomes tightly coupled to the specific packing layout of `GameState`.

## Guidance
*   Implement the packing logic within `Chess.Board.GameState`, hiding the raw `Word64`s behind a safe interface.
*   Update `Chess.Board` to unpack the three `Word64` fields.
*   Ensure `Chess.Core` adapts to this change by updating its `ActiveGame` wrapper (per previous proposals) to hold the unpacked state or the `Board` itself.
