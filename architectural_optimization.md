# Architectural Optimization: Bit-Packed Game State Representation

## The Problem
The core `GameState` structure—containing `turn`, `castlingRights`, `epSquare`, `halfmoveClock`, `fullmoveNumber`, and `zobristHash`—is currently defined as a standard Haskell record. Despite strictness annotations, this design forces the allocation of a new heap object (closure) for every single move applied during the search phase. In a high-performance chess engine that executes millions of nodes per second, this constant allocation creates substantial pressure on the Garbage Collector and introduces pointer indirection (cache misses) on the critical path of move application. The current architecture treats the game state as a "bag of fields" rather than a unified, atomic value.

## How It Leverages Haskell's Strengths
This optimization utilizes Haskell's advanced facilities for precise data layout control:
*   **Unboxed Primitives**: By compressing the state into two `Word64` values, we leverage GHC's ability to handle unboxed types, allowing the entire game state to be passed in CPU registers or stored on the stack, bypassing the heap entirely.
*   **Pattern Synonyms**: We can employ `PatternSynonyms` and `ViewPatterns` to expose a high-level, type-safe API (using types like `Color`, `Square`, and `CastlingRights`) while internally managing bit-packed integers. This perfectly aligns the abstraction (logical fields) with the implementation (optimized bits), a core tenet of Haskell design.
*   **Strictness and Unpacking**: Using `{-# UNPACK #-}` ensures that the `Word64` values are embedded directly into parent constructors (like `Board`), eliminating pointer chasing and ensuring a flat memory layout.

## Why It Improves Speed and Design
*   **Speed (Zero Allocation):** Transforms `applyMove` from a function that allocates memory to one that performs simple bitwise operations on registers. This massive reduction in GC churn is critical for search throughput.
*   **Speed (Cache Locality):** Compresses the state to 16 bytes (two words). This ensures the entire state is fetched in a single cache line, significantly reducing memory latency compared to chasing pointers for individual fields.
*   **Design (Explicitness):** Explicitly models the finite bounds of the domain. For example, allocating exactly 10 bits for the halfmove clock and 7 bits for the en passant square makes the constraints of the game rules part of the data structure's definition, rather than just runtime checks.
*   **Design (Encapsulation):** Decouples the storage format from the API. The rest of the engine (Search, Evaluation) continues to interact with semantic types, maintaining code clarity while the performance optimization is encapsulated within the `GameState` module.

## Trade-offs and Limitations
*   **Implementation Complexity:** The internal code for `GameState` becomes significantly more complex, involving bitwise shifts, masks, and logical operations instead of simple record updates.
*   **Field Size Limits:** The packing imposes strict bit-width limits on fields (e.g., 16 bits for `FullmoveNumber`). While sufficient for all legal chess games, this theoretically reduces the representable state space compared to machine-word integers.
*   **Debugging Friction:** Raw `Word64` values are harder to interpret during debugging than record fields, necessitating robust `Show` instances or inspection tools to visualize the packed state.

## Guidance
*   **Location:** Implement the bit-packing logic within `src/Chess/Board/GameState.hs`.
*   **API Strategy:** Retain the existing `GameState` record syntax via `PatternSynonyms` to avoid breaking changes in `Chess.Board`, `Chess.Engine`, and `Chess.Core`.
*   **Integration:** Update `src/Chess/Board.hs` to verify that `GameState` is unpacked into the `Board` data type, ensuring the board representation remains flat and contiguous.
