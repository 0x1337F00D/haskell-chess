# Architectural Optimization: Packed Unboxed Move Generation

## The Problem: Ephemeral Graph Allocation

The current move generation architecture produces moves as lazy linked lists (`[GenMove]`) of boxed Sum Types. While semantically clear, this design creates a massive runtime burden:

1.  **Heap Allocation**: Every generated move allocates at least two heap objects: a list cons cell and a `GenMove` thunk/closure. In a high-throughput engine visiting millions of nodes per second, this results in gigabytes of short-lived garbage (observed ~1.6GB), saturating the nursery and triggering frequent Garbage Collection cycles.
2.  **Pointer Indirection**: A list of boxed objects is a scattered graph of pointers. Iterating over moves requires chasing these pointers, which destroys CPU cache locality and prevents the prefetcher from working effectively.
3.  **Misaligned Evaluation**: The Alpha-Beta search algorithm heavily relies on **Move Ordering** (sorting moves by heuristics like MVV-LVA). Sorting requires materializing the entire collection of moves, negating the theoretical benefits of lazy generation.

## The Solution: Packed Value Types & Unboxed Vectors

I propose a shift from **Lazy, Boxed Structures** to **Strict, Flat Memory** layouts for the hot path of move generation.

### 1. Refactor `GenMove` to a Bit-Packed Value Type

Convert the `GenMove` Sum Type into a bit-packed `newtype` wrapping a primitive 64-bit word.

*   **Current**: `data GenMove = GenQuiet Sq Sq Pt | GenCapture Sq Sq Pt Pt | ...` (Boxed, variable size, pointer-rich)
*   **Proposed**: `newtype GenMove = GenMove Word64` (Unboxed, fixed size, register-resident)

We can encode the entire semantic payload (Source, Destination, Moving Piece, Captured Piece, Promotion Type, and Move Tag) into bitfields within the `Word64`. This allows `GenMove` to be stored without headers or pointers.

### 2. Adopt Unboxed Vectors for Move Lists

Change the core move generation API to return `Data.Vector.Unboxed.Vector GenMove` instead of `[GenMove]`.

*   **Flat Layout**: A move list becomes a contiguous block of memory (a `ByteArray#`).
*   **Zero-Cost Iteration**: Traversing the moves becomes a linear scan of memory, maximizing CPU cache utilization and allowing GHC to generate tight inner loops (potentially using SIMD).

## Why this improves Speed and Design

*   **Shrinking the Problem Space**: We eliminate the "Graph" aspect of move lists entirely. The runtime no longer manages millions of list nodes; it manages a few simple arrays.
*   **Aligning with Usage**: Since the search engine must sort moves, providing a strict, random-access container (Vector) is architecturally superior to a sequential, lazy container (List).
*   **Correctness by Construction**: By using `Data.Vector.Unboxed`, we enforce at the type level that no "thunks" or "unevaluated moves" can exist inside the container. The data is guaranteed to be fully evaluated and flat.

## Trade-offs and Limitations

*   **Strictness**: We lose the ability to generate moves lazily (e.g., stop after finding the first pseudo-legal move). However, given the necessity of Move Ordering in strong engines, this is a theoretical loss rather than a practical one.
*   **Implementation Complexity**: Managing bit-masks and shifts is more error-prone than using Algebraic Data Types. This complexity should be encapsulated within the `GenMove` type definition, exposing a clean API to the rest of the system.

## Architectural Placement

*   **Definition**: `Chess.Board.MoveGen` (or `Chess.Types`) should define the packed `GenMove` and its `Unbox` instances.
*   **Generation**: `Chess.Board.MoveGen` functions (`pseudoLegalMoves`, `legalMoves`) should be updated to construct and return Vectors.
*   **Consumption**: `Chess.Engine.Search` should be refactored to iterate over these Vectors, leveraging efficient indexing for sorting and selection.
