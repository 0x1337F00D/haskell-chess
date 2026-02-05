# Architectural Optimization: End-to-End Vectorized Move Pipeline

## The Problem: The List Bottleneck in a Vector World

The current architecture suffers from a critical impedance mismatch between the **Move Generation** layer and the **Search** layer.

1.  **Fast Generation, Slow Delivery**: The `Chess.Board.MoveGen` module is capable of efficient, bit-packed move generation using `Word64`-based `GenMove` types. However, the safe interface (`Chess.Board`) degrades this performance by eagerly converting these internal vectors into linked lists to satisfy the public API.
2.  **Allocation Pressure**: The Search engine consumes these lists. For every node in the search tree (potentially millions per second), the engine allocates a cons-cell and a boxed `LegalMove` thunk for every generated move. Memory profiling indicates this list construction is a primary source of garbage (~1.6GB ephemeral allocation), saturating the nursery and triggering frequent GC pauses.
3.  **Double Conversion**: The engine often generates moves (Vector -> List), partitions them (List -> List), and then sorts them (List -> List). This involves chasing pointers across the heap, destroying cache locality and preventing the CPU prefetcher from optimizing memory access.

## The Solution: A Unified Unboxed Pipeline

I propose eliminating the list intermediate representation entirely from the hot path by unifying the generation and consumption layers on **Unboxed Vectors**.

### 1. Type-Level Unboxing
Extend the `LegalMove` wrapper type to support unboxing. Since the underlying `GenMove` type is already implemented as a bit-packed `Word64` newtype (as found in `Chess.Board.MoveGen`), it natively supports unboxed vector storage. We can therefore derive `Unbox` instances for the `LegalMove` wrapper using Generalized Newtype Deriving or manual instance propagation.

### 2. Vector-First API
Refactor the `Chess.Board` module to expose vector-based accessors alongside the existing list-based ones. These new functions should return unboxed vectors of validated moves, enabling consumers to bypass list allocation completely.

### 3. Zero-Allocation Generation
Modify the internal implementation of `Chess.Board.MoveGen` to use `ST`-based mutable vector builders for the accumulation of moves. This ensures that intermediate lists are never constructed, even during the generation phase.

### 4. Vectorized Search
Update the `Chess.Engine.Search` module to operate directly on unboxed vectors of moves.
*   **Sorting**: Use mutable algorithms (e.g., from `vector-algorithms`) to sort moves in-place within a mutable vector, based on heuristics.
*   **Partitioning**: Use vector partitioning functions to separate Good/Bad captures without allocating cons cells.
*   **Iteration**: Refactor the search loop to iterate by index or stream over the vector, avoiding pointer chasing.

## Why this Improves Speed and Design

*   **Shrinking the Runtime Problem Space**: By removing the "List of Moves" concept, we delete the largest source of short-lived garbage in the engine. The runtime no longer manages millions of list nodes; it manages a few reusable arrays.
*   **Aligning Abstractions**: The Alpha-Beta algorithm requires random access (for sorting) and linear traversal. A Vector is the natural data structure for this access pattern; a List is not.
*   **Performance-Oriented Design**: This change allows move values to stay in CPU registers from generation through sorting to application, without ever being boxed onto the heap.

## Trade-offs and Limitations

*   **Strictness**: Vectors are strict. We lose the theoretical ability to "lazy generate" moves. However, since robust engines require sorting all moves (Move Ordering) for effective pruning, this strictness is already a requirement, not a penalty.
*   **Complexity**: Sorting vectors requires mutable algorithms (`ST` monad), which is more verbose than list sorting. This complexity should be encapsulated in helper modules.

## Architectural Placement

*   **`Chess.Types` / `Chess.Board`**: Define `Unbox` for `LegalMove`.
*   **`Chess.Board`**: Expose unboxed vector APIs.
*   **`Chess.Engine.Search`**: Refactor search stage logic and move ordering to use Vectors.
