# Architectural Optimization: Type-Indexed Check Status

## The Problem: Boolean Blindness in Move Generation

The engine's core `Board` type presently treats the king's check status as an implicit runtime property. Functions like `legalMoves` operate on a monolithic `Board` type, forcing them to dynamically check if the king is under attack to determine which move generation logic to apply (e.g., standard generation vs. evasion generation). This "boolean blindness" prevents the compiler from verifying that specific logic is only applied in appropriate states and forces the runtime to perform redundant checks or rely on fragile invariants.

## The Solution: Phased-Indexed Board Types

We propose lifting the `CheckStatus` (Safe, Checked) into the type system as a phantom type index on the `Board` type. By utilizing `DataKinds` and `GADTs`, the `Board` type becomes `Board (s :: CheckStatus)`.

This architectural change allows us to:
1.  **Specialize Functions**: Define distinct functions for `generateMoves` on a `Board 'Safe` versus a `Board 'Checked`. The compiler can statically dispatch to the correct optimized implementation.
2.  **Enforce Invariants**: Make it a compile-time impossibility to call castling generation logic on a board that is in check.
3.  **Refine Transitions**: The `makeMove` function (or `applyMove`) would return an existentially quantified result (e.g., `SomeBoard`), forcing the consumer (the Search loop) to pattern match on the new check status. This ensures that the check status is computed exactly once—during the transition—and is then statically known for all subsequent operations on that board.

## Why It Leverages Haskell's Strengths

*   **DataKinds and GADTs**: This uses Haskell's advanced type system to encode the finite state machine of chess rules directly into the data structures.
*   **Static Dispatch**: By separating the types, we allow GHC to generate specialized machine code for the "Safe" and "Checked" paths, eliminating branching overhead in the hottest loops of the engine.
*   **Existential Quantification**: Leveraging existentials allows us to handle the dynamic result of a move while immediately recovering type safety for the next step of the computation.

## Why It Improves Speed and Design

*   **Speed (Eliminating Redundancy)**: Removes the need for repeated `isCheck` calls inside move generation and evaluation. The status is a compile-time fact for the duration of the function body.
*   **Speed (Specialization)**: Evasion move generation is fundamentally different from quiet move generation. Separating them at the type level enables more aggressive inlining and optimization of the distinct code paths.
*   **Design (Correctness)**: It aligns the code structure with the domain logic. A "Checked" board is fundamentally different from a "Safe" board in terms of available actions; the type system now reflects this reality.

## Trade-offs and Limitations

*   **API Complexity**: The `Board` type signature becomes more complex. Consumers of the API must be comfortable with type-level programming concepts and handling existential types.
*   **Verbosity**: Working with existentially quantified types often requires unpacking or Continuation-Passing Style (CPS), which can be more verbose than simple `if-else` branching.

## Guidance

*   **Location**: Implement this change in `Chess.Board`.
*   **Integration**: Introduce a `CheckStatus` data kind (or reuse the one from `Chess.Core`). Refactor the `Board` definition to accept the phantom type. Update the `Search` loop to handle the existential result of move application, dispatching to the appropriate specialized search function.
