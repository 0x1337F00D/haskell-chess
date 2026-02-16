# Architectural Optimization: Phase-Indexed Board Validation with Integrated Check Status

## The Problem

The `Chess.Engine` layer currently operates on a `ValidatedBoard` type that, while distinct from a raw `Board`, discards the most critical tactical invariant in chess: whether the king is in check. This "blindness" forces the engine into a pattern of redundant runtime calculation and defensive programming:

1.  **Redundant Runtime Checks**: The Alpha-Beta search loop (`Chess.Engine.Search.AlphaBeta`) must manually call `isCheck` after every move application to update the search context (`scCheckState`) and inform move ordering heuristics. This involves an O(N) bitboard scan that is often computed from scratch, ignoring information available during move generation.
2.  **Implicit Invariants**: The `SearchContext` maintains a separate `scCheckState` field which *should* match the board state, but the type system provides no guarantee of this synchronization. A bug in the search logic could easily lead to a state where `scCheckState` says `Safe` but the board is actually `Checked`.
3.  **Suboptimal Move Generation**: The `MoveGen` module generates moves uniformly and filters them using `isLegal`, which is an expensive operation that checks if the move leaves the king in check. However, if the board is known to be `Safe`, many of these checks (e.g., verifying evasion logic) are unnecessary. Conversely, if the board is `Checked`, the generator wastes cycles producing non-evasion pseudo-moves that will be inevitably filtered out.

## The Solution

**Refine the `ValidatedBoard` type to carry the check status as a phantom type index.**

We propose replacing the current opaque wrapper:
```haskell
newtype ValidatedBoard = ValidatedBoard Board
```

with a type-indexed version:
```haskell
newtype ValidatedBoard (s :: CheckStatus) = ValidatedBoard Board
```

And introducing an existential wrapper for the result of move application:
```haskell
data SomeValidatedBoard where
  SomeValidatedBoard :: SCheckStatus s -> ValidatedBoard s -> SomeValidatedBoard
```

The core operation `makeMove` (or `applyMove`) will then return this proof:
```haskell
makeMove :: ValidatedBoard s -> Move -> SomeValidatedBoard
```

## How It Leverages Haskell's Strengths

This optimization uses **DataKinds** and **GADTs** to promote a runtime boolean property (is the king in check?) into a compile-time distinction.

*   **Type-Driven Dispatch**: Functions can be specialized based on the check status. For example, `legalMoves` can use a type class or pattern match on `SCheckStatus` to dispatch to `generateEvasions` (for `Checked` boards) or `generateMoves` (for `Safe` boards) without runtime branching or accidental misuse.
*   **Correctness by Construction**: The `SearchContext` no longer needs a redundant `scCheckState` field. The check status is intrinsic to the board type itself, making it impossible for the search logic to be "out of sync" with the board's reality.
*   **Existential Encapsulation**: The use of `SomeValidatedBoard` forces the consumer (the Search loop) to pattern match on the resulting check status, ensuring that both the `Safe` and `Checked` paths are handled explicitly.

## Why It Improves Speed and Design

*   **Speed (Eliminating Redundancy)**: The `isCheck` call in the hot loop of the search is eliminated. The check status is computed *once* during move application—often more efficiently by leveraging the move generation context (e.g., `givesCheckFast`)—and then carried as a zero-cost type index.
*   **Speed (Optimized Generation)**: By distinguishing `Checked` boards at the type level, we can invoke specialized move generators that only produce candidate evasions, skipping the generation of thousands of invalid pseudo-moves per second.
*   **Design (Clarity)**: The control flow of the search becomes a reflection of the game rules:
    ```haskell
    case makeMove board move of
      SomeValidatedBoard SSafe newBoard -> ... -- Standard search
      SomeValidatedBoard SChecked newBoard -> ... -- Evasion search / Check extension
    ```

## Trade-offs and Limitations

*   **Strictness Requirement**: To construct `SomeValidatedBoard`, we must determine the check status immediately upon move application. This enforces strictness in the check calculation, which is generally desirable for a chess engine but removes the possibility of lazy check detection (though in Alpha-Beta search, this information is almost always needed immediately anyway).
*   **Module Coupling**: This requires exposing `CheckStatus` and `SCheckStatus` (currently in `Chess.Core`) to `Chess.Board` and `Chess.Engine`, necessitating a shared `Chess.Types` or `Chess.Common` module to avoid circular dependencies.

## Architectural Placement

*   **`Chess.Types`**: Move `CheckStatus` and `SCheckStatus` here.
*   **`Chess.Board`**: Redefine `ValidatedBoard` and `SomeValidatedBoard`. Update `applyMove` to compute and return the proof.
*   **`Chess.Engine.Search`**: Update the loop to pattern match on `SomeValidatedBoard` and remove `scCheckState`.
