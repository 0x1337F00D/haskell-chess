# Architecture & Layering

This document defines the architectural boundaries for the Haskell Chess Engine.
The goal is to maintain a clean separation of concerns, ensuring "illegal states are unrepresentable" where possible, and preventing circular dependencies.

## High-Level Layering

The codebase is organized into three primary layers. Dependencies flow downwards.

```
Chess.App  (Application Layer)
   ↓
Chess.Engine (Decision Layer)
   ↓
Chess.Board (Domain Layer)
```

### 1. Chess.Board (The Domain)
* **Responsibility**: Representation of the board, move generation, move validation, and game rules.
* **Invariants**: Must never import `Chess.Engine` or `Chess.App`.
* **Key Modules**:
    * `Base`: Bitboards, attacks, primitive operations.
    * `Move`: Move types and bit-packing.
    * `Position`: The full game state (Board + side + castling + ep).
    * `Validation`: Move legality checks.

### 2. Chess.Engine (The Brain)
* **Responsibility**: Search, Evaluation, and Time Management.
* **Invariants**:
    * `Search` depends on `Evaluation`.
    * `Evaluation` must **never** import `Search`.
* **Key Modules**:
    * `Search`:
        * `AlphaBeta`: Core search algorithm.
        * `Quiescence`: Stabilization search.
        * `Ordering`: Move ordering heuristics.
        * `Pruning`: Reduction and pruning logic.
        * `Types`: Common search types and contexts.
    * `Evaluation`: Static position scoring.
    * `TT`: Transposition Table.
    * `Context`: Search context and helpers.

### 3. Chess.App (The Interface)
* **Responsibility**: Protocol handling (UCI), benchmarking, and reporting.
* **Invariants**: Can import everything.
* **Key Modules**:
    * `UCI`: Universal Chess Interface protocol implementation.
    * `BenchReport`: Performance reporting tools.

## Core Rules

1.  **Board must never import Engine.**
2.  **Evaluation must never import Search.**
3.  **Search may depend on Evaluation, not vice versa.**

## Design Principles

*   **Make Illegal States Unrepresentable**: Use types to encode logic states (e.g., `NodeKind`, `CheckState`) rather than boolean flags.
*   **Semantic Types**: Prefer `data Phase = Opening | Middlegame` over integers or strings.
*   **Explicit State**: Engine state (TT, History) should be passed explicitly or via `ReaderT`/`StateT`, not global `IORef`s (where possible, though performance may dictate `IORef` in `IO`).

## Future Roadmap

See `proposal.md` or the project roadmap for upcoming architectural phases.
