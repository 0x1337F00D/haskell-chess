# Architecture, Status, and Plans

This document consolidates the architectural design, current implementation status, and future optimization plans for the Haskell Chess Engine.

## 1. Overview: The Hybrid Architecture

The engine employs a **Hybrid Architecture** composed of two complementary layers:

1.  **Type-Safe Core Layer (`Chess.Core.*`)**
    *   **Goal**: Correctness by construction.
    *   **Mechanism**: Uses `DataKinds`, `GADTs`, and phantom types to enforce chess rules at compile time (e.g., turn order, check status).
    *   **State**: The `ActiveGame` type wraps the optimized `Chess.Board.Base.Board` and `Chess.Board.GameState` directly, minimizing conversion overhead.

2.  **Performance-Oriented Engine Layer (`Chess.Board.*`)**
    *   **Goal**: Raw execution speed.
    *   **Mechanism**: Bitboard-based representation, unboxed vectors, and specialized move generation.
    *   **State**: `Board` (bitboards + scores) and `GameState` (counters + rights).

## 2. Current Architecture & Implemented Optimizations

The following architectural proposals have been **implemented**:

### 2.1. Type-Indexed Check Status
*   **Source**: `TypeIndexedCheckStatus.md`, `ARCHITECTURE.md`
*   **Implementation**: The `ValidatedBoard` (and `ActiveGame`) type is indexed by `CheckStatus` (Safe | Checked). This allows the engine to statically dispatch specialized logic (e.g., evasion generation vs. standard generation) and eliminates "boolean blindness" regarding king safety.

### 2.2. Consolidated State Representation
*   **Source**: `proposal.md`
*   **Implementation**: `ActiveGame` in `Chess.Core` now contains a `gameState :: Chess.Board.GameState` field directly. This eliminates the need to allocate and convert state when crossing the Core/Engine boundary, and enables full Fischer Random support via the engine's bitboard-based castling rights.

### 2.3. Incremental Evaluation Context
*   **Source**: `INCREMENTAL_EVALUATION_PROPOSAL.md`
*   **Implementation**: The `Chess.Board.Base.Board` structure includes `{-# UNPACK #-}` fields for `scoreWhite`, `scoreBlack`, and `gamePhase`. These are updated incrementally during `makeMove` / `unsafePutPiece`.
*   **Benefit**: The evaluation function (`Chess.Engine.Evaluation.evaluate`) is now O(1) for material and PST terms, replacing the previous O(N) scan.

### 2.4. Extended `givesCheck`
*   **Source**: `ARCHITECTURAL_OPTIMIZATION_PROPOSAL.md`
*   **Implementation**: `Chess.Board.MoveGen.givesCheck` has been implemented to handle all move types (Quiet, Capture, EnPassant, Castling, Promotion).
*   **Benefit**: Allows determining if a move checks the opponent without applying the move fully to a new board state. (Integration into `ActiveGame` transitions is pending).

## 3. Performance Status

**Date**: 2026-02-01
**Source**: `BENCHMARKS.md`

*   **Search Performance**: ~500k NPS (KiwiPete Depth 6) searching check evasions in QS.
*   **Core NPS**: ~6.3M NPS (Bolt Optimization).
*   **Tactics**: Solves standard tactical suites (Fool's Mate, Scholar's Mate).
*   **Strength**: Beat restricted Stockfish (Depth 1) 10-0 at Depth 5.

## 4. Optimization Plans (Todos)

The following architectural improvements are proposed but **NOT yet implemented**:

### 4.1. Context-Aware Legality (`isLegalSafe`)
*   **Source**: `PROPOSAL.md`
*   **Problem**: `isLegal` currently performs an O(N) `isAttackedBy` scan for every pseudo-legal move, even in `Safe` positions (95% of cases).
*   **Plan**:
    1.  Implement a `pinned` bitboard calculation in `MoveGen`.
    2.  Implement `isLegalSafe`:
        *   If piece is NOT pinned: Move is legal (except King moves).
        *   If piece IS pinned: Check if move is along the pin ray (Bitwise AND).
    3.  Expose `legalGenMovesSafe` for `NotInCheck` boards.
*   **Expected Gain**: Significant speedup in perft and move generation.

### 4.2. Zero-Allocation Game State
*   **Source**: `RECOMMENDATION.md`, `architectural_optimization.md`
*   **Problem**: `GameState` is currently a heap-allocated record. It is allocated for every node in the search tree.
*   **Plan**:
    1.  Pack `turn`, `epSquare`, `halfmoveClock`, `fullmoveNumber`, and `castlingRights` into one or two `Word64` values.
    2.  Update `Chess.Board.Base.Board` to store these `Word64`s as `{-# UNPACK #-}` fields.
    3.  Use Pattern Synonyms to maintain a high-level API.
*   **Expected Gain**: Massive reduction in GC pressure (zero-allocation move application).

### 4.3. End-to-End Vectorized Search
*   **Source**: `ARCHITECTURE_PROPOSAL.md`
*   **Problem**: `MoveGen` produces Unboxed Vectors, but `Search` converts them to Lists (`[Move]`) for iteration. This causes allocation and pointer chasing.
*   **Plan**:
    1.  Refactor `Chess.Engine.Search` to operate directly on `Vector GenMove`.
    2.  Implement in-place sorting and partitioning for move ordering.
*   **Expected Gain**: Reduced allocation, better cache locality.

### 4.4. Incremental Check Integration
*   **Source**: `ARCHITECTURAL_OPTIMIZATION_PROPOSAL.md`
*   **Status**: Partial (`givesCheck` exists).
*   **Plan**: Ensure `ActiveGame` transitions (`makeMove`) use `givesCheck` to determine the new `CheckStatus` type index, rather than calling the expensive `isCheck` on the resulting board.
