# Construction Sites for haskell-chess vs Stockfish

To become competitive with Stockfish and achieve similar performance, the `haskell-chess` engine needs to implement a variety of advanced search heuristics and evaluation improvements. This document outlines the key construction sites, expected effort, implementation ideas, and expected Elo gains based on the structure of modern chess engines like Stockfish.

## 1. Advanced Search Heuristics (Alpha-Beta Search)

### 1.1 Mate Distance Pruning
*   **Description**: Pruning branches where a mate is found earlier than the current alpha/beta bounds allow.
*   **Expected Effort**: Low
*   **Implementation Idea**: Add logic at the beginning of `alphaBetaBody` to adjust alpha/beta based on `mateValue` and depth, and return immediately if `alpha >= beta`.
*   **Expected Elo Gain**: +10-20 Elo

### 1.2 Transposition Table (TT) Improvements
*   **Description**: Improve TT probe/store logic, including aging, replacement strategies, and handling of excluded moves for Singular Extensions.
*   **Expected Effort**: Medium
*   **Implementation Idea**: Enhance `probeTT` and `storeTT` in `TT.hs`. Add a `TTEntry` struct with more flags (e.g., age, bound type). Implement a replacement scheme based on depth and age.
*   **Expected Elo Gain**: +30-50 Elo

### 1.3 Razoring
*   **Description**: Pruning near the leaves if the static evaluation is significantly below alpha, assuming the opponent can easily refute.
*   **Expected Effort**: Low-Medium
*   **Implementation Idea**: In `alphaBetaBody`, before the move loop, if `depth <= 3` and `staticEval + margin < alpha`, either return the static evaluation or drop directly into Quiescence Search.
*   **Expected Elo Gain**: +20-30 Elo

### 1.4 Futility Pruning
*   **Description**: Pruning moves near the leaves (e.g., depth 1 or 2) if the static evaluation plus a margin (based on piece values) is still below alpha.
*   **Expected Effort**: Medium
*   **Implementation Idea**: Inside the move loop of `alphaBetaBody`, if `depth <= 2`, the move is quiet, and `staticEval + margin <= alpha`, skip the search for this move.
*   **Expected Elo Gain**: +40-60 Elo

### 1.5 Null Move Pruning (NMP) Enhancements
*   **Description**: Improve the existing NMP by adding verification searches (to avoid Zugzwang issues) and tweaking the reduction factor based on depth and evaluation.
*   **Expected Effort**: Medium
*   **Implementation Idea**: Refine the `doNmp` condition and reduction calculation `R` in `alphaBetaBody`. Add a verification search if the evaluation is close to beta.
*   **Expected Elo Gain**: +20-40 Elo

### 1.6 Internal Iterative Reductions (IIR)
*   **Description**: Reducing the depth of search for nodes where no TT move is available, encouraging shallower searches first to populate the TT.
*   **Expected Effort**: Low
*   **Implementation Idea**: If `ttMove` is not present and depth is high (e.g., `depth >= 4`), reduce the search depth by 1 for subsequent moves.
*   **Expected Elo Gain**: +10-20 Elo

### 1.7 ProbCut
*   **Description**: A form of forward pruning that assumes if a shallow search with a high beta cutoff fails, a deeper search will also fail.
*   **Expected Effort**: High
*   **Implementation Idea**: Perform a shallow search with `beta + margin` before the main move loop. If it causes a cutoff, return beta. Requires careful tuning of margins.
*   **Expected Elo Gain**: +30-50 Elo

### 1.8 Singular Extensions
*   **Description**: Extending the search depth when there is only one valid move (or one move is significantly better than others) to resolve tactical sequences accurately.
*   **Expected Effort**: High
*   **Implementation Idea**: During the move loop, if a move's score is much higher than the second-best move, extend its depth. This requires tracking the top few moves or using a specialized "singular extension search".
*   **Expected Elo Gain**: +40-70 Elo

### 1.9 Late Move Reductions (LMR) Enhancements
*   **Description**: Improve the LMR logic by taking into account more factors like history scores, move ordering, and node type (PV vs. Cut node).
*   **Expected Effort**: Medium
*   **Implementation Idea**: Update the `lmrTable` and the reduction calculation `lmr` in `alphaBetaBody`. Add conditions to increase reduction for bad history moves or decrease it for good ones.
*   **Expected Elo Gain**: +50-80 Elo

## 2. Move Ordering and Quiescence Search

### 2.1 History Heuristics (Relative History, Countermove History)
*   **Description**: Keeping track of how often quiet moves cause cutoffs relative to how often they are played. Also, tracking history based on the previous move (Countermove History).
*   **Expected Effort**: Medium
*   **Implementation Idea**: Add more arrays to `SearchResources` in `Types.hs` to track history scores. Update `updateHistory` and `orderQuiets` in `Ordering.hs` to use these arrays.
*   **Expected Elo Gain**: +40-60 Elo

### 2.2 See (Static Exchange Evaluation) Improvements
*   **Description**: Enhancing the SEE function to handle pinned pieces, overloads, and more complex capturing scenarios.
*   **Expected Effort**: Medium
*   **Implementation Idea**: Refine the `see` function in `SEE.hs`. Integrate it tighter with move ordering and pruning decisions (e.g., pruning bad captures in Quiescence Search).
*   **Expected Elo Gain**: +20-40 Elo

### 2.3 Quiescence Search Pruning
*   **Description**: Adding pruning techniques specifically for Quiescence Search, such as delta pruning or pruning bad SEE captures.
*   **Expected Effort**: Medium
*   **Implementation Idea**: In `Quiescence.hs`, add a check before evaluating captures: if `staticEval + captureValue + margin < alpha`, prune the capture.
*   **Expected Elo Gain**: +30-50 Elo

## 3. Evaluation

### 3.1 NNUE (Efficiently Updatable Neural Networks)
*   **Description**: Replacing the hand-crafted evaluation function with a neural network trained on millions of positions.
*   **Expected Effort**: Very High
*   **Implementation Idea**: Implement the NNUE inference architecture in Haskell. Parse and load Stockfish's `.nnue` weights file. Integrate it into `evaluatePos` in `Evaluation.hs`. This is by far the biggest structural change needed to reach modern engine strength.
*   **Expected Elo Gain**: +500-1000 Elo

### 3.2 Tapered Evaluation (Hand-crafted Eval Backup)
*   **Description**: If NNUE is not implemented immediately, improving the hand-crafted evaluation by smoothly transitioning piece values and PSTs between opening, middlegame, and endgame phases.
*   **Expected Effort**: Medium
*   **Implementation Idea**: Add game phase calculation based on remaining material. Interpolate between opening and endgame values in `Evaluation.hs`. Currently `Evaluation.hs` seems somewhat basic.
*   **Expected Elo Gain**: +50-100 Elo

## 4. Move Generation and Core Infrastructure

### 4.1 Magic Bitboards / MoveGen Tuning
*   **Description**: Ensure move generation is as fast as possible, as this bounds the nodes per second (NPS) the search can achieve.
*   **Expected Effort**: Low-Medium
*   **Implementation Idea**: The codebase already seems to use Magic Bitboards (`Chess.Bitboard.MagicTables`). The main focus should be micro-optimizing the Hot paths in `MoveGen.hs` using profiling.
*   **Expected Elo Gain**: +10-20 Elo (via increased NPS)

### 4.2 Syzygy Tablebases
*   **Description**: Integrating Syzygy Tablebases directly into the search for perfect endgame play.
*   **Expected Effort**: High
*   **Implementation Idea**: Integrate a C library like Fathom via FFI, or write a native parser. Query tablebases at root and deep in the search to terminate branches early.
*   **Expected Elo Gain**: +30-50 Elo (in endgames)

## Summary

Implementing these features will significantly improve the playing strength of `haskell-chess`. The most impactful change by far would be the integration of **NNUE**, which is standard in all top-tier modern chess engines, including Stockfish. Following that, refining search heuristics like **LMR**, **Futility Pruning**, and **History Heuristics** will provide substantial Elo gains. The search algorithm currently implemented in `AlphaBeta.hs` has the foundation but lacks the deep layers of pruning that characterize Stockfish's `search.cpp`.
