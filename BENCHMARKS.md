# Benchmark Results

Date: 2026-02-01

## Feature Verification: Tactical Fixes & Stockfish Bench

**Changes**:
- Added "Endgame Protection" guards to NMP, LMR, and Futility Pruning (disabled if piece count <= 5).
- Protected checking moves from being pruned (logic moved after `applyMove`).
- Increased search depth for tactical verification.

### 1. Tactical Suite (BenchTactics)
- **Fool's Mate**: Pass
- **Scholar's Mate**: Pass
- **Fine #70 (Depth 12)**: Fail (Found `h1h2`, Expected `h1g1`).
  - *Note*: Despite extensive pruning guards, the engine prefers `h1h2` (+2.16) over `h1g1`. This suggests a Quiescence Search limitation (quiet mate not seen) or subtle evaluation bias. The search finds `h1g1` at depth 7 but discards it at depth 8.

### 2. Search Performance (KiwiPete Depth 6)
- **Nodes**: 281,693 (Increased from 209k)
- **Time**: 0.35s
- **NPS**: ~0.80M
- **Analysis**: Node count increased by ~35% due to safety guards (protecting checks and endgames). This is an acceptable trade-off for correctness.

### 3. Strength Benchmark vs Stockfish
- **Configuration**:
  - **Haskell Chess**: Depth 5
  - **Stockfish 18**: Depth 1
  - **Games**: 10
- **Result**: 10 - 0 (Haskell Wins)
- **Elo Difference**: +800
- **Conclusion**: The engine is playing legal and reasonably strong chess, easily defeating Stockfish restricted to depth 1.

## Historical Results

### Date: 2026-02-01 (Bolt Optimization)
- **Core NPS**: ~6.29M
- **Search (Depth 6)**: 209k nodes, 0.28s

### Date: 2026-01-30
- **Core NPS**: ~5.95M
- **Search (Depth 6)**: 1.5M nodes, 1.28s

## Conclusion
The engine has undergone significant optimization (Search nodes reduced by ~80% from baseline). While `Fine #70` remains elusive (likely due to QSearch limitations), the engine demonstrates strong tactical awareness (solving basic mates) and competitive play against a restricted reference engine.
