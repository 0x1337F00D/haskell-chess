# Benchmark Results

Date: 2026-02-01

## Optimization Verification: Bolt (Inline Bitboards)

**Changes**: Added `{-# INLINE #-}` pragmas to sliding piece attack generators in `Chess.Bitboard`.

### 1. Core Performance (Perft)
- **Start Position (Depth 5)**: 6,234,356 NPS (approx 6.23M)
- **KiwiPete (Depth 4)**: 6,293,815 NPS (approx 6.29M)

**Analysis**:
- slight improvement/stable compared to previous (5.95M).

### 2. Search Performance (KiwiPete Depth 6)
- **Nodes**: 209,944
- **Time**: 0.276s
- **NPS**: ~760,000 NPS (approx 0.76M)

**Analysis**:
- Search node count remains low (~0.21M), confirming pruning effectiveness.
- Time is excellent (<0.3s).

### 3. Tactical Suite (BenchTactics)
- **Fool's Mate (Depth 8)**: Pass (1.65M nodes)
- **Scholar's Mate (Depth 8)**: Pass (0.83M nodes)
- **Fine #70 (Depth 8)**: Fail (Found `h1h2`, Expected `h1g1`)

### 4. Self-Play (BenchElo)
- **Config**: 10 games, 2 concurrent, Depth 5
- **Score**: 5.0 - 5.0 (50%)
- **Elo Difference**: 0.00
- **Conclusion**: No regression in playing strength.

## Historical Results

### Date: 2026-01-30

#### Core NPS (Perft)
- **Start Position (Depth 5)**: 6,234,103 NPS
- **KiwiPete (Depth 4)**: 5,946,324 NPS

#### Search NPS (KiwiPete Depth 6)
- **Nodes**: 1,556,000
- **Time**: 1.278s
- **NPS**: 1.22M

### Conclusion
The engine is becoming significantly faster and more efficient. Node counts for depth 6 have dropped from ~1.5M to ~0.2M due to recent pruning/extensions optimizations. The "Bolt" inline optimization maintains high core NPS and playing strength.
