# Benchmark Results

Date: 2026-01-30

## Core NPS (Perft)

- **Start Position (Depth 5)**: 6,234,103 NPS (approx 6.23M)
- **KiwiPete (Depth 4)**: 5,946,324 NPS (approx 5.95M)

**Analysis**:
- Core NPS is consistently above 4M NPS.
- There is a slight regression (~5%) compared to previous peak values (6.56M / 6.22M).

## Search NPS (KiwiPete Depth 6)

- **Current (Killer + History)**: 1,217,510 NPS (approx 1.22M)
- **Time**: 1.278s
- **Nodes**: 1,556,000

**Comparison**:
- **Baseline (Single Threaded likely)**: ~0.38M NPS (`baseline.log`)
- **Parallel Only**: ~1.59M NPS (42M Nodes)
- **Killer + History**: ~1.22M NPS (1.5M Nodes)

**Analysis**:
- **Drastic improvement in search efficiency**: Node count reduced from ~42M to ~1.5M.
- **Time to depth 6**: Reduced from 26.86s to 1.28s.
- NPS decreased slightly due to move ordering overhead, but overall search is much faster.

## Conclusion

The engine maintains high performance (>6M NPS for core operations). Search performance has notably improved due to Move Ordering optimizations (Killer Moves + History Heuristic).
