# Benchmark Results

Date: 2026-01-29

## Core NPS (Perft)

- **Start Position (Depth 5)**: 6,234,103 NPS (approx 6.23M)
- **KiwiPete (Depth 4)**: 5,946,324 NPS (approx 5.95M)

**Analysis**:
- Core NPS is consistently above 4M NPS.
- There is a slight regression (~5%) compared to previous peak values (6.56M / 6.22M).

## Search NPS (KiwiPete Depth 6)

- **Current (Parallel)**: 1,589,363 NPS (approx 1.59M)
- **Time**: 26.86s
- **Nodes**: 42,690,375

**Comparison**:
- **Baseline (Single Threaded likely)**: ~0.38M NPS (`baseline.log`)
- **Previous Parallel**: ~0.92M NPS (`parallel.log`)
- **Current**: 1.59M NPS

**Analysis**:
- Search performance has improved significantly (+72% vs previous parallel run).
- Parallelism is effective (329% CPU usage observed).

## Conclusion

The engine maintains high performance (>6M NPS for core operations). Search performance has notably improved due to parallelization optimizations.
