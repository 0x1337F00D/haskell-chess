# Performance Tracking

| Commit Hash | Description | Category | NPS |
| ----------- | ----------- | -------- | --- |
| `d7eee1b` | ⚡ Bolt: [Discovery Candidates Propagation] | Search | Consistent or slightly improved |
| `d19ab16` | Introduce perftExecuteMove to decouple perft traversal from standard checkmate detection | Perft | ~2M NPS to ~17M NPS |
| `2f24ee9` | optimize check detection in search with discovery candidates | Search | 18% NPS |
| `206212c` | Refactor isLegal to use zero-allocation isLegalGeneric | Start | +25% NPS |
| `206212c` | Refactor isLegal to use zero-allocation isLegalGeneric | Kiwipete | +3% NPS |
| `206212c` | Refactor isLegal to use zero-allocation isLegalGeneric | Atomic | +14% NPS |
| `a7046b7` | Refactor MoveGen to use single-pass safe generation and unify perft | Start Position | 5.88M NPS |
| `a7046b7` | Refactor MoveGen to use single-pass safe generation and unify perft | Kiwipete | 34.75M NPS |
| `f90e97d` | ⚡ Bolt: Optimized pseudo-legal move generation | Start Position | ~6.36M NPS (+4.1%) |
| `f90e97d` | ⚡ Bolt: Optimized pseudo-legal move generation | Kiwipete | ~39.72M NPS (+17.4%) |
| `f90e97d` | ⚡ Bolt: Optimized pseudo-legal move generation | Atomic | ~2.14M NPS (+35.8%) |
| `dcbad8a` | Add INLINE pragma to pstValue for performance optimization | Start Position | ~3.5M NPS -> ~4.0M NPS |
| `dcbad8a` | Add INLINE pragma to pstValue for performance optimization | Kiwipete | ~8.7M NPS -> ~8.3M NPS |
| `a53cb9c` | perf: fuse pseudo-legal move generation into single pass | General | NPS: ~6.29M -> ~6.76M (+7.5%) |
| `f696594` | Refactor GenMove to use bit-packed Word64 | Perft | ~15% regression |
| `3eee8ad` | Optimize search with advanced pruning and safety guards | Kiwipete | ~0.38M NPS |
| `768ad2f` | ⚡ Bolt: Optimize sliding piece attacks with INLINE pragmas | Start Position | 6.27M NPS (+3.1%) |
| `768ad2f` | ⚡ Bolt: Optimize sliding piece attacks with INLINE pragmas | Kiwipete | 6.31M NPS (+4.1%) |
| `95de08d` | Optimize pawn capture generation using accumulators | Start Position | ~5.95M NPS |
| `95de08d` | Optimize pawn capture generation using accumulators | Kiwipete | ~6.14M NPS |
| `37a879a` | Bolt: Optimize `isAttackedBy` with direct field access | Core | See commit |
| `37a879a` | Bolt: Optimize `isAttackedBy` with direct field access | Search | See commit |
| `21add31` | Implement CI benchmark summary generation | Core | N/A |
| `1c1daed` | Refactor search parallelization to use dynamic root move scheduling | General | Previous NPS: ~931k |
| `1c1daed` | Refactor search parallelization to use dynamic root move scheduling | General | New NPS: ~1.38M |
| `0bdcec4` | Refactor GenMove to categorical Sum Type (Fix Warnings) | Search | -30% NPS |
| `d66431e` | Bolt: Optimize applyMoveBase with unsafeMovePiece | Kiwipete | 4.5% NPS |
| `dc660ed` | Optimize perft by removing redundant move execution and mate checks | Start Position | ~5.2M NPS |
| `e22cccc` | Fix critical search NPS regression by optimizing build and fixing alpha-beta pruning. | Search | 1k NPS |
| `e22cccc` | Fix critical search NPS regression by optimizing build and fixing alpha-beta pruning. | Search | 6M NPS |
| `f4bb935` | Cache PieceType in StandardMove for NPS optimization | Start Position | +1.5%, +2.2% |
| `1789479` | Benchmark vs PyChess and optimize move generation NPS | Pychess | ~72k NPS |
| `1789479` | Benchmark vs PyChess and optimize move generation NPS | Haskell | See commit |
| `d627c2d` | ⚡ Bolt: Optimize applyMove with direct bitboard lookups | Start Position | 3.5% NPS |
| `e3f7ad5` | Revert "Optimize ActiveGame NPS by removing redundant gameBoard" | General | See commit |
| `c5cdf46` | Refactor ActiveGame to remove redundant high-level board for NPS improvement | General | See commit |
| `f4485f7` | Optimize move generation by avoiding pieceAt calls | Kiwipete | ~4.00M NPS |
| `b7a3d61` | Perf: Optimize putPiece with incremental bitboard updates | General | See commit |
