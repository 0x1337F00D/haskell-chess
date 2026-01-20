# Arrow-based Evaluation in Haskell Chess

This document explores the use of Haskell's `Control.Arrow` to implement the chess evaluation function.

## Overview

The standard evaluation function (`Chess.Engine.Evaluation.evaluate`) computes material and positional scores and sums them up. The structure is simple:

```haskell
evaluate board = material board + positional board
```

We implemented an equivalent function using Arrows in `Chess.Engine.ArrowEval.evaluateArrow`:

```haskell
evaluateArrow :: Arrow a => a Board Score
evaluateArrow = proc (Board b gs _) -> do
    mat <- arr evalMaterial -< b
    pos <- arr evalPositional -< b
    let score = mat + pos
    returnA -< if turn gs == White then score else -score
```

## Performance

We benchmarked both implementations on 1,000,000 iterations over a set of different positions.

**Results:**
- **Standard**: ~2.07 M iter/s
- **Arrow**: ~2.16 M iter/s

The Arrow implementation performed slightly better (~4%), though this is likely within the margin of error or due to minor GHC optimization differences (e.g., inlining or register allocation). Effectively, for the `(->)` arrow, the performance is identical to standard function composition.

## Architectural Implications

### Pros
- **Composability**: Arrows allow for building pipelines of computation where components can be swapped or inspected (if using an Arrow instance that supports inspection).
- **Point-free style**: Can lead to cleaner code for complex data flows.

### Cons
- **Complexity**: `proc` notation and Arrow combinators (`&&&`, `***`) are less familiar to many Haskell developers than standard Monads or Applicatives.
- **Overhead**: For simple functions, Arrows add no value over standard function application.

## Conclusion

While Arrows are a powerful abstraction, they do not offer significant performance or architectural advantages for the current structure of the chess engine's evaluation function. The standard functional approach is simpler and equally performant. However, `Chess.Engine.ArrowEval` is kept as a proof-of-concept for potential future use in more complex pipelines (e.g., if we move to a signal-processing model for evaluation tuning).
