## 2024-05-19 - Test Entry
**Learning:** Checking journal creation.
**Action:** Proceed with finding bottleneck.

## 2024-05-19 - Removed Maybe and tuple allocations in probeTT
**Learning:** Returning a boxed `Maybe (Move, Int, Depth, TTFlag)` from `probeTT` forces heap allocation in the critical path. GHC couldn't unbox this effectively across module boundaries even with `INLINE`.
**Action:** Replaced `probeTT` with `probeTTFast`, returning an unboxed-like tuple `(Word64, Word64)` containing the raw TT entry key and data. This allows the caller to perform the hit check and unpack only on a hit, drastically reducing GC pressure without relying on explicit unboxed tuples.

## 2024-05-19 - Removed Maybe and tuple allocations in probeTT
**Learning:** Returning a boxed `Maybe (Move, Int, Depth, TTFlag)` from `probeTT` forces heap allocation in the critical path. GHC couldn't unbox this effectively across module boundaries even with `INLINE`.
**Action:** Replaced `probeTT` with `probeTTFast`, returning an unboxed-like tuple `(Word64, Word64)` containing the raw TT entry key and data. This allows the caller to perform the hit check and unpack only on a hit, drastically reducing GC pressure without relying on explicit unboxed tuples.
