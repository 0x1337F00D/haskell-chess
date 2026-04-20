1. **Identify Bottleneck**: The hot path check evasion logic in `src/Chess/Board/MoveGen/Core.hs` currently uses `Square (fromMaybe 0 (lsb attackers))` heavily. It is evaluating `lsb` and pattern matching on a `Maybe Int`, and handling a fallback.
2. **Analysis**: We are invoking `lsb` *after* verifying `isSingleCheck` via `(attackers .&. (attackers - 1)) == 0` AND guarding it with `unless (attackers == 0)`. Therefore, `attackers` is strictly non-zero.
3. **Plan**:
   - Create total variations of `lsb` and `msb` in `Chess.Bitboard`: `lsbTotal` and `msbTotal`. These functions will be non-total by default, but safe to use in paths where `bb /= 0` is guaranteed. They will bypass the `Maybe` wrapper, using raw `countTrailingZeros` and `countLeadingZeros`.
   - Update `Chess.Bitboard` to export `lsbTotal` and `msbTotal`.
   - In `src/Chess/Board/MoveGen/Core.hs`, replace the `Square (fromMaybe 0 (lsb attackers))` calls with `Square (lsbTotal attackers)`.
   - Run benchmarks to measure performance impact.
4. **Verification**:
   - `cabal build bench-core`
   - `cabal run bench-core`
