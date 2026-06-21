with open('src/Chess/Engine/TT.hs', 'r') as f:
    tt_content = f.read()

tt_patch = """{-# INLINE unpackDataFast #-}
unpackDataFast :: Word64 -> (Move, Int, Depth, TTFlag)
unpackDataFast !w =
    let !m = coerce (fromIntegral (w .&. 0xFFFF) :: Word16) :: Move
        !s = fromIntegral ((w `shiftR` 16) .&. 0xFFFF) - 32768
        !d = Depth (fromIntegral ((w `shiftR` 32) .&. 0xFF))
        !f = toEnum (fromIntegral ((w `shiftR` 40) .&. 0x3))
    in (m, s, d, f)
"""

tt_content = tt_content.replace('{-# INLINE unpackDataFast #-}\nunpackDataFast :: Word64 -> (Move, Int, Depth, TTFlag)\nunpackDataFast !w = let (!m, !s, !d, !f, _) = unpackData w in (m, s, d, f)\n', tt_patch)

with open('src/Chess/Engine/TT.hs', 'w') as f:
    f.write(tt_content)
