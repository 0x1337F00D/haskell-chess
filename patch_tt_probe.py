with open('src/Chess/Engine/TT.hs', 'r') as f:
    tt_content = f.read()

probe_tt_patch = """{-# INLINE probeTT #-}
probeTT :: TT -> Word64 -> IO (Maybe (Move, Int, Depth, TTFlag))
probeTT (TT v mask) !key = do
    let !idx = ttIndex mask key
    !entryKey <- UM.unsafeRead v idx
    if entryKey == key
    then do
        !entryData <- UM.unsafeRead v (idx + 1)
        let (!m, !s, !d, !f, _) = unpackData entryData
        return $ Just (m, s, d, f)
    else return Nothing

{-# INLINE probeTTFast #-}"""

tt_content = tt_content.replace('{-# INLINE probeTTFast #-}', probe_tt_patch)

with open('src/Chess/Engine/TT.hs', 'w') as f:
    f.write(tt_content)
