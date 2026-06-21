with open('src/Chess/Engine/TT.hs', 'r') as f:
    tt_content = f.read()

tt_content = tt_content.replace('{-# INLINE probeTT #-}\nprobeTT :: TT -> Word64 -> IO (Maybe (Move, Int, Depth, TTFlag))\nprobeTT (TT v mask) !key = do\n    let !idx = ttIndex mask key\n    !entryKey <- UM.unsafeRead v idx\n    if entryKey == key\n    then do\n        !entryData <- UM.unsafeRead v (idx + 1)\n        let (!m, !s, !d, !f, _) = unpackData entryData\n        return $ Just (m, s, d, f)\n    else return Nothing\n\n', '')

with open('src/Chess/Engine/TT.hs', 'w') as f:
    f.write(tt_content)

with open('src/Chess/Engine/Search/AlphaBeta.hs', 'r') as f:
    ab_content = f.read()

ab_content = ab_content.replace('probeTT, ', '')
ab_content = ab_content.replace('let (ttHit, ttMove, ttScore, ttDepth, ttFlag)', 'let (ttHit, ttMove, ttScore, _, ttFlag)')

with open('src/Chess/Engine/Search/AlphaBeta.hs', 'w') as f:
    f.write(ab_content)

with open('src/Chess/Engine/Search/Quiescence.hs', 'r') as f:
    q_content = f.read()

q_content = q_content.replace('probeTT, ', '')
q_content = q_content.replace('let (ttHit, ttMove, ttScore, ttDepth, ttFlag)', 'let (ttHit, ttMove, ttScore, _, ttFlag)')

with open('src/Chess/Engine/Search/Quiescence.hs', 'w') as f:
    f.write(q_content)
