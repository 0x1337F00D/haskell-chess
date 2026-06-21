with open('src/Chess/Engine/TT.hs', 'r') as f:
    tt_content = f.read()

tt_patch = """
{-# INLINE probeTTFast #-}
probeTTFast :: TT -> Word64 -> IO (Word64, Word64)
probeTTFast (TT v mask) !key = do
    let !idx = ttIndex mask key
    !entryKey <- UM.unsafeRead v idx
    !entryData <- UM.unsafeRead v (idx + 1)
    return (entryKey, entryData)

{-# INLINE unpackDataFast #-}
unpackDataFast :: Word64 -> (Move, Int, Depth, TTFlag)
unpackDataFast !w = let (!m, !s, !d, !f, _) = unpackData w in (m, s, d, f)
"""

tt_content = tt_content.replace('-- | Store in TT.', tt_patch + '\n-- | Store in TT.')
with open('src/Chess/Engine/TT.hs', 'w') as f:
    f.write(tt_content)

with open('src/Chess/Engine/Search/AlphaBeta.hs', 'r') as f:
    ab_content = f.read()

ab_content = ab_content.replace('probeTT, storeTT', 'probeTTFast, unpackDataFast, storeTT')

ab_patch1_search = """    ttEntry <- probeTT tt hash
    let ttMove = case ttEntry of Just (m, _, _, _) -> Just m; Nothing -> Nothing"""

ab_patch1_replace = """    (!entryKey, !entryData) <- probeTTFast tt hash
    let ttMove = if entryKey == hash
                 then let (!m, _, _, _) = unpackDataFast entryData in Just m
                 else Nothing"""

ab_content = ab_content.replace(ab_patch1_search, ab_patch1_replace)

ab_patch2_search = """                    ttEntry <- probeTT tt hash
                    let (ttMove, ttScore, ttDepth, ttFlag) = case ttEntry of
                            Just (m, s, d, f) -> (Just m, s, d, f)
                            Nothing -> (Nothing, 0, mkDepth (-1), TTExact)

                    let ttHit = isJust ttEntry && ttDepth >= depth"""

ab_patch2_replace = """                    (!entryKey, !entryData) <- probeTTFast tt hash
                    let (ttHit, ttMove, ttScore, ttDepth, ttFlag) = if entryKey == hash
                            then let (!m, !s, !td, !f) = unpackDataFast entryData
                                 in (td >= depth, Just m, s, td, f)
                            else (False, Nothing, 0, mkDepth (-1), TTExact)"""

ab_content = ab_content.replace(ab_patch2_search, ab_patch2_replace)

with open('src/Chess/Engine/Search/AlphaBeta.hs', 'w') as f:
    f.write(ab_content)

with open('src/Chess/Engine/Search/Quiescence.hs', 'r') as f:
    q_content = f.read()

q_content = q_content.replace('probeTT, storeTT', 'probeTTFast, unpackDataFast, storeTT')

q_patch_search = """    ttEntry <- probeTT tt hash
    let (ttMove, ttScore, ttDepth, ttFlag) = case ttEntry of
            Just (m, s, d, f) -> (Just m, s, d, f)
            Nothing -> (Nothing, 0, mkDepth (-1), TTExact)

    let ttHit = isJust ttEntry"""

q_patch_replace = """    (!entryKey, !entryData) <- probeTTFast tt hash
    let (ttHit, ttMove, ttScore, ttDepth, ttFlag) = if entryKey == hash
            then let (!m, !s, !td, !f) = unpackDataFast entryData
                 in (True, Just m, s, td, f)
            else (False, Nothing, 0, mkDepth (-1), TTExact)"""

q_content = q_content.replace(q_patch_search, q_patch_replace)

with open('src/Chess/Engine/Search/Quiescence.hs', 'w') as f:
    f.write(q_content)
