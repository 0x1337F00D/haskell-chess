with open('src/Chess/Engine/Search/Quiescence.hs', 'r') as f:
    q_content = f.read()

q_patch_search = """    ttEntry <- probeTT tt hash

    -- Use CheckState from context"""

q_patch_replace = """    (!entryKey, !entryData) <- probeTTFast tt hash
    let (ttHit, _, ttScore, _, ttFlag) = if entryKey == hash
            then let (!m, !s, !td, !f) = unpackDataFast entryData
                 in (True, Just m, s, td, f)
            else (False, Nothing, 0, depthZero, TTExact)

    -- Use CheckState from context"""

q_content = q_content.replace(q_patch_search, q_patch_replace)

q_patch2_search = """    let (ttHit, _, ttScore, _, ttFlag) = if entryKey == hash
            then let (!m, !s, !td, !f) = unpackDataFast entryData
                 in (True, Just m, s, td, f)
            else (False, Nothing, 0, depthZero, TTExact)"""
q_content = q_content.replace(q_patch2_search, q_patch2_search.replace('let (ttHit, _, ttScore, _, ttFlag)', 'let (ttHit, ttMove, ttScore, _, ttFlag)'))

with open('src/Chess/Engine/Search/Quiescence.hs', 'w') as f:
    f.write(q_content)
