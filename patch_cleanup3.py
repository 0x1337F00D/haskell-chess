with open('src/Chess/Engine/Search/AlphaBeta.hs', 'r') as f:
    ab_content = f.read()

ab_content = ab_content.replace('let (ttHit, ttMove, ttScore, ttDepth, ttFlag) = if entryKey == hash\n                            then let (!m, !s, !td, !f) = unpackDataFast entryData\n                                 in (td >= depth, Just m, s, td, f)', 'let (ttHit, ttMove, ttScore, _, ttFlag) = if entryKey == hash\n                            then let (!m, !s, !td, !f) = unpackDataFast entryData\n                                 in (td >= depth, Just m, s, td, f)')

with open('src/Chess/Engine/Search/AlphaBeta.hs', 'w') as f:
    f.write(ab_content)
