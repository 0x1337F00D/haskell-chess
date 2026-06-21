with open('src/Chess/Engine/Search/Quiescence.hs', 'r') as f:
    q_content = f.read()

q_patch_search = """        let staticEval = case ttEntry of
                Just (_, s, _, TTEval) -> Just s
                _ -> Nothing"""

q_patch_replace = """        let staticEval = if ttHit && ttFlag == TTEval
                then Just ttScore
                else Nothing"""

q_content = q_content.replace(q_patch_search, q_patch_replace)

with open('src/Chess/Engine/Search/Quiescence.hs', 'w') as f:
    f.write(q_content)
