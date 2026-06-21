with open('src/Chess/Engine/TT.hs', 'r') as f:
    tt_content = f.read()

tt_content = tt_content.replace('    probeTT,\n', '')

with open('src/Chess/Engine/TT.hs', 'w') as f:
    f.write(tt_content)
