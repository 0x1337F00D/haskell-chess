with open('src/Chess/Engine/TT.hs', 'r') as f:
    tt_content = f.read()

tt_content = tt_content.replace('    newTT(..),\n', '    newTT,\n')
tt_content = tt_content.replace('    TT(..),\n', '    TT(..),\n')

with open('src/Chess/Engine/TT.hs', 'w') as f:
    f.write(tt_content)
