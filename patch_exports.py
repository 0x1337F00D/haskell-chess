with open('src/Chess/Engine/TT.hs', 'r') as f:
    tt_content = f.read()

tt_content = tt_content.replace('module Chess.Engine.TT where', 'module Chess.Engine.TT (\n    TT,\n    TTFlag(..),\n    newTT,\n    clearTT,\n    cloneTT,\n    packData,\n    unpackData,\n    ttIndex,\n    probeTT,\n    probeTTFast,\n    unpackDataFast,\n    storeTT\n) where')

with open('src/Chess/Engine/TT.hs', 'w') as f:
    f.write(tt_content)
