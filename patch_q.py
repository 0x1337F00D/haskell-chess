with open('src/Chess/Engine/Search/Quiescence.hs', 'r') as f:
    q_content = f.read()

q_content = q_content.replace('probeTTFast, unpackDataFast, storeTT', 'probeTT, probeTTFast, unpackDataFast, storeTT')

with open('src/Chess/Engine/Search/Quiescence.hs', 'w') as f:
    f.write(q_content)
