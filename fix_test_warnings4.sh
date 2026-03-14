#!/bin/bash
sed -i 's/trustBoard _board/trustBoard board/g' test/EngineSpec.hs
sed -i 's/let Just _board = parseFen/let Just board = parseFen/g' test/EngineSpec.hs
sed -i 's/search _board tt/search board tt/g' test/EngineSpec.hs
