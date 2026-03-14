#!/bin/bash
sed -i 's/let Just (b2, gs2) =/let Just (_b2, _gs2) =/g' app/Main.hs
sed -i 's/let Just b = parseFen fen/let Just _b = parseFen fen/g' scripts/BenchSearch.hs
