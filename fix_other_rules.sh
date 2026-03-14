#!/bin/bash
sed -i 's/CrazyhouseState wPocket bPocket promoted/CrazyhouseState wPocket bPocket _promoted/g' src/Chess/Core/Rules/Crazyhouse.hs
sed -i 's/import Data.Bits ((.&.), complement, (.|.))/import Data.Bits ((.&.), complement)/g' src/Chess/Core/Rules/Atomic.hs
sed -i '/(from, to) = case m of/d' src/Chess/Core/Rules/Atomic.hs
