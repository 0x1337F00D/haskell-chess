#!/bin/bash
sed -i 's/import qualified Chess.Bitboard as BB//g' src/Chess/Core/Rules/Crazyhouse.hs
sed -i 's/import Data.Bits (testBit, countTrailingZeros, (.|.), setBit, clearBit)//g' src/Chess/Core/Rules/Crazyhouse.hs
sed -i '/import Data.Word (Word8)/d' src/Chess/Core/Rules/Crazyhouse.hs
sed -i 's/(CrazyhouseState wPocket bPocket promoted) = variantState ag/(CrazyhouseState wPocket bPocket prevPromoted) = variantState ag/g' src/Chess/Core/Rules/Crazyhouse.hs
sed -i 's/prevPromoted/promotedState/g' src/Chess/Core/Rules/Crazyhouse.hs
sed -i 's/promotedState .|./promoted .|./g' src/Chess/Core/Rules/Crazyhouse.hs
