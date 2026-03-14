#!/bin/bash

# src/Chess/Core/Rules/Common.hs
sed -i 's/import Data.Bits (setBit, clearBit, (.&.), (.|.), testBit, complement)/import Data.Bits (setBit, (.&.), (.|.), complement)/g' src/Chess/Core/Rules/Common.hs
sed -i '/import Data.Word (Word8)/d' src/Chess/Core/Rules/Common.hs
sed -i '/import qualified Data.Vector.Unboxed as U/d' src/Chess/Core/Rules/Common.hs
sed -i '/import qualified Data.Vector.Unboxed.Mutable as UM/d' src/Chess/Core/Rules/Common.hs
sed -i '/import Control.Monad (forM_)/d' src/Chess/Core/Rules/Common.hs
sed -i '/mmToPieceType MQueen = Queen/d' src/Chess/Core/Rules/Common.hs
sed -i 's/promoted = T.Piece c (toPieceType promo)/_promoted = T.Piece c (toPieceType promo)/g' src/Chess/Core/Rules/Common.hs
sed -i 's/let promoted = T.Piece (toColor (colorVal @c)) (toPieceType p)/let _promoted = T.Piece (toColor (colorVal @c)) (toPieceType p)/g' src/Chess/Core/Rules/Common.hs
