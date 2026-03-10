sed -i 's/oldFlag, //' src/Chess/Engine/TT.hs
sed -i 's/import Control.Monad.ST (RealWorld, stToIO)/import Control.Monad.ST (RealWorld)/' src/Chess/NNUE/Flat.hs
sed -i 's/import Control.Monad.ST.Unsafe (unsafeIOToST)//' src/Chess/NNUE/Flat.hs
sed -i 's/import Data.Int//' src/Chess/NNUE/Flat.hs
sed -i 's/floor $ 0.5 + log (fromIntegral d)/floor $ (0.5 :: Double) + log (fromIntegral d)/' src/Chess/Engine/Search/Pruning.hs
