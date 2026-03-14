#!/bin/bash

# src/Chess/Engine/TT.hs
sed -i 's/oldFlag/_oldFlag/g' src/Chess/Engine/TT.hs

# src/Chess/NNUE/Flat.hs
sed -i '/import Data.Int/d' src/Chess/NNUE/Flat.hs
sed -i '/import Control.Monad.ST (RealWorld, stToIO)/d' src/Chess/NNUE/Flat.hs
sed -i '/import Control.Monad.ST.Unsafe (unsafeIOToST)/d' src/Chess/NNUE/Flat.hs

# src/Chess/Engine/Search/Pruning.hs
sed -i 's/0.5 + log (fromIntegral d) \* log (fromIntegral idx) \/ 2.0/(0.5 :: Double) + log (fromIntegral d :: Double) \* log (fromIntegral idx :: Double) \/ 2.0/g' src/Chess/Engine/Search/Pruning.hs

# tools/ConvertNnue.hs
sed -i 's/let ftIn  = 98304/let ftIn  = 98304 :: Int/g' tools/ConvertNnue.hs
sed -i 's/acc   = 256/acc   = 256 :: Int/g' tools/ConvertNnue.hs
sed -i 's/hid   = 32/hid   = 32 :: Int/g' tools/ConvertNnue.hs
sed -i 's/sc    = 400 -- Default scale/sc    = 400 :: Int32 -- Default scale/g' tools/ConvertNnue.hs
sed -i '/import Data.Word (Word32)/d' tools/ConvertNnue.hs
sed -i '/import qualified Data.ByteString as BS/d' tools/ConvertNnue.hs

# src/Chess/Board/Fen.hs
sed -i 's/import Control.Monad (foldM, guard)/import Control.Monad (foldM)/g' src/Chess/Board/Fen.hs

# src/Chess/Board/MoveGen/Common.hs
sed -i '/import Data.Word (Word64)/d' src/Chess/Board/MoveGen/Common.hs
sed -i '/{-# INLINE isQuiet #-}/,+15d' src/Chess/Board/MoveGen/Common.hs

# src/Chess/Board/Validation.hs
sed -i '/import qualified Data.Vector.Unboxed as U/d' src/Chess/Board/Validation.hs
sed -i 's/import Chess.Board.MoveGen (pseudoLegalMoves, isLegal, kingSquare, hasLegalMove)/import Chess.Board.MoveGen (kingSquare, hasLegalMove)/g' src/Chess/Board/Validation.hs

# src/Chess/Core/Move.hs
sed -i '/import Data.Monoid ((<>))/d' src/Chess/Core/Move.hs
sed -i 's/import Chess.Core.Board.Internal (squareToString, squareToBuilder)/import Chess.Core.Board.Internal (squareToBuilder)/g' src/Chess/Core/Move.hs

# src/Chess/Board.hs
sed -i 's/isCapture = testBit (Base.occupiedTotal b) toI/isCaptureMove = testBit (Base.occupiedTotal b) toI/g' src/Chess/Board.hs
sed -i 's/if isCapture/if isCaptureMove/g' src/Chess/Board.hs

# src/Chess/Board/Phase.hs
sed -i '/import Data.Kind (Type)/d' src/Chess/Board/Phase.hs

# src/Chess/Engine/Search/Ordering.hs
sed -i '/import Control.Monad (forM_)/d' src/Chess/Engine/Search/Ordering.hs
sed -i 's/pattern GenDrop, getBoard, pieces, getGenMove)/pattern GenDrop, getBoard, getGenMove)/g' src/Chess/Engine/Search/Ordering.hs
sed -i 's/import Chess.Engine.SEE (see, seeGen)/import Chess.Engine.SEE (seeGen)/g' src/Chess/Engine/Search/Ordering.hs

# src/Chess/Engine/Search/Quiescence.hs
sed -i 's/getBoard, state, pieces, applyLegalMove/getBoard, state, applyLegalMove/g' src/Chess/Engine/Search/Quiescence.hs
sed -i '/import qualified Chess.Board/d' src/Chess/Engine/Search/Quiescence.hs
sed -i '/import qualified Chess.Board.MoveGen as MoveGen/d' src/Chess/Engine/Search/Quiescence.hs
sed -i 's/import Chess.Types (Move, nullMove)/import Chess.Types (Move)/g' src/Chess/Engine/Search/Quiescence.hs

# src/Chess/Engine/Search/AlphaBeta.hs
sed -i 's/let !dcBitboard = KingSafety.discoveryCandidates (pieces board) (GS.turn (state board))/let !_dcBitboard = KingSafety.discoveryCandidates (pieces board) (GS.turn (state board))/g' src/Chess/Engine/Search/AlphaBeta.hs
sed -i 's/searchStage dcBitboard (lm:lms) !index inCheck staticEval !a !b !d !flag !bestScore !bestM !found !playedQuiets/searchStage dcBitboard (lm:lms) !index inCheck staticEval !a !b !d !flag !bestScore !bestM !_found !playedQuiets/g' src/Chess/Engine/Search/AlphaBeta.hs

# scripts/BenchEval.hs
sed -i '/import Control.Monad (forM_)/d' scripts/BenchEval.hs
sed -i 's/putStrLn $ "NPS: " ++ show nps/putStrLn $ "NPS: " ++ show (nps :: Double)/g' scripts/BenchEval.hs

# test/CoreSpec.hs
sed -i 's/CastlingRights(..), Pockets(..), CrazyhouseState(..), castlingWhiteKingSide, castlingWhiteQueenSide, castlingBlackKingSide, castlingBlackQueenSide, /CastlingRights(..), Pockets(..), /g' test/CoreSpec.hs
sed -i '/import qualified Chess.Types as T/d' test/CoreSpec.hs
sed -i 's/import Data.Bits ((.|.), (.&.))/import Data.Bits ((.&.))/g' test/CoreSpec.hs
sed -i '/import Data.Word (Word8)/d' test/CoreSpec.hs

# test/EngineSpec.hs
sed -i 's/let Just board = parseFen/let Just _board = parseFen/g' test/EngineSpec.hs

# test/PyChessCongruencySpec.hs
sed -i '/import qualified Chess.Types as T/d' test/PyChessCongruencySpec.hs
