module PyChessCongruencySpec (spec) where

import Test.Hspec
import Chess (parseFen)
import Chess.Board (Board, legalMoves, applyMove)
import Chess.Board.Uci (uci)
import qualified Data.Set as Set
import qualified PyChessData as PD

perft :: Int -> Board -> Int
perft 0 _ = 1
perft depth board =
    let moves = legalMoves board
    in if depth == 1
       then length moves
       else sum $ map (\m -> perft (depth - 1) (applyMove board m)) moves

spec :: Spec
spec = do
  describe "PyChess Congruency" $ do
    mapM_ runCase PD.cases

runCase :: PD.PyChessCase -> Spec
runCase (PD.PyChessCase fenStr depth expectedNodes expectedMoves) = do
  describe ("FEN: " ++ fenStr) $ do
    let mBoard = parseFen fenStr

    it "parses FEN correctly" $ do
        mBoard `shouldSatisfy` \x -> case x of Just _ -> True; Nothing -> False

    case mBoard of
        Nothing -> return ()
        Just board -> do
            it ("matches perft(" ++ show depth ++ ") node count") $ do
                perft depth board `shouldBe` expectedNodes

            it "matches legal moves list" $ do
                let moves = legalMoves board
                let uciMoves = Set.fromList $ map uci moves
                let expectedSet = Set.fromList expectedMoves

                -- Check for extra or missing moves
                let missing = Set.difference expectedSet uciMoves
                let extra = Set.difference uciMoves expectedSet

                if Set.null missing && Set.null extra
                    then return ()
                    else expectationFailure $
                         "Moves mismatch.\nMissing: " ++ show (Set.toList missing) ++
                         "\nExtra: " ++ show (Set.toList extra)
