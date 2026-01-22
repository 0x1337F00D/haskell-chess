{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

module PyChessCongruencySpec (spec) where

import Test.Hspec
import qualified Data.Set as Set
import qualified PyChessData as PD

import Chess.Core.Game.Internal
import Chess.Core.Rules
import Chess.Core.Perft (perft)
import Chess.Core.Move (toUCI)
import Chess.Core.Board.Internal (KnownColor, sColor, SColor(..))

spec :: Spec
spec = do
  describe "PyChess Congruency" $ do
    mapM_ runCase PD.cases

runCase :: PD.PyChessCase -> Spec
runCase (PD.PyChessCase variantStr fenStr depth expectedNodes expectedMoves) = do
  describe (variantStr ++ " FEN: " ++ fenStr) $ do

    it ("matches perft(" ++ show depth ++ ") node count") $ do
        case variantStr of
            "Standard" -> testPerft @'Standard gameFromFEN
            "Atomic" -> testPerft @'Atomic atomicGameFromFEN
            "KingOfTheHill" -> testPerft @'KingOfTheHill kingOfTheHillGameFromFEN
            "RacingKings" -> testPerft @'RacingKings racingKingsGameFromFEN
            "ThreeCheck" -> testPerft @'ThreeCheck threeCheckGameFromFEN
            "Crazyhouse" -> testPerft @'Crazyhouse crazyhouseGameFromFEN
            _ -> expectationFailure $ "Unknown variant: " ++ variantStr

    it "matches legal moves list" $ do
         case variantStr of
            "Standard" -> testMoves @'Standard gameFromFEN
            "Atomic" -> testMoves @'Atomic atomicGameFromFEN
            "KingOfTheHill" -> testMoves @'KingOfTheHill kingOfTheHillGameFromFEN
            "RacingKings" -> testMoves @'RacingKings racingKingsGameFromFEN
            "ThreeCheck" -> testMoves @'ThreeCheck threeCheckGameFromFEN
            "Crazyhouse" -> testMoves @'Crazyhouse crazyhouseGameFromFEN
            _ -> expectationFailure $ "Unknown variant: " ++ variantStr
  where
    testPerft :: forall v. ChessVariant v => (String -> Maybe (Game v 'Active)) -> Expectation
    testPerft parse = do
        case parse fenStr of
            Nothing -> if expectedNodes == 0 then return () else expectationFailure "Parse failed"
            Just (InProgressGame (ag :: ActiveGame v turn status)) ->
                case sColor @turn of
                    SWhite -> perft depth ag `shouldBe` expectedNodes
                    SBlack -> perft depth ag `shouldBe` expectedNodes
            Just _ -> expectationFailure "Game finished immediately?"

    testMoves :: forall v. ChessVariant v => (String -> Maybe (Game v 'Active)) -> Expectation
    testMoves parse = do
        case parse fenStr of
             Nothing ->
                 if null expectedMoves then return () else expectationFailure "Parse failed"
             Just (InProgressGame ag) -> do
                 let moves = generateLegalMoves ag
                 let uciMoves = Set.fromList $ map toUCI moves
                 let expectedSet = Set.fromList expectedMoves
                 let missing = Set.difference expectedSet uciMoves
                 let extra = Set.difference uciMoves expectedSet

                 if Set.null missing && Set.null extra
                    then return ()
                    else expectationFailure $
                         "Moves mismatch.\nMissing: " ++ show (Set.toList missing) ++
                         "\nExtra: " ++ show (Set.toList extra)
             Just _ -> do
                 if null expectedMoves then return () else expectationFailure "Game finished immediately but expected moves"
