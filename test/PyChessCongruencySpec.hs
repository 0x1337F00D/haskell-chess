{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE RankNTypes #-}

module PyChessCongruencySpec (spec) where

import Test.Hspec
import Chess.Core.Game hiding (Variant)
import Chess.Core.Game.Internal (Game(..), ActiveGame(..), SCheckStatus(..), VariantState)
import Chess.Core.Rules
import Chess.Core.Board.Internal (KnownColor(..), SColor(..), Color(..), sColor)
import Chess.Core.Move (toUCI)
import qualified Chess.Board.Fen as Fen
import qualified Chess.Core.Fen as CoreFen
import qualified Data.Set as Set
import qualified Data.ByteString.Char8 as BSC
import qualified PyChessData as PD
import qualified Chess.Types as T
import Chess.Types (mkDepth, Depth, unDepth)

-- | Wrapper for any game variant
data SomeGame = forall v. (ChessVariant v, Show (Game v 'Active)) => SomeGame (Game v 'Active)

instance Show SomeGame where
  show (SomeGame g) = show g

-- | Load game based on variant string and FEN
loadGame :: String -> String -> Maybe SomeGame
loadGame "Standard" s = fmap SomeGame (gameFromFEN s)
loadGame "Crazyhouse" s = fmap SomeGame (crazyhouseGameFromFEN s)
loadGame "Chess960" s = fmap SomeGame (fischerRandomGameFromFEN s)
loadGame "Three-check" s = loadThreeCheck s
loadGame "Atomic" s = loadSimple @'Atomic s
loadGame "King of the Hill" s = loadSimple @'KingOfTheHill s
loadGame "Racing Kings" s = loadSimple @'RacingKings s
loadGame "Antichess" s = loadSimple @'Antichess s
loadGame "Horde" s = loadSimple @'Horde s
loadGame _ _ = Nothing

dispatchColor :: forall v. (ChessVariant v, VariantState v ~ (), Show (Game v 'Active)) => String -> Maybe SomeGame
dispatchColor s = do
  (b, gs) <- Fen.parseFen s
  -- Parse turn manually from FEN string as we need it at type level
  let parts = words s
  if length parts < 2 then Nothing else do
      let turnStr = parts !! 1
      case turnStr of
         "w" -> return $ SomeGame (InProgressGame (ActiveGame b gs () SUnchecked :: ActiveGame v 'White 'Unchecked))
         "b" -> return $ SomeGame (InProgressGame (ActiveGame b gs () SUnchecked :: ActiveGame v 'Black 'Unchecked))
         _ -> Nothing

dispatchThreeCheck :: String -> Maybe SomeGame
dispatchThreeCheck s = do
  (b, gs, checks) <- CoreFen.parseThreeCheckFen s
  let parts = words s
  if length parts < 2 then Nothing else do
      let turnStr = parts !! 1
      case turnStr of
         "w" -> return $ SomeGame (InProgressGame (ActiveGame b gs checks SUnchecked :: ActiveGame 'ThreeCheck 'White 'Unchecked))
         "b" -> return $ SomeGame (InProgressGame (ActiveGame b gs checks SUnchecked :: ActiveGame 'ThreeCheck 'Black 'Unchecked))
         _ -> Nothing

loadSimple :: forall v. (ChessVariant v, VariantState v ~ (), Show (Game v 'Active)) => String -> Maybe SomeGame
loadSimple = dispatchColor @v

loadThreeCheck :: String -> Maybe SomeGame
loadThreeCheck = dispatchThreeCheck

-- Helper to bring KnownColor (Opposite c) into scope
withOpposite :: forall c r. KnownColor c => (KnownColor (Opposite c) => r) -> r
withOpposite f = case sColor @c of
  SWhite -> f
  SBlack -> f

-- Helper to run Perft
runPerft :: Depth -> SomeGame -> Int
runPerft d (SomeGame (InProgressGame (ag :: ActiveGame v c s))) =
  withOpposite @c $ perftVariant @v (unDepth d) ag
runPerft _ _ = 0

-- Helper to get legal moves
getLegalMoves :: SomeGame -> [String]
getLegalMoves (SomeGame (InProgressGame (ag :: ActiveGame v c s))) =
  -- generateMoves does NOT require KnownColor (Opposite c) directly, but Move c requires KnownColor c
  -- generateMoves signature: KnownColor c => ActiveGame v c s -> [Move c]
  map (BSC.unpack . toUCI) (generateMoves ag)
getLegalMoves _ = []

spec :: Spec
spec = do
  describe "PyChess Congruency" $ do
    mapM_ runCase PD.cases

runCase :: PD.PyChessCase -> Spec
runCase (PD.PyChessCase variantStr fenStr depth expectedNodes expectedMoves) = do
  describe (variantStr ++ " FEN: " ++ fenStr) $ do
    let mGame = loadGame variantStr fenStr

    it "parses FEN correctly" $ do
        mGame `shouldSatisfy` \x -> case x of Just _ -> True; Nothing -> False

    case mGame of
        Nothing -> return ()
        Just game -> do
            it ("matches perft(" ++ show depth ++ ") node count") $ do
                runPerft (mkDepth depth) game `shouldBe` expectedNodes

            it "matches legal moves list" $ do
                let uciMoves = Set.fromList (getLegalMoves game)
                let expectedSet = Set.fromList expectedMoves

                -- Check for extra or missing moves
                let missing = Set.difference expectedSet uciMoves
                let extra = Set.difference uciMoves expectedSet

                if Set.null missing && Set.null extra
                    then return ()
                    else expectationFailure $
                         "Moves mismatch.\nMissing: " ++ show (Set.toList missing) ++
                         "\nExtra: " ++ show (Set.toList extra)
