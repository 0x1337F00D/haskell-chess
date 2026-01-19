module TimeSpec where

import Test.Hspec
import Chess.Time
import Chess.Types (Color(..), Outcome(..), Termination(..))

spec :: Spec
spec = do
  describe "Clock" $ do
    it "Initializes correctly" $ do
      let tc = Standard 300000 5000
      let clock = mkClock tc
      cWhiteTime clock `shouldBe` 300000
      cBlackTime clock `shouldBe` 300000
      cConfig clock `shouldBe` tc

    describe "updateClock" $ do
      it "Standard: subtracts elapsed and adds increment" $ do
        let tc = Standard 60000 2000 -- 60s + 2s
        let c0 = mkClock tc
        let c1 = updateClock c0 White 5000 -- White spent 5s
        cWhiteTime c1 `shouldBe` (60000 - 5000 + 2000)
        cBlackTime c1 `shouldBe` 60000

      it "Delay: subtracts elapsed (over delay) and adds increment" $ do
        let tc = Delay 60000 5000 2000 -- 60s + 5s delay + 2s inc
        let c0 = mkClock tc
        -- Move < Delay
        let c1 = updateClock c0 White 3000
        cWhiteTime c1 `shouldBe` (60000 - 0 + 2000)

        -- Move > Delay
        let c2 = updateClock c0 Black 7000 -- 7s used
        cBlackTime c2 `shouldBe` (60000 - (7000 - 5000) + 2000)

      it "MoveTime: resets to perMove, flags if exceeded" $ do
        let tc = MoveTime 5000
        let c0 = mkClock tc

        -- Success
        let c1 = updateClock c0 White 4000
        cWhiteTime c1 `shouldBe` 5000
        isFlagged c1 White `shouldBe` False

        -- Fail
        let c2 = updateClock c0 Black 5001
        cBlackTime c2 `shouldBe` (-1)
        isFlagged c2 Black `shouldBe` True

    describe "isFlagged" $ do
      it "Detects timeout" $ do
        let tc = Standard 1000 0
        let c0 = mkClock tc
        let c1 = updateClock c0 White 1001
        isFlagged c1 White `shouldBe` True
        isFlagged c1 Black `shouldBe` False

    describe "clockOutcome" $ do
      it "Returns Timeout if flagged" $ do
        let tc = Standard 1000 0
        let c0 = mkClock tc
        let c1 = updateClock c0 White 1001
        clockOutcome c1 White `shouldBe` Just (Outcome Timeout (Just Black))
