module TablebaseSpec (spec) where

import Test.Hspec
import Chess.Tablebase

spec :: Spec
spec = do
  describe "Tablebase Local Probe" $ do
    describe "probeLocalWith (Mocked)" $ do
      it "parses standard Fathom output" $ do
        -- Assuming format: Probe: Win DTZ: 10
        let output = "Loading...\nProbe: Win DTZ: 10\n"
        probeLocalWith (\_ _ _ -> return output) "tool" "path" "fen"
          `shouldReturn` Right (SyzygyResult Win 10 0)

      it "parses Fathom output with WDL label" $ do
        let output = "Probe: WDL: Loss DTZ: -5"
        probeLocalWith (\_ _ _ -> return output) "tool" "path" "fen"
          `shouldReturn` Right (SyzygyResult Loss (-5) 0)

      it "parses Fathom output with Draw" $ do
        let output = "Probe: Draw DTZ: 0"
        probeLocalWith (\_ _ _ -> return output) "tool" "path" "fen"
          `shouldReturn` Right (SyzygyResult Draw 0 0)

      it "handles execution error" $ do
         probeLocalWith (\_ _ _ -> ioError (userError "Exec failed")) "tool" "path" "fen"
           `shouldReturn` Left "Execution error: user error (Exec failed)"

      it "handles parsing failure" $ do
         let output = "Some garbage output"
         probeLocalWith (\_ _ _ -> return output) "tool" "path" "fen"
           `shouldReturn` Left ("No Probe line found in output: " ++ output)
