{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

module Main where

import Criterion.Main
import qualified Chess.Board.San as San
import Chess.Types

main :: IO ()
main = do
    let src = B1
    let candidates_1 = [B1]
    let candidates_2 = [B1, D1]
    let candidates_3 = [B1, B5]
    let candidates_4 = [B1, D1, B5]

    defaultMain [
      bgroup "disambiguate" [
        bench "length_1" $ whnf (San.disambiguate src) candidates_1,
        bench "length_2_diff_file" $ whnf (San.disambiguate src) candidates_2,
        bench "length_2_same_file" $ whnf (San.disambiguate src) candidates_3,
        bench "length_3" $ whnf (San.disambiguate src) candidates_4
      ]
      ]
