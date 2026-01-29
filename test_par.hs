module Main where
import GHC.Conc (par, pseq)

main :: IO ()
main = do
  let x = sum [1..1000000]
  let y = sum [1..1000000]
  let z = x `par` (y `pseq` (x + y))
  print z
