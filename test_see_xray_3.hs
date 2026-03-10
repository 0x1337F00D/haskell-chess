import Data.Word
import Data.Bits

newtype Square = Square { unSquare :: Int } deriving (Eq, Show)

bbFromSquare :: Square -> Word64
bbFromSquare (Square sq) = bit sq

rayInitOld :: Square -> Square -> Word64
rayInitOld a@(Square ai) b@(Square bi)
  | a == b = 0
  | abs df == abs dr || df == 0 || dr == 0 = go (fileA+dfSign) (rankA+drSign) 0
  | otherwise = 0
  where
    fileA = ai `mod` 8
    rankA = ai `div` 8
    fileB = bi `mod` 8
    rankB = bi `div` 8
    df = fileB - fileA
    dr = rankB - rankA
    dfSign = signum df
    drSign = signum dr

    go f r acc
      | f == fileB && r == rankB = acc
      | f < 0 || f > 7 || r < 0 || r > 7 = 0
      | otherwise =
          let acc' = acc .|. bbFromSquare (Square (r*8 + f))
          in go (f+dfSign) (r+drSign) acc'

main :: IO ()
main = do
    putStrLn "In SEE.hs, `sq` is the target square. `from` is the square of the piece that just captured."
    putStrLn "We want to find an attacker BEHIND `from`."
    putStrLn "So the ray should be from `sq` extending PAST `from`."
    putStrLn "Wait... Does `ray sq from` extend PAST `from` in the original code? Let me check."
    putStrLn $ "rayInitOld (sq=E1, from=E4): " ++ show (rayInitOld (Square 4) (Square 28))
    putStrLn "Bit 12 (E2), 20 (E3). Does it have E5 (36)?"
    putStrLn $ "testBit 36: " ++ show (testBit (rayInitOld (Square 4) (Square 28)) 36)
