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
    putStrLn "Wait! In the PR they added `getXRayAttacker`. Did they?"
    putStrLn "Let's use git log to see where `getXRayAttacker` was added."
