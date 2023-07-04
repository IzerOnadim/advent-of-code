import Data.Array.IArray
import Data.Char (digitToInt)

type Grid = Array (Int, Int) Int

main = do
  content <- readFile "input.txt"
  print $ scenicScore (parse content)

parse :: String -> Grid
parse inp = listArray ((0, 0), (r, c)) (map digitToInt (concat ls))
  where
    r = length ls - 1
    c = length l - 1
    ls@(l:_) = lines inp

scenicScore :: Grid -> Int
scenicScore a = maximum $ map (score a) (indices a)

score :: Grid -> (Int, Int) -> Int
score a i = product $ map (visibility a i (a!i)) [above, below, left, right]

above (r, c) = (r - 1, c)
below (r, c) = (r + 1, c)
left  (r, c) = (r, c - 1)
right (r, c) = (r, c + 1)

visibility :: Grid -> (Int, Int) -> Int -> ((Int, Int) -> (Int, Int)) -> Int
visibility a i mx next
  | not $ inRange (bounds a) n = 0
  | (a ! n) >= mx              = 1
  | otherwise                  = 1 + visibility a n mx next
  where n = next i
