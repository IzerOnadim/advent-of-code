import Data.Char (isUpper, ord)
import Data.List (intersect)

main = do
  content <- readFile "input.txt"
  print $ sumPriority (lines content)

sumPriority :: [String] -> Int
sumPriority = sum . map priority

priority :: String -> Int
priority xs = calcPriority (head (intersect ys zs))
  where
    ys = take n xs
    zs = drop n xs
    n = div (length xs) 2

calcPriority :: Char -> Int
calcPriority c
  | isUpper c = ord c - ord 'A' + 27
  | otherwise = ord c - ord 'a' + 1
