import Data.Char (isUpper, ord)
import Data.List (intersect)

main = do
  content <- readFile "input.txt"
  print $ sumPriority (lines content)

sumPriority :: [String] -> Int
sumPriority (x:y:z:xs) = groupPriority x y z + sumPriority xs
sumPriority []         = 0
sumPriority _          = error "These elves don't fit into groups of three!"

groupPriority :: String -> String -> String -> Int
groupPriority x y z = calcPriority (head (intersect x (intersect y z)))

calcPriority :: Char -> Int
calcPriority c
  | isUpper c = ord c - ord 'A' + 27
  | otherwise = ord c - ord 'a' + 1
