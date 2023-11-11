import Data.List
import Data.List.Split

main = do
  contents <- readFile "input.txt"
  print $ highestThree (parse contents)

parse :: String -> [[Int]]
parse = map parseToList . filter (not . null) . splitOn "\n\n"

parseToList :: String -> [Int]
parseToList = map read . filter (not . null) . lines

highestThree :: [[Int]] -> Int
highestThree = sum . take 3 . reverse . sort . map sum


