import Data.List.Split

main = do
  contents <- readFile "input.txt"
  print $ highestTotal (parse contents)

parse :: String -> [[Int]]
parse = map parseToList . filter (not . null) . splitOn "\n\n"

parseToList :: String -> [Int]
parseToList = map read . filter (not . null) . lines

highestTotal :: [[Int]] -> Int
highestTotal = maximum . map sum
