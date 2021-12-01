import Data.List.Split

main = do  
   contents <- readFile "input.txt"
   print $ countIncreases (parseToList contents)

parseToList :: String -> [Int]
parseToList = map read . filter (not . null) . splitOn "\n"

countIncreases :: [Int] -> Int
countIncreases = countIncreases' maxBound

countIncreases' :: Int -> [Int] -> Int
countIncreases' _ []        = 0
countIncreases' prev (x:xs) = inc + countIncreases' x xs
  where
    inc = if x > prev then 1 else 0

