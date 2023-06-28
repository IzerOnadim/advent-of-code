import Data.List.Split

main = do  
   contents <- readFile "input.txt"
   print $ countIncreases (parseToList contents)

parseToList :: String -> [Int]
parseToList = map read . filter (not . null) . splitOn "\n"

countIncreases :: [Int] -> Int
countIncreases = countIncreases' maxBound

countIncreases' :: Int -> [Int] -> Int
countIncreases' prev (x1:x2:x3:xs) = inc + countIncreases' sum (x2:x3:xs)
  where
    sum = x1 + x2 + x3
    inc = if sum > prev then 1 else 0
countIncreases' _ _ = 0

