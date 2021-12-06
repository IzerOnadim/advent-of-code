import Data.List.Split

main = do  
   contents <- readFile "input.txt"
   print $ numFishAfterDays (parseToList contents) 80 -- 80 days.

parseToList :: String -> [Int]
parseToList = map read . filter (not . null) . splitOn ","

numFishAfterDays :: [Int] -> Int -> Int  
numFishAfterDays xs d = length ((iterate singleDay xs) !! d)

singleDay :: [Int] -> [Int]
singleDay [] = []
singleDay (x:xs)
  | x > 0 = x - 1 : singleDay xs
  | x == 0 = 8 : 6 : singleDay xs
  | otherwise = error "Negative numbers not allowed."
