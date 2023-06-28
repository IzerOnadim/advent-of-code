import Data.List.Split

main = do  
   contents <- readFile "input.txt"
   print $ numFishAfterDays (parseToList contents) 256 -- 256 days.

parseToList :: String -> [Int]
parseToList = map read . filter (not . null) . splitOn ","

numFishAfterDays :: [Int] -> Int -> Int
numFishAfterDays xs d = length xs''
  where
    (weeks, days) = (d `div` 7, d `mod` 7)
    xs'  = (iterate singleWeek xs) !! weeks
    xs'' = (iterate singleDay xs') !! days

singleWeek :: [Int] -> [Int]
singleWeek [] = []
singleWeek (8:xs) = 1 : singleWeek xs
singleWeek (7:xs) = 0 : singleWeek xs
singleWeek (x:xs) = x : (x + 2) : singleWeek xs

singleDay :: [Int] -> [Int]
singleDay [] = []
singleDay (0:xs) = 8 : 6 : singleDay xs
singleDay (x:xs) = x - 1 : singleDay xs

