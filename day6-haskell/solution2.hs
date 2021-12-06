import Data.List.Split

main = do  
   contents <- readFile "input.txt"
   print $ numFishAfterDays (parseToList contents) 256 -- 256 days.

parseToList :: String -> [Int]
parseToList = map read . filter (not . null) . splitOn ","

numFishAfterDays :: [Int] -> Int -> Int  
numFishAfterDays xs d = length ((iterate singleDay xs) !! d)

singleDay :: [Int] -> [Int]
singleDay [] = []
singleDay (0:xs) = 8 : 6 : singleDay xs
singleDay (x:xs) = x - 1 : singleDay xs

