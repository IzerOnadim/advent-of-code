import Data.List.Split

main = do  
   contents <- readFile "input.txt"
   print $ minFuel (parseToList contents)

parseToList :: String -> [Int]
parseToList = map read . filter (not . null) . splitOn ","

minFuel :: [Int] -> Int
minFuel subs = minFuelRange subs mn mx 
  where
    mx = foldl1 max subs
    mn = foldl1 min subs

minFuelRange :: [Int] -> Int -> Int -> Int
minFuelRange subs mn mx = foldl1 min [fuelPos subs pos | pos <- [mn..mx] ]

fuelPos :: [Int] -> Int -> Int
fuelPos []     _ = 0
fuelPos (x:xs) p = fuelCost + fuelPos xs p
  where
    diff     = abs (p - x)
    fuelCost = div (diff * (diff + 1)) 2

