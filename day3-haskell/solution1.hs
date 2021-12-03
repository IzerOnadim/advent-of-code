import Data.List.Split

main = do
   contents <- readFile "input.txt"
   print $ getPowerCons (parseToList contents)

parseToList :: String -> [String]
parseToList = filter (not . null) . splitOn "\n"

getPowerCons :: [String] -> Integer
getPowerCons []       = 0
getPowerCons xs@(x:_) = getProduct 0 0 acc (length acc - 1)
  where
    n   = length x -- Assume all binary strings are same length.
    acc = foldl binAdd (replicate n 0) xs

getProduct :: Integer -> Integer -> [Int] -> Int -> Integer
getProduct g e []     _ = g * e
getProduct g e (x:xs) n = getProduct g' e' xs (n - 1)
  where
    shift = 2 ^ n
    (g', e') = if x >= 0 then (g + shift, e) else (g, e + shift)

binAdd :: [Int] -> String -> [Int]
binAdd [] []             = []
binAdd (ac:acs) ('0':xs) = (ac - 1) : (binAdd acs xs)
binAdd (ac:acs) ('1':xs) = (ac + 1) : (binAdd acs xs)
binAdd _ _ = error "Invalid char in binary string or strings of differing length."
