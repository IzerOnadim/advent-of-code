import Data.List.Split

main = do  
   contents <- readFile "input.txt"
   print $ getPowerCons (parseToList contents)

parseToList :: String -> [String]
parseToList = filter (not . null) . splitOn "\n"

getPowerCons :: [String] -> Integer
getPowerCons []     = 0
getPowerCons (x:xs) = getGammaEpsilon (x:xs) (replicate n 0)
  where
    n = length x -- Assume all binary strings are same length.  

getGammaEpsilon :: [String] -> [Int] -> Integer
getGammaEpsilon [] acc     = getProduct 0 0 acc (length acc - 1)
getGammaEpsilon (x:xs) acc = getGammaEpsilon xs (binAdd x acc)

getProduct :: Integer -> Integer -> [Int] -> Int -> Integer
getProduct g e []     _ = g * e
getProduct g e (x:xs) n = getProduct g' e' xs (n - 1) 
  where
    shift = 2 ^ n
    (g', e') = if x >= 0 then (g + shift, e) else (g, e + shift)

binAdd :: String -> [Int] -> [Int]
binAdd [] []             = [] 
binAdd ('0':xs) (ac:acs) = (ac - 1) : (binAdd xs acs)
binAdd ('1':xs) (ac:acs) = (ac + 1) : (binAdd xs acs) 
binAdd _ _ = error "Invalid char in binary string or strings of differing length."
