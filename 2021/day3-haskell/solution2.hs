import Data.List.Split

main = do
  contents <- readFile "input.txt"
  print $ getRating (parseToList contents)

parseToList :: String -> [String]
parseToList = filter (not . null) . splitOn "\n"

getRating :: [String] -> Int
getRating bStrs = getRating' 0 '1' bStrs * getRating' 0 '0' bStrs

getRating' :: Int -> Char -> [String] -> Int
getRating' _ _ []       = error "No bit string matching predicates." 
getRating' _ _ [b]      = binToDec b  
getRating' idx ch bStrs = getRating' (idx + 1) ch (filter criterion bStrs)
  where
    (_, total) = foldl countBin (idx, 0) bStrs
    startsWithCh = \bs -> (bs !! idx) == ch
    criterion
      | total >= 0 = startsWithCh
      | otherwise  = not . startsWithCh

binToDec :: String -> Int
binToDec bStr = binToDec' (length bStr - 1) bStr

binToDec' :: Int -> String -> Int
binToDec' _ []         = 0
binToDec' shift (b:bs) = dec + binToDec' (shift - 1) bs
  where
    dec = (2 ^ shift) * read [b]

countBin :: (Int, Int) -> String -> (Int, Int)
countBin (i, t) str 
  | c == '1'  = (i, t + 1)
  | c == '0'  = (i, t - 1)
  | otherwise = error "Unrecognised character in binary string." 
  where
    c = str !! i

