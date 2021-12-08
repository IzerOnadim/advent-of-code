import ParseInput
import Types

main = do
  contents <- readFile "input.txt"
  print $ countDigits (parse contents)

countDigits :: [Pattern] -> Int
countDigits = sum . map countDigits'

countDigits' :: Pattern -> Int
countDigits' (_, digits) = length $ filter isUnique digits

isUnique :: String -> Bool
isUnique s = length s == 2 || length s == 3 || length s == 4 || length s == 7 
