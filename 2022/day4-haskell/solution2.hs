import Data.List.Split

main = do
  content <- readFile "input.txt"
  print $ numContains (parse content)

parse :: String -> [((Int, Int), (Int, Int))]
parse = map (makePair . map (intPair . splitOn "-") . splitOn ",") . lines

makePair :: [a] -> (a, a)
makePair (x:y:xs) = (x, y)
makePair _        = error "Insufficient elements to make pair"

intPair :: [String] -> (Int, Int)
intPair (x:y:xs) = (read x, read y)
intPair _        = error "Insufficient elements to make pair"

numContains :: [((Int, Int), (Int, Int))] -> Int
numContains = sum . map contains

contains :: ((Int, Int), (Int, Int)) -> Int
contains ((a, b), (x, y)) = fromEnum ((a <= x && b >= x) || (x <= a && y >= a))
