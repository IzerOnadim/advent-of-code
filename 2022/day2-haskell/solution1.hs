import Data.Char (ord)

main = do
  contents <- readFile "input.txt"
  print $ totalScore (parse contents)

parse :: String -> [(Char, Char)]
parse = map (makePair . words) . lines

makePair :: [String] -> (Char, Char)
makePair (x:y:z:_) = error "Too many elements in list to make pair" 
makePair ([x]:[y]:_) = (x, y)  
makePair _ = error "Not enough elements in list to make pair"

totalScore :: [(Char, Char)] -> Int
totalScore = sum . map score

score :: (Char, Char) -> Int
score (opp, you) = outcomeScore opp you + selectionScore you

outcomeScore :: Char -> Char -> Int
outcomeScore 'A' 'X' = 3
outcomeScore 'A' 'Y' = 6
outcomeScore 'A' 'Z' = 0
outcomeScore 'B' 'X' = 0
outcomeScore 'B' 'Y' = 3
outcomeScore 'B' 'Z' = 6
outcomeScore 'C' 'X' = 6
outcomeScore 'C' 'Y' = 0
outcomeScore 'C' 'Z' = 3

selectionScore :: Char -> Int
selectionScore c
  | c == 'X'  = 1 
  | c == 'Y'  = 2 
  | c == 'Z'  = 3
  | otherwise = error "Unrecognised move"  
