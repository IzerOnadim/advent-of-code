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
score (opp, you) = outcomeScore you + selectionScore opp you

outcomeScore :: Char -> Int
outcomeScore c
  | c == 'X'  = 0
  | c == 'Y'  = 3
  | c == 'Z'  = 6
  | otherwise = error "Unrecognised move"

selectionScore :: Char -> Char -> Int
selectionScore 'A' 'X' = 3
selectionScore 'A' 'Y' = 1
selectionScore 'A' 'Z' = 2
selectionScore 'B' 'X' = 1
selectionScore 'B' 'Y' = 2
selectionScore 'B' 'Z' = 3
selectionScore 'C' 'X' = 2
selectionScore 'C' 'Y' = 3
selectionScore 'C' 'Z' = 1

