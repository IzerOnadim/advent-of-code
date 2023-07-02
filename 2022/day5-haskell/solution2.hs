import Data.List.Split

type Stack = [Char]
type Instr = (Int, Int, Int)

main = do
  content <- readFile "input.txt"
  print $ topCrates (parse content)

parse :: String -> ([Stack], [Instr])
parse s = (parseStacks s1, parseInstrs s2)
  where (s1:s2:_) = splitOn "\n\n" s

parseStacks :: String -> [Stack]
parseStacks = fillStacks . reverse . map tokenise . init . lines

tokenise :: String -> [Char]
tokenise (x:y:z:xs) = (y : tokenise rest)
  where
    rest = if null xs then [] else tail xs
tokenise _ = []

fillStacks :: [[Char]] -> [Stack]
fillStacks = fillStacksAux []
  where
    fillStacksAux :: [Stack] -> [[Char]] -> [Stack]
    fillStacksAux s []     = s
    fillStacksAux s (x:xs) = fillStacksAux (addCrate x s []) xs

    addCrate :: [Char] -> [Stack] -> [Stack] -> [Stack]
    addCrate [] _ s          = s
    addCrate cs [] _         = map (\c -> [c]) cs
    addCrate (c:cs) (x:xs) s = addCrate cs xs (s ++ [stack])
      where
        stack = if c == ' ' then x else (c:x)

parseInstrs :: String -> [Instr]
parseInstrs = map parseInstr . lines
  where
    parseInstr :: String -> Instr
    parseInstr s = (read (w!!1), read (w!!3), read (w!!5))
      where w = words s

topCrates :: ([Stack], [Instr]) -> String
topCrates (s, [])     = tops s
topCrates (s, (i:is)) = topCrates ((apply s i), is)

apply :: [Stack] -> Instr -> [Stack]
apply s (num, from, to) = uncurry (add to) (takeN from num s)
  where
    takeN = takeNAux 1
    takeNAux :: Int -> Int -> Int -> [Stack] -> (Stack, [Stack])
    takeNAux _ _ _ [] = error "Index out of bounds"
    takeNAux i p n (x:xs)
      | i == p    = (take n x, (drop n x) : xs)  
      | otherwise = (s, (x : rest))
      where
        (s, rest) = takeNAux (i+1) p n xs
 
    add = addAux 1
    addAux :: Int -> Int -> Stack -> [Stack] -> [Stack]
    addAux i p s []
      | i == p    = [s]
      | otherwise = error "Index out of bounds"
    addAux i p s (x:xs)
      | i == p    = ((s ++ x) : xs)
      | otherwise = (x : addAux (i+1) p s xs)

tops :: [Stack] -> String
tops []             = []
tops ([] : rest)    = tops rest
tops ((x:_) : rest) = (x : tops rest)
