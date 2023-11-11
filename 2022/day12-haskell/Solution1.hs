import Data.Array.IArray

type Pos = (Int, Int)
type Grid = Array Pos Int
data State = S Grid Pos Pos

main = do
  content <- readFile "test.txt"
  print $ fewestSteps (parse content)

parse :: String -> State
parse s =  
  where
    lns@(l:_) = lines s
    n = length lns
    m = length l

makeState :: Array Pos Char -> State
makeState =  

charGrid :: String -> Array Pos Char
charGrid = listArray ((0, 0), (n, m))

inline :: String -> (Int, Int, String)
inline = concat . lines

fewestSteps :: State -> Int
