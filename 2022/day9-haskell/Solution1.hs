import Data.List (nub)

type Pos = (Int, Int)
type State = (Pos, Pos)
data Motion = U Int | D Int | L Int | R Int

main = do
  content <- readFile "input.txt"
  print $ numVisited (parse content)

parse :: String -> [Motion]
parse = map parseMotion . lines

parseMotion :: String -> Motion
parseMotion ('U':' ':x) = U (read x)
parseMotion ('D':' ':x) = D (read x)
parseMotion ('L':' ':x) = L (read x)
parseMotion ('R':' ':x) = R (read x)
parseMotion _ = error "Unrecognised input"

numVisited :: [Motion] -> Int
numVisited = length . visited ((0, 0), (0, 0)) []

visited :: State -> [Pos] -> [Motion] -> [Pos]
visited _ v []     = nub v
visited s v (m:ms) = visited newS (moreV ++ v) ms
  where (newS, moreV) = apply s m

apply :: State -> Motion -> (State, [Pos])
apply s (U x) = move up x (s, [])
apply s (D x) = move down x (s, [])
apply s (L x) = move left x (s, [])
apply s (R x) = move right x (s, [])

up    (x, y) = (x - 1, y)
down  (x, y) = (x + 1, y)
left  (x, y) = (x, y - 1)
right (x, y) = (x, y + 1)

move :: (Pos -> Pos) -> Int -> (State, [Pos]) -> (State, [Pos])
move _    0     r      = r
move next steps (s, v) = move next (steps - 1) (newS, (newT:v))
  where newS@(_, newT) = moveOnce s next

moveOnce :: State -> (Pos -> Pos) -> State
moveOnce (h, t) next = (newH, newT)
  where
    newH = next h
    newT = follow newH t

follow :: Pos -> Pos -> Pos
follow (hx, hy) (tx, ty)
  -- Diagonal cases
  | (hx - tx > 1 && hy - ty > 0) || (hx - tx > 0 && hy - ty > 1) = (tx + 1, ty + 1)
  | (hx - tx > 1 && ty - hy > 0) || (hx - tx > 0 && ty - hy > 1) = (tx + 1, ty - 1)
  | (tx - hx > 1 && hy - ty > 0) || (tx - hx > 0 && hy - ty > 1) = (tx - 1, ty + 1)
  | (tx - hx > 1 && ty - hy > 0) || (tx - hx > 0 && ty - hy > 1) = (tx - 1, ty - 1)
  -- Horizontal cases
  | hx - tx > 1 = (tx + 1, ty)
  | tx - hx > 1 = (tx - 1, ty)
  -- Vertical cases
  | hy - ty > 1 = (tx, ty + 1)
  | ty - hy > 1 = (tx, ty - 1)
  -- No movement
  | otherwise   = (tx, ty)
 
