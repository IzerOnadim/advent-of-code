import Data.Array.IArray
import Data.Char (digitToInt)

type Grid = Array (Int, Int) Int

main = do
  content <- readFile "input.txt"
  print $ visibleTrees (parse content)

parse :: String -> Grid
parse inp = listArray ((0, 0), (r, c)) (map digitToInt (concat ls))
  where
    r = length ls - 1
    c = length l - 1
    ls@(l:_) = lines inp

visibleTrees :: Grid -> Int
visibleTrees a = countVisible $ applyAll ranges a v
  where
  (_, (r, c)) = bounds a
  rows = [range ((i, 0), (i, c)) | i <- [0..r]]
  cols = [range ((0, i), (r, i)) | i <- [0..c]]
  ranges = rows ++ map reverse rows ++ cols ++ map reverse cols
  v = visibilityMap (snd (bounds a))

countVisible :: Grid -> Int
countVisible = length . filter (==1) . elems

visibilityMap :: (Int, Int) -> Grid
visibilityMap (r, c) = listArray ((0, 0), (r, c)) (replicate ((r + 1) * (c + 1)) 0)

applyAll :: [[(Int, Int)]] -> Grid -> Grid -> Grid
applyAll []     _ v = v
applyAll (x:xs) a v = applyAll xs a (apply (-1) x a v)

apply :: Int -> [(Int, Int)] -> Grid -> Grid -> Grid
apply _ []     _ v = v
apply m (x:xs) a v
  | h > m     = apply h xs a v//[(x, 1)]
  | otherwise = apply m xs a v
   where h = a ! x
