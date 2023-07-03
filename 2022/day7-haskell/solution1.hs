import Parse (parse, File (..))

maxSize :: Int
maxSize = 100000

main = do
  content <- readFile "input.txt"
  print $ totalSize (parse content)

totalSize :: File -> Int
totalSize = sum . filterSize maxSize

filterSize :: Int -> File -> [Int]
filterSize m (F _ _) = []
filterSize m (D _ sz cn)
  | sz <= m   = (sz:rest)
  | otherwise = rest
  where rest = concat (map (filterSize m) cn)
