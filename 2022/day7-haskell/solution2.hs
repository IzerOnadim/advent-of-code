import Parse (parse, File (..))

totalSize :: Int
totalSize = 70000000

spaceNeeded :: Int
spaceNeeded = 30000000

main = do
  content <- readFile "input.txt"
  print $ minimalDelete (parse content)

minimalDelete :: File -> Int
minimalDelete (F _ _)      = error "Cannot delete a file" 
minimalDelete r@(D _ sz _) = smallestGreaterThan sz required r
  where required = spaceNeeded - (totalSize - sz)

smallestGreaterThan :: Int -> Int -> File -> Int
smallestGreaterThan curr req (F _ _) = curr  
smallestGreaterThan curr req (D _ sz cn)
  | req <= sz && sz <= curr = minimum $ map (smallestGreaterThan sz req) cn
  | otherwise               = minimum $ map (smallestGreaterThan curr req) cn
