markerLength :: Int
markerLength = 14

main = do
  content <- readFile "input.txt"
  print $ findMarker content

findMarker :: String -> Int
findMarker = findMarkerAux markerLength
  where
    findMarkerAux :: Int -> String -> Int
    findMarkerAux pos s
      | length s < markerLength            = error "No marker found"
      | allDifferent (take markerLength s) = pos
      | otherwise                          = findMarkerAux (pos + 1) (tail s)

    allDifferent :: String -> Bool
    allDifferent "" = True
    allDifferent (x:xs)
      | elem x xs = False
      | otherwise = allDifferent xs
