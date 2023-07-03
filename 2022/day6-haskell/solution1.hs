markerLength :: Int
markerLength = 4

main = do
  content <- readFile "input.txt"
  print $ findMarker content

findMarker :: String -> Int
findMarker = findMarkerAux markerLength
  where
    findMarkerAux :: Int -> String -> Int
    findMarkerAux pos (a:b:c:d:rest)
      | allDifferent a b c d = pos
      | otherwise            = findMarkerAux (pos + 1) (b:c:d:rest)
    findMarkerAux _ _        = error "No marker found"

    allDifferent :: Char -> Char -> Char -> Char -> Bool
    allDifferent a b c d = (a /= b) && (a /= c) && (a /= d) &&
                           (b /= c) && (b /= d) && (c /= d)
