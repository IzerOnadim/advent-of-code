import Prelude hiding (lookup)
import Data.Map (Map, fromList, insert, insertWith, lookup, toList)
import Data.Maybe (fromJust)
import Data.List (sort)
import ParseInput
import Types

main = do
  contents <- readFile "input.txt"
  print $ sumDigits (parse contents)

-- Map from segments to associated digits.
digitMap :: Map String Char
digitMap = fromList [("abcefg", '0'), ("cf", '1'), ("acdeg", '2'), ("acdfg", '3'), ("bcdf", '4'),
                     ("abdfg", '5'), ("abdefg", '6'), ("acf", '7'), ("abcdefg", '8'), ("abcdfg", '9')] 

segments :: [Char]
segments = ['a'..'g']

-- Map from input segment labels to all possible actual segments
segMap :: SegMap
segMap = fromList $ map (\ch -> (ch, segments)) segments

sumDigits :: [Pattern] -> Int
sumDigits = sum . map decodeDigits

decodeDigits :: Pattern -> Int
decodeDigits (patterns, digits) = read $ map decodeDigit (mapDigits sMap digits)
  where 
    sMap = genSegMap patterns

    decodeDigit :: String -> Char 
    decodeDigit s = fromJust $ lookup (sort s) digitMap 

mapDigits :: SegMap -> [String] -> [String]
mapDigits sMap strs = map (mapDigit sMap) strs
  where
    mapDigit :: SegMap -> String -> String
    mapDigit _    []     = []
    mapDigit sMap (x:xs) = fromJust (lookup x sMap) ++ mapDigit sMap xs 

genSegMap :: [String] -> SegMap
genSegMap strs = freq sMap strs -- frequency analysis 
  where
    sMap = foldl eliminate segMap strs -- analyse segment strings of unique lengths

freq :: SegMap -> [String] -> SegMap
freq sMap strs = foldl (freq' str) sMap freqChars
  where
    freqChars = [('b', 'd'), ('c', 'f'), ('e', 'g')] -- chars to perform frequency analysis on
    str = concat strs

freq' :: String -> SegMap -> (Char, Char) -> SegMap
freq' str sMap (less, more) = insert l [less] (insert m [more] sMap) 
  where
    (l, m)    = if c1 < c2 then (k1, k2) else (k2, k1)
    (c1, c2)  = (count k1 str, count k2 str) 
    val       = [less, more] -- Value in segment map 
    (k1:k2:_) = map fst (filter ((==val) . snd) (toList sMap))
  
    count :: Char -> String -> Int
    count ch = foldl (\a c -> if ch == c then a + 1 else a) 0 

eliminate :: SegMap -> String -> SegMap
eliminate sMap s@[c, f]       = combine sMap s "cf"   -- Segments for 1
eliminate sMap s@[a, c, f]    = combine sMap s "acf"  -- Segments for 7
eliminate sMap s@[b, c, d, f] = combine sMap s "bcdf" -- Segments for 4
eliminate sMap _              = sMap -- No further analysis based on number of segments

combine :: SegMap -> String -> String -> SegMap
combine mp keys val = foldl (\m ch -> insertWith merge ch val m) mp' keys
  where
    notKeys = filter (\ch -> not (elem ch keys)) segments
    notVal  = filter (\ch -> not (elem ch val)) segments
    mp' = foldl (\m ch -> insertWith merge ch notVal m) mp notKeys

    merge :: [Char] -> [Char] -> [Char]
    merge [] _ = []
    merge (x:xs) ys
      | elem x ys = x : merge xs ys
      | otherwise = merge xs ys

