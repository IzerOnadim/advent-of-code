module ParseInput where

import Data.List.Split
import Types

parse :: String -> [Pattern]
parse = map parseToPair . parseToList "\n"

parseToPair :: String -> Pattern
parseToPair s = (parseToList " " s1, parseToList " " s2)
  where
    (s1:s2:_) = parseToList " | " s

parseToList :: String -> String -> [String]
parseToList d = filter (not . null) . splitOn d
