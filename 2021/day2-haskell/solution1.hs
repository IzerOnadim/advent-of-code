import Data.List.Split

data Command = Forward Int | Up Int | Down Int

main = do  
   contents <- readFile "input.txt"
   print $ calcPos (parseToList contents)

strToCom :: String -> Command
strToCom ('f':'o':'r':'w':'a':'r':'d':' ':x) = Forward (read x)
strToCom ('u':'p':x)                         = Up (read x)
strToCom ('d':'o':'w':'n':' ':x)             = Down (read x)
strToCom _ = error "Command string not recognised."

parseToList :: String -> [Command]
parseToList = map strToCom . filter (not . null) . splitOn "\n"

calcPos :: [Command] -> Int
calcPos = calcPos' 0 0 -- call tail recursive version

calcPos' :: Int -> Int -> [Command] -> Int
calcPos' hr dp []                    = hr * dp
calcPos' hr dp (Forward step : coms) = calcPos' (hr + step) dp coms
calcPos' hr dp (Up step : coms)      = calcPos' hr (dp - step) coms
calcPos' hr dp (Down step : coms)    = calcPos' hr (dp + step) coms

