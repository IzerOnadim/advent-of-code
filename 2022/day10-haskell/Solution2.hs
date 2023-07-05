import Data.List.Split (chunksOf)
import Data.Array.IArray

type Reg = Int
type History = [Reg]
type Screen = Array Int Char
data Instr = Noop | Add Int

main = do
  content <- readFile "input.txt"
  putStrLn $ drawScreen (parse content)

parse :: String -> [Instr]
parse = map parseInstr . lines
  where
    parseInstr :: String -> Instr
    parseInstr ('a':'d':'d':'x':' ':v) = Add (read v)
    parseInstr ('n':'o':'o':'p':_)     = Noop
    parseInstr _                       = error "Unrecognised Instruction"

drawScreen :: [Instr] -> String
drawScreen =  toString . renderScreen . reverse . history 1 []

toString :: Screen -> String
toString scr = concat $ map (++"\n") (chunksOf 40 (elems scr))

renderScreen :: History -> Screen
renderScreen = renderPixel 0 scr
  where scr = listArray (0, 239) (replicate 240 '.')

renderPixel :: Int -> Screen -> History -> Screen
renderPixel _ scr [] = scr
renderPixel i scr (x:xs)
  | (x - 1 <= p) && (p <= x + 1) = renderPixel (i+1) (scr//[(i, '#')]) xs
  | otherwise                    = renderPixel (i+1) scr xs
  where p = mod i 40

history :: Reg -> History -> [Instr] -> History
history x h []             = x:h
history x h (Add v : rest) = history (x + v) (x:x:h) rest
history x h (Noop : rest)  = history x (x:h) rest

