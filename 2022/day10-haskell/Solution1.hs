type Reg = Int
type History = [Reg]
data Instr = Noop | Add Int

main = do
  content <- readFile "input.txt"
  print $ signalStrength (parse content)

parse :: String -> [Instr]
parse = map parseInstr . lines
  where
    parseInstr :: String -> Instr
    parseInstr ('a':'d':'d':'x':' ':v) = Add (read v)
    parseInstr ('n':'o':'o':'p':_)     = Noop
    parseInstr _                       = error "Unrecognised Instruction"

signalStrength :: [Instr] -> Int
signalStrength = strength . history 1 []

history :: Reg -> History -> [Instr] -> History
history x h []             = x:h
history x h (Add v : rest) = history (x + v) (x:x:h) rest
history x h (Noop : rest)  = history x (x:h) rest

strength :: History -> Int
strength = sum . take40th 20 . reverse

take40th :: Int -> History -> History
take40th i h
  | i <= length h = (i * h!!(i-1) : take40th (i+40) h)
  | otherwise     = []
