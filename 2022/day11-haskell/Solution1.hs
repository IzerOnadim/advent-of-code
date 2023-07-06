import Data.Array.IArray
import Data.List (sort)
import Data.List.Split

data Op = Add Int | Mult Int | Square
data Monkey = M [Int] Op Int Int Int Int
type Monkeys = Array Int Monkey

main = do
  content <- readFile "input.txt"
  print $ monkeyBusiness (parse content)

parse :: String -> Monkeys
parse s = listArray (0, length m - 1) m
  where m = map parseMonkey (splitOn "\n\n" s)

parseMonkey :: String -> Monkey
parseMonkey s = M items op test t f 0
  where
    (_:l1:l2:l3:l4:l5:_) = lines s
    items = map read (splitOn ", " (last (splitOn ": " l1)))
    op = parseOp (last (splitOn "= " l2))
    test = read (last (splitOn "y " l3))
    t = read (last (splitOn "y " l4))
    f = read (last (splitOn "y " l5))

    parseOp :: String -> Op
    parseOp ('o':'l':'d':' ':'*':' ':'o':_) = Square
    parseOp ('o':'l':'d':' ':'*':' ':a)     = Mult (read a)
    parseOp ('o':'l':'d':' ':'+':' ':a)     = Add (read a)
    parseOp _ = error "Unrecognised operation"

monkeyBusiness :: Monkeys -> Int
monkeyBusiness = product . take 2 . reverse . sort . inspections . runRounds 20

inspections :: Monkeys -> [Int]
inspections = map (\(M _ _ _ _ _ i) -> i) . elems

runRounds :: Int -> Monkeys -> Monkeys
runRounds 0 = id
runRounds r = runRounds (r-1) . runRound 0

runRound :: Int -> Monkeys -> Monkeys
runRound i ms
  | inRange (bounds ms) i = runRound (i+1) (apply i (ms ! i) ms)
  | otherwise             = ms

apply :: Int -> Monkey -> Monkeys -> Monkeys
apply idx (M items op test t f i) ms = newMs//[(idx, M [] op test t f (i + length items))]
  where
    newMs = foldl (throw test t f) ms (map (inspect op) items)
    throw :: Int -> Int -> Int -> Monkeys -> Int -> Monkeys
    throw test t f ms item
      | mod item test == 0 = ms//[(t, catch mt item)]
      | otherwise          = ms//[(f, catch mf item)]
      where
        mt = ms ! t
        mf = ms ! f

    catch :: Monkey -> Int -> Monkey
    catch (M items op test t f i) new = M (items ++ [new]) op test t f i
 
    inspect :: Op -> Int -> Int
    inspect op item = div (exec op item) 3

    exec :: Op -> Int -> Int
    exec Square   = (^2)
    exec (Add  x) = (+x)
    exec (Mult x) = (*x)
