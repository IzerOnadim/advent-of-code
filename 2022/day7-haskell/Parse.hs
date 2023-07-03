module Parse (parse, File (..)) where

data File = D String Int [File] | F String Int
type Path = [String]
type State = (File, Path)

parse :: String -> File
parse = populateSizes . parseLines (D "/" 0 [], []) . lines

populateSizes :: File -> File
populateSizes = fst . sizes
  where
    sizes :: File -> (File, Int)
    sizes f@(F _ sz)  = (f, sz)
    sizes (D nm _ cn) = (D nm sz newCns, sz)
      where
        sz = sum szs
        (newCns, szs) = unzip (map sizes cn)

parseLines :: State -> [String] -> File
parseLines s [] = fst s
parseLines s (line@(x:xs):rest)
  | x == '$'  = parseLines (parseCommand s (tail xs)) rest
  | x == 'd'  = parseLines (parseDirectory s line) rest
  | otherwise = parseLines (parseFile s line) rest

parseCommand :: State -> String -> State
parseCommand (d, p) ('c':'d':' ':'.':'.':_) = (d, init p)
parseCommand (d, _) ('c':'d':' ':'/':_)     = (d, [])
parseCommand s      ('c':'d':' ':nm)        = cd s nm
parseCommand s      ('l':'s':_)             = s
parseCommand s cmd                          = error ("Unrecognised command" ++ cmd)

cd :: State -> String -> State
cd (d, p) nm = (d, p ++ [nm])

parseDirectory :: State -> String -> State
parseDirectory s line = addFile s (D nm 0 [])
  where (_:nm:_) = words line

parseFile :: State -> String -> State
parseFile s line = addFile s (F nm (read sz))
  where (sz:nm:_) = words line

getName :: File -> String
getName (F nm _)   = nm
getName (D nm _ _) = nm 

addFile :: State -> File -> State
addFile (D nm sz cn, []) f
  | containsFile cn (getName f) = error "File with this name already exists"
  | otherwise                   = (D nm sz (f:cn), [])
addFile (d@(D nm sz cn), path@(p:ps)) f = (D nm sz newCn, path)
  where
    newCn = (newDir:rmCn)
    (oldDir, rmCn) = rmDir cn p
    (newDir, _) = addFile (oldDir, ps) f
addFile _ _ = error "Cannot add a file into another file"

rmDir :: [File] -> String -> (File, [File])
rmDir [] nm                         = error "No such Directory"
rmDir (d@(D n _ _):fs) nm | n == nm = (d, fs)
rmDir (f:fs) nm                     = (d, f:newFs)
  where (d, newFs) = rmDir fs nm

dirContains :: File -> String -> Bool
dirContains (D _ _ ds) nm = containsFile ds nm
dirContains _ _           = False

containsFile :: [File] -> String -> Bool
containsFile ((F n _):_) nm   | n == nm = True
containsFile ((D n _ _):_) nm | n == nm = True
containsFile [] _                       = False
containsFile (_:ds) nm                  = containsFile ds nm

