import Prelude hiding (Left, Right)
import Data.BitString

data Op       = Left | Right | Loop [Op] deriving (Show)
type Program  = [Op]
type Tape     = ([Bool], Bool, [Bool])

step :: Op -> Tape -> Tape
step Left  ([]  , m, r) = ([],  False, m:r)
step Left  (l:ls, m, r) = (ls,      l, m:r)
step Right (l, m,   []) = (m:l,  True,  [])
step Right (l, m, r:rs) = (m:l, not r,  rs)
step _  t@  (_,False,_) = t
step        (Loop os) t = run t os

run :: Tape -> Program -> Tape
run = foldl (flip step)

newTape :: Tape
newTape = ([], False, [])

run' :: Program -> Tape
run' = run newTape

parse :: String -> Program
parse ('<':xs) = Left  : parse xs
parse ('>':xs) = Right : parse xs
parse ('?':xs) | (a,b) <- span (=='?') xs
               , (c,d) <- splitAt (length a + 1) $ parse b
               = Loop c : d
parse (_:xs)   = parse xs
parse []       = []

tapeToArr :: Tape -> [Bool]
tapeToArr (a,b,c) = reverse a++b:c

interpret = tapeToArr . run' . parse

