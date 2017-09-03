-- Proven turing complete by reduction from Fractran
module Collatz where

import qualified Brainfuck as B
import Math.NumberTheory.Primes
import Data.List
import Data.Digits

type Collatz = (Integer, Integer, Integer)
type Program = [Collatz]


-- Note that this is never using more than 3 memory cells, proving
-- brainfuck turing complete with only 3 bytes of memory.
toBrainfuck :: [(Integer,Integer,Integer)] -> B.Program
toBrainfuck l = B.parse $ ">>+[-" ++ foldr branch "++<<[->+<]" l ++ ">[-<+>]>]"
  where branch (a,b,c) s = concat [
            "[-<", a*="+", ">]", c*="+", '<' : b*="+",
            "<[->", b*="-", '[' : a*="-", ">+<]>", c*="-",
            s, "]"
          ]
        n*=l = [1..n]>>l


ps = filter ((==10).length.fst)
   . map (\((x,y):xs) -> (take 10 (x:map fst xs), y))
   . groupBy (\a b -> snd a == snd b)
   . sortOn snd
   . map (\x -> (x, sum $ digits 10 x))
   $ takeWhile (<(10^8)) primes
