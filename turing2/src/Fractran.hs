-- Formally proven turing complete
module Fractran where

import Data.Ratio
import Data.List
import qualified Collatz as C

type Program = [Rational]


interpret :: Program -> Integer -> Integer
interpret p n
  | Just q <- find (\q -> (n*numerator q `mod` denominator q) == 0) p
  = interpret p $ n*numerator q `div` denominator q
  | otherwise = n

tests :: [Program]
tests =
  [ [3%11, 847%45, 143%6, 7%3, 10%91, 3%7, 36%325, 1%2, 36%5] -- Prime sieve
  ]

toCollatz :: Program -> C.Program
toCollatz fractions = [
    case find (\f -> i `mod` denominator f == 0) fractions of
        Nothing     -> (m, i, 0)
        Just f      -> (floor (f*fromIntegral m), floor (f*fromIntegral i), 1)
    | i <- [0..m-1] ]
  where
    m = foldl' lcm 1 $ map denominator fractions
