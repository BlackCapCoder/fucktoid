-- Formally proven turing complete
module Tag where

import qualified Data.Map as M
import Control.Arrow


type Rules  a = M.Map (Symbol a) [Symbol a]
data Symbol a = Halt | Symb a
              deriving (Show, Eq, Ord)
type Tag a = (Int, Rules a, [Symbol a])

interpret :: (Ord a) => Int -> Rules a -> [Symbol a] -> [Symbol a]
interpret m p xs
  | Halt <- x = xs
  | Just r <- M.lookup x p
  = interpret m p $ rst ++ r
 where (x:_, rst) = splitAt m xs

interpret' :: (Ord a) => Tag a -> [Symbol a]
interpret' (m, r, s) = interpret m r s

----------

tests :: [Tag Char]
tests =
  [ tag 'H' 2
      [ '1' --> "3321H"
      , '2' --> "331"
      , '3' --> "33" ]
      "221"
  ]

rules :: (Eq a, Ord a) => a -> [(a, [a])] -> Rules a
rules h = M.fromList . map (Symb *** map (\q -> if q==h then Halt else Symb q))

(-->) = (,)

tag h m l p = (m, rules h l, Symb<$>p)
