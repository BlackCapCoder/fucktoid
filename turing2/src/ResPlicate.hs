-- https://esolangs.org/wiki/ResPlicate
-- ResPlicate is turing complete via tag
module ResPlicate where

import qualified Data.Map as M

type Program = [Int]
data Status  = Terminated | Cycle


interpret = interpret' M.empty

interpret' :: M.Map Program () -> Program -> Status
interpret' m k@(x:y:xs)
  | Just _ <- M.lookup k m = Cycle
  | (a,b)  <- splitAt x $ xs ++ repeat 0
  = interpret' (M.insert k () m)
  $ zipWith const (drop x xs) b ++ ([1..y]>>a)
interpret' _ _ = Terminated


tests :: [Program]
tests =
  [ [6,2,8,1,6,2,8,1] -- 12 period oscillator
  , [6,2,7,1,6,3,8,0] -- Grows indefinitely
  ]
