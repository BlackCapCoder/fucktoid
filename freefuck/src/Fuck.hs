{-# LANGUAGE DeriveFunctor #-}
module Fuck where

import Control.Monad.Free
import Control.Monad.State.Strict
import Data.Default


data Op next
  = Inc   next | Dec   next
  | MoveL next | MoveR next
  | Inp   next | Outp  next
  | Loop BrainFuck next
  deriving (Functor, Show)

type BrainFuck = Free Op ()

inc, dec, moveL, moveR, inp, outp :: BrainFuck
inc   = liftF $ Inc   ()
dec   = liftF $ Dec   ()
moveL = liftF $ MoveL ()
moveR = liftF $ MoveR ()
inp   = liftF $ Inp   ()
outp  = liftF $ Outp  ()

loop :: BrainFuck -> BrainFuck
loop p = liftF $ Loop p ()

-----------

pretty :: BrainFuck -> String
pretty p = case p of
  (Free (Inc    n)) -> '+' : pretty n
  (Free (Dec    n)) -> '-' : pretty n
  (Free (MoveL  n)) -> '<' : pretty n
  (Free (MoveR  n)) -> '>' : pretty n
  (Free (Inp    n)) -> ',' : pretty n
  (Free (Outp   n)) -> '.' : pretty n
  (Free (Loop p n)) -> '[' : pretty p ++ ']' : pretty n
  (Pure r)          -> ""

-----------

test = do
  inc
  loop $ do
    moveR
    inc
    moveL
    dec
