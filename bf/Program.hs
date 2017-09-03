{-# LANGUAGE MultiWayIf #-}

import qualified Lex as L
import Data.List

data Inst = Ptr Int
          | Add Int
          | Loop [Inst]
          | Out
          | Inp
          deriving (Eq, Show)

fromLex (L.LOp L.Prev) = Ptr (-1)
fromLex (L.LOp L.Next) = Ptr 1
fromLex (L.LOp L.Dec)  = Add (-1)
fromLex (L.LOp L.Inc)  = Add 1
fromLex (L.Loop xs)    = Loop $ fromLex <$> xs
fromLex (L.LOp L.Out)  = Out
fromLex (L.LOp L.Inp)  = Inp

type Program = [Inst]

parse :: String -> Program
parse = map fromLex
      . filter (\x -> case x of (L.LOp L.NOP) -> False; _ -> True)
      . L.lex'

---- Optimization ----
unite [] = []
unite (Add a:Add b:xs) = unite $ Add (a+b) : xs
unite (Ptr a:Ptr b:xs) = unite $ Ptr (a+b) : xs
unite (Loop a:xs) = (Loop $ unite a) : unite xs
unite (x:xs) = x : unite xs

removeNOPs = filter $ not . flip elem [Add 0, Ptr 0]

simplify x | y <- unite $ removeNOPs x
           = if | x == y    -> x
                | otherwise -> simplify y

type Seq = (Int, Program)
toSeq (Ptr n:xs) | (a,b) <- break (\x -> case x of Ptr _ -> True; _ -> False) xs
                 = (n,a) : toSeq b
toSeq [] = []
toSeq x  = toSeq $ Ptr 0 : x

isReformattable (Loop _) = False
isReformattable x        = x `notElem` [Inp, Out]


