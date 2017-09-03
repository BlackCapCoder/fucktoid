{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Fucktoid (BF, Op (..), Fucktoid (..), (~>), printBF, parse) where
import Prelude hiding (Left, Right)

(~>) = (,)

type BF = [Op]
data Op = Next | Prev | Inc
        | Dec  | Inp  | Out
        | Loop [Op]   | NOP
        deriving (Eq)

instance Show Op where
  show x = case x of Next -> ">"
                     Prev -> "<"
                     Inc  -> "+"
                     Dec  -> "-"
                     Inp  -> ","
                     Out  -> "."
                     NOP  -> ""
                     Loop xs -> '[' : (xs >>= show) ++ "]"

class Fucktoid f where
  to    :: f  -> BF
  from  :: BF -> f
  hasIO :: f  -> Bool

  hasIO = any hasIO . to

instance Fucktoid Op where
  to    = pure
  from  = head
  hasIO = flip elem [Inp, Out]

instance Fucktoid BF where
  to   = id
  from = id

printBF :: Fucktoid a => a -> String
printBF = concatMap show . to


data Lex = LOp Op | Left | Right
lexTable = [ '>' ~> LOp Next
           , '<' ~> LOp Prev
           , '+' ~> LOp Inc
           , '-' ~> LOp Dec
           , '.' ~> LOp Out
           , ',' ~> LOp Inp
           , '[' ~> Left
           , ']' ~> Right ]

lexBF x | (Just o) <- lookup x lexTable = o
        | otherwise                     = LOp NOP

lexBF' :: [Lex] -> ([Op], [Lex])
lexBF' (Left:xs)  | (a,b) <- lexBF' xs
                  , (c,d) <- lexBF' b  = (Loop a : c, d)
lexBF' (Right:xs) = ([], xs)
lexBF' (LOp x:xs) | (a,b) <- lexBF' xs = (x : a, b)
lexBF' []         = ([], [])

parse = fst . lexBF' . map lexBF
