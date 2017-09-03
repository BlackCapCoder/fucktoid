module Lex (Op(..), Lex(..), lex') where
import Prelude hiding (Left, Right, lex)

data Op = Next | Prev | Inc  | Dec
        | Out  | Inp  | Left | Right
        | NOP
        deriving (Eq)

opTable = [ '>' ~> Next
          , '<' ~> Prev
          , '+' ~> Inc
          , '-' ~> Dec
          , '.' ~> Out
          , ',' ~> Inp
          , '[' ~> Left
          , ']' ~> Right ]
  where (~>) = (,)

readOp x | (Just o) <- lookup x opTable = o
         | otherwise                    = NOP

toOps = map readOp

instance Show Op where
  show x | (Just c) <- lookup x $ map (\(a,b)->(b,a)) opTable = [c]
         | otherwise = " "



data Lex = LOp Op | Loop [Lex]

lex :: [Op] -> ([Lex], [Op])
lex (Left:xs)  | (a,b) <- lex xs
               , (c,d) <- lex b  = (Loop a : c, d)
lex (Right:xs) = ([], xs)
lex (x:xs)     | (a,b) <- lex xs = (LOp x : a, b)
lex []         = ([], [])

instance Show Lex where
  show (LOp  x) = show x
  show (Loop x) = '[' : (x >>= show) ++ "]"

lex' = fst . lex . toOps

