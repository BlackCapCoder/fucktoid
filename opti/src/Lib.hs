{-# LANGUAGE MultiWayIf #-}
module Lib where

data Op = NOP
        | Skip Op -- Skip next unless 0
        | Flip
        | Move Int
        | Halt
        deriving (Show, Eq)

type Program = [Op]


parse :: String -> Program
parse str | (a,b) <- span (=='#') $ reverse str
          = repeatedly simplify
          . f $ a ++ reverse b
  where f []  = []
        f str | (a,b) <- parseOne str = a ++ f b

parseOne :: String -> (Program, String)
parseOne (x:xs) =
  case x of '<' -> ([Move (-1)],      xs)
            '>' -> ([Move   1, Flip], xs)
            '@' -> ([Halt], xs)
            '#' | (o:os, s) <- parseOne xs -> (Skip o : os, s)
            _   -> ([], xs)

simplify :: Program -> Program
simplify prg =
  case prg of
    (Move a:Move b:xs) | a+b == 0   -> simplify xs
                       | otherwise -> simplify $ Move (a+b) : xs
    (Flip:Flip:xs)    -> simplify xs
    (Skip(Skip x):xs) -> simplify $ x:xs
    (x:xs)         -> x : simplify xs
    x              -> x

repeatedly :: (Program -> Program) -> Program -> Program
repeatedly f p | p' <- f p, p /= p' = repeatedly f p' | 1<2 = p

