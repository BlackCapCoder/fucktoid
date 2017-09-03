module BF where

import Control.Lens
import Control.Monad
import Debug.Trace
import Data.List
import Optimize
import qualified Data.Map as M
import Data.Word (Word8)

data Op = Move Int
        | In
        | Put String
        | NOP
        | Out Loc
        | Add      (M.Map Loc Word8)
        | Set      (M.Map Loc Word8)
        | Mult Loc (M.Map Loc Word8)
        | Loop Loc Program
        | When Loc Program
        | DUMP
        deriving (Read, Show, Eq)

type Program = [Op]
type Loc     = Int


parse :: String -> Program
parse = fst . parse'
  where parse' []       = ([NOP], [])
        parse' (']':xs) = ([NOP], xs)
        parse' xs | Just (o, rst) <- parseOp xs = parse' rst & _1 %~ (o:)
                  | otherwise = parse' $ tail xs

        parseOp (x:xs)
          = case x of
              '+' -> Just (Add $ M.singleton 0 1,    xs)
              '-' -> Just (Add $ M.singleton 0 (-1), xs)
              '>' -> Just (Move 1,    xs)
              '<' -> Just (Move (-1), xs)
              ',' -> Just (In       , xs)
              '.' -> Just (Out 0    , xs)
              '[' | (x,xs) <- parse' xs -> Just (Loop 0 x, xs)
              _   -> Nothing

isPure (Out    _) = False
isPure (Put    _) = False
isPure (In      ) = False
isPure (Loop _ o) = all isPure o
isPure _          = True
