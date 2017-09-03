{-# Language MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances #-}
module Brainfuck (Op (..), Brainfuck, showBf, parse, parseFile, mandel) where

import Language

data Op = L | R | Read | Write | Inc | Dec | Loop [Op]
  deriving Eq
type Brainfuck = [Op]

instance Lang Brainfuck where

showBf = (=<<) $ \x -> case x of
  L -> "<"
  R -> ">"
  Read -> ","
  Write -> "."
  Inc -> "+"
  Dec -> "-"
  (Loop x) -> '[' : showBf x ++ "]"

parse :: String -> Brainfuck
parse = fst . parse'

parse' ('<':xs) | (o, xs') <- parse' xs = (L : o, xs')
parse' ('>':xs) | (o, xs') <- parse' xs = (R : o, xs')
parse' ('+':xs) | (o, xs') <- parse' xs = (Inc : o, xs')
parse' ('-':xs) | (o, xs') <- parse' xs = (Dec : o, xs')
parse' ('.':xs) | (o, xs') <- parse' xs = (Write : o, xs')
parse' (',':xs) | (o, xs') <- parse' xs = (Read : o, xs')
parse' ('[':xs) | (o, xs') <- parse' xs
                , (q, ys') <- parse' xs' = (Loop o : q, ys')
parse' (']':xs) = ([], xs)
parse' (_:xs)   = parse' xs
parse' []       = ([], [])

parseFile pth = parse <$> readFile pth

mandel = parseFile "mandelbroth.bf"
