{-# Language MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances #-}
module SmallFuck (Op (..), SmallFuck) where

import Language
import qualified MuFuck as M
import qualified Brainfuck as B


data Op        = L | R | Flip | Loop [Op]
type SmallFuck = [Op]

instance Lang SmallFuck where

instance Trans M.MuFuck SmallFuck where
  trans = (=<<) $ \x -> case x of
            M.R         -> [R, Flip]
            M.L         -> [L]
            (M.Loop xs) -> [Loop $ trans xs]

instance Trans SmallFuck M.MuFuck where
  trans (R:Flip:xs) = M.R : trans xs
  trans (R:xs)      = M.R : M.L : M.R : trans xs
  trans (L:xs)      = M.L : trans xs
  trans (Flip:xs)   = M.L : M.R : trans xs
  trans (Loop o:xs) = M.Loop (trans o) : trans xs
