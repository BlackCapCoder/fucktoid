{-# Language MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances #-}
module BfVars (Op (..), BfVars, show') where

import Language
import qualified Brainfuck as B
import Control.Monad.State
import Debug.Trace


data Op = L | R | Read | Write | Inc | Dec | Loop [Op] | Var Int deriving Show
type BfVars = [Op]

instance Lang BfVars where

instance Trans BfVars B.Brainfuck where
  trans x = trans' $ preprocessVars x

trans' = (=<<) $ \x -> case x of
  L -> [B.L]
  R -> [B.R]
  Read -> [B.Read]
  Write -> [B.Write]
  Inc -> [B.Inc]
  Dec -> [B.Dec]
  (Loop x) -> [B.Loop $ trans' x]
  (Var x) -> if x > 0 then replicate x B.R
                      else replicate (abs x) B.L

show' = (=<<) $ \x -> case x of
    L -> "<"
    R -> ">"
    Read -> ","
    Write -> "."
    Inc -> "+"
    Dec -> "-"
    (Loop x) -> '[' : show' x ++ "]"
    (Var x)  -> pure $ ['A'..'Z'] !! x


preprocessVars :: BfVars -> BfVars
preprocessVars = reverse . fst . foldl process ([], 0)
  where process (out, ptr) x = case x of
          L -> (x:out, ptr-1)
          R -> (x:out, ptr+1)
          (Loop o) | (o', ptr') <- foldl process ([], ptr) o -> (Loop (reverse o') : out, ptr')
          (Var n) -> (Var (n-ptr) : out, n)
          _ -> (x:out, ptr)
