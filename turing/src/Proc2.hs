{-# Language MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances #-}
module Proc2 where

import Prelude hiding (and, or)
import Language
import qualified Brainfuck as B
import qualified BfVars as V
import Control.Monad.State

data Doc = Doc
  { varCnt :: Int
  , doc    :: V.BfVars
  }

type Op = State Doc ()


run o | (_, s) <- runState o $ Doc 0 [] = doc s

var, inc, dec, clear :: V.Op -> Op

var v = modify $ \x -> x{doc = doc x ++ [v]}
low = var

clear v = modify $ \x -> x{doc = doc x ++ [v, V.Loop [V.Dec]]}

loop :: Op -> Op
loop o = modify $ \x ->
  let (_, s) = runState o $ Doc (varCnt x) []
  in  x { varCnt = varCnt s, doc = doc x ++ [V.Loop $ doc s] }

inc v = modify $ \x -> x{doc = doc x ++ [v, V.Inc]}
dec v = modify $ \x -> x{doc = doc x ++ [v, V.Dec]}

clears  = mapM_ clear
incs = mapM_ inc

finc  v = for v . inc
fincs v = for v . incs

for v o = do
  var v
  loop $ o >> dec v

lclear v o = var v >> loop (o >> clear v)

popVar = do
  d <- gets varCnt
  modify $ \x -> x { varCnt = varCnt x + 1 }
  return $ V.Var d

popVars n = sequence $ replicate n popVar

vars n f = popVars n >>= f >> freeVars n

vars' n f = popVars n >>= (\vs -> clears vs >> f vs) >> freeVars n

freeVars :: Int -> Op
freeVars n = modify $ \x -> x { varCnt = varCnt x - n }


-- x = y
assign x y = vars' 1 $ \[tmp] -> do
  clear x
  fincs y [x, tmp]
  finc tmp y

-- x = x + y
add x y = vars' 1 $ \[tmp] -> do
  fincs y [x, tmp]
  finc tmp y

-- x = x - y
sub x y = vars' 1 $ \[tmp] -> do
  for y $ do
    dec x
    inc tmp
  finc tmp y

-- x = x * y
mult x y = vars' 2 $ \[tmp0,tmp1] -> do
  finc x tmp1
  for tmp1 $ do
    fincs y [x, tmp0]
    finc tmp0 y

-- x = x * x
pow2 x = vars' 3 $ \[tmp0, tmp1, tmp2] -> do
  fincs x [tmp2, tmp1]
  for tmp1 $ do
    fincs tmp2 [x, tmp0]
    finc tmp0 tmp2

-- x = x / y
div x y = vars' 4 $ \[tmp0,tmp1,tmp2,tmp3] -> do
  finc x tmp0
  for tmp0 $ do
    fincs y [tmp1, tmp2]
    finc tmp2 y
    var tmp1
    loop $ do
      inc tmp2
      dec tmp0
      for tmp0 $ do
        clear tmp2
        inc tmp3
      finc tmp3 tmp0
      for tmp2 $ do
        dec tmp1
        loop $ do
          dec x
          clear tmp1
        low V.Inc
    inc x
    var tmp1

-- x = x ^ y
pow x y = vars 3 $ \[tmp0,tmp1,tmp2] -> do
  clear tmp0
  finc x tmp0
  inc x
  for y $ do
    clears [tmp1, tmp2]
    finc x tmp2
    for tmp2 $ do
      fincs tmp0 [x, tmp1]
      finc tmp1 tmp0

swap x y = vars' 1 $ \[tmp0] -> do
  finc x tmp0
  finc y x
  finc tmp0 y

-- lz = loop $ low V.L
-- rz = loop $ low V.R

readAll = do
  low V.Read
  loop $ do
    low V.R
    low V.Read

-- x = x == y
eq x y = vars' 2 $ \[tmp0,tmp1] -> do
  finc x tmp1
  low V.Inc
  for y $ do
    dec tmp1
    inc tmp0
  finc tmp0 y
  lclear tmp1 $ dec x

-- x = x == y, y = 0
eq' x y = do
  for x $ do
    low V.Dec
    dec y
    var x
  low V.Inc
  lclear y $ dec x

-- x = x != y
neq x y = vars' 2 $ \[tmp0,tmp1] -> do
  finc x tmp1
  for y $ do
    dec tmp1
    inc tmp0
  finc tmp0 y
  lclear tmp1 $ inc x

-- x = x <= y
leq x y = vars' 2 $ \[tmp0,tmp1] -> do
  low V.R
  loop $ low V.Dec
  low V.Inc
  low V.R
  loop $ low V.Dec
  low V.L
  low V.L
  fincs y [tmp0, tmp1]
  finc tmp0 y
  finc x tmp0
  low V.Inc

  var tmp1
  loop $ low V.R >> low V.Dec
  low V.R

  loop $ do
    low V.L
    dec x
    clear tmp0
    var tmp1
    low V.R
    low V.Dec
    low V.R
  low V.L
  low V.Inc
  low V.L

  for tmp0 $ do
    dec tmp1
    loop $ low V.R >> low V.Dec
    low V.R

    loop $ do
      low V.L
      dec x
      clear tmp0
      low V.Inc
      var tmp1
      low V.R
      low V.Dec
      low V.R

    low V.L
    low V.Inc
    low V.L


-- x = x < y
lt x y = vars' 2 $ \[tmp0,tmp1] -> do
  clears [tmp0, tmp1]
  low V.R
  loop $ low V.Dec
  low V.Inc
  low V.R
  loop $ low V.Dec
  low V.L
  low V.L

  fincs y [tmp0, tmp1]
  finc tmp1 y
  finc tmp1 x

  var tmp1
  loop $ do
    low V.R
    low V.Dec
  low V.R
  loop $ do
    low V.L
    inc x
    clear tmp0
    var tmp1
    low V.R
    low V.Dec
    low V.R
  low V.L
  low V.Inc
  low V.L

  for tmp0 $ do
    dec tmp1
    loop $ do
      low V.R
      low V.Dec
    low V.R

    loop $ do
      low V.L
      inc x
      clear tmp0
      low V.Inc
      var tmp1
      low V.R
      low V.Dec
      low V.R

    low V.L
    low V.Inc
    low V.L

-- z = x > y
gt' z x y = vars' 2 $ \[tmp0, tmp1] -> do
  clear z
  for x $ do
    inc tmp0
    var y
    loop $ do
      low V.Dec
      clear tmp0
      inc tmp1
      var y

    var tmp0
    loop $ do
      low V.Dec
      inc z
      var tmp0

    var tmp1
    loop $ do
      low V.Dec
      inc y
      var tmp1

    dec y

-- z = x - y
sub' z x y = vars' 2 $ \[tmp0,tmp1] -> do
  clear z
  for x $ do
    inc tmp0
    var y
    loop $ do
      low V.Dec
      clear tmp0
      inc tmp1
      var y
    var tmp0
    loop $ do
      low V.Dec
      inc z
      var tmp0
    loop $ do
      low V.Dec
      inc y
      var tmp1
    dec y

-- x = not x
not x = vars' 1 $ \[tmp0] -> do
  clear tmp0
  lclear x $ inc tmp0
  low V.Inc
  for tmp0 $ dec x

-- x = x and y
and x y = vars' 2 $ \[tmp0,tmp1] -> do
  finc x tmp1
  var tmp1
  loop $ do
    clear tmp1
    fincs y [tmp1, tmp0]
    finc tmp0 y
    lclear tmp1 $ inc x

-- z = x and y
and' z x y = do
  clear z
  for x $ finc y z
  clear y

-- x = x or y
or x y = vars' 2 $ \[tmp0,tmp1] -> do
  finc x tmp1
  lclear tmp1 $ dec x
  fincs y [tmp1, tmp0]
  finc tmp0 y
  lclear tmp1 $ do
    clear x
    low V.Dec

-- z = x or y
or' z x y = vars' 1 $ \[tmp0] -> do
  clear z
  low V.Inc
  for x $ inc z >> dec tmp0
  var tmp0
  loop $ do
    low V.Dec
    finc y z
  clear y

while cond code = do
  cond
  loop $ code >> cond

iff cond code = vars' 2 $ \[tmp0,tmp1] -> do
  fincs cond [tmp0, tmp1]
  finc tmp0 cond
  lclear tmp1 code

-- clears cond
iff' = lclear

elif x t f = vars 2 $ \[tmp0,tmp1] -> do
  clear tmp0
  low V.Inc
  clear tmp1
  var x
  loop $ do
    t
    dec tmp0
    finc x tmp1
  finc tmp1 x
  for tmp0 f

-- TODO: rand

num :: Int -> Op
num n = modify $ \x -> x{doc = doc x ++ replicate n V.Inc}

writeChar v = var v >> low V.Write
readChar    = low V.Read

(~=)  = assign
(*=)  = mult
(+=)  = add
(^=)  = pow
(~==) = eq
(~/=) = neq
(~>=) = leq
(~&)  = and
(~|)  = or
(~>)  = lt

ivar n = popVar >>= \x -> var x >> num n >> return x

-- transpiles to: B+B+B+B+B+C+C+A,A[-B-AA-]+B[A-B[-]]D[-]+E[-]A[F[-]G[-]H[-]A[H+G+A-]G[H[A+F+H-]F[H+F-]G-]D-A[E+A-]]E[A+E-]D[F[-]G[-]B[G+B-]G[A[B+F+A-]F[A+F-]G-]D-].

test' = B.showBf . trans . run $ do
  x <- popVar
  var x >> readChar
  y <- ivar 5
  z <- ivar 2
  x ~== y
  elif x (pow2 x) (mult y z)
  low V.Write
