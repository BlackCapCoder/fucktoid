{-# Language MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances #-}
module Proc where

import Language
import qualified Brainfuck as B
import qualified BfVars as V
import Control.Monad.Writer

type Op = Writer V.BfVars ()

var, clear, inc, dec :: V.Op -> Op

low = tell . return

var v   = tell $ return v
clear v = tell [v, V.Loop [V.Dec]]
clears  = mapM_ clear
loop o  = tell $ return . V.Loop . snd $ runWriter o

inc v = tell [v, V.Inc]
dec v = tell [v, V.Dec]

incs = mapM_ inc

for v o = do
  var v
  loop $ o >> dec v

finc  v = for v . inc
fincs v = for v . incs

lclear v o = var v >> loop (o >> clear v)

-- x = y
assign x y tmp = do
  clears [x, tmp]
  fincs y [x, tmp]
  finc tmp y

-- x = x + y
add x y tmp = do
  clear tmp
  fincs y [x, tmp]
  finc tmp y

-- x = x - y
sub x y tmp = do
  clear tmp
  for y $ do
    dec x
    inc tmp
  finc tmp y

-- x = x * y
mult x y tmp0 tmp1 = do
  clears [tmp0, tmp1]
  finc x tmp1
  for tmp1 $ do
    fincs y [x, tmp0]
    finc tmp0 y

-- x = x * x
pow2 x tmp0 tmp1 tmp2 = do
  clears [tmp0, tmp1, tmp2]
  fincs x [tmp2, tmp1]
  for tmp1 $ do
    fincs tmp2 [x, tmp0]
    finc tmp0 tmp2

-- x = x / y
div x y tmp0 tmp1 tmp2 tmp3 = do
  clears [tmp0, tmp1, tmp2, tmp3]
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
pow x y tmp0 tmp1 tmp2 = do
  clear tmp0
  finc x tmp0
  inc x
  for y $ do
    clears [tmp1, tmp2]
    finc x tmp2
    for tmp2 $ do
      fincs tmp0 [x, tmp1]
      finc tmp1 tmp0

swap x y tmp0 = do
  clear tmp0
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
eq x y tmp0 tmp1 = do
  clears [tmp0, tmp1]
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
neq x y tmp0 tmp1 = do
  clears [tmp0, tmp1]
  finc x tmp1
  for y $ do
    dec tmp1
    inc tmp0
  finc tmp0 y
  lclear tmp1 $ inc x

-- x = x <= y
leq x y tmp0 tmp1 = do
  clears [tmp0, tmp1]
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
lt x y tmp0 tmp1 = do
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

-- z = x - y
sub' z x y tmp0 tmp1 = do
  clears [tmp0, tmp1, z]
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
not x tmp0 = do
  clear tmp0
  lclear x $ inc tmp0
  low V.Inc
  for tmp0 $ dec x

-- x = x and y
and x y tmp0 tmp1 = do
  clears [tmp0, tmp1]
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
or x y tmp0 tmp1 = do
  clears [tmp0, tmp1]
  finc x tmp1
  lclear tmp1 $ dec x
  fincs y [tmp1, tmp0]
  finc tmp0 y
  lclear tmp1 $ do
    clear x
    low V.Dec

-- z = x or y
or' z x y tmp0 = do
  clears [z, tmp0]
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

iff cond code tmp0 tmp1 = do
  clears [tmp0, tmp1]
  fincs cond [tmp0, tmp1]
  finc tmp0 cond
  lclear tmp1 code

-- clears cond
iff' = lclear

elif x t f tmp0 tmp1 = do
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



test = V.show' . snd . runWriter $ do
  let x = V.Var 0
  let y = V.Var 1
  let z = V.Var 2
  let w = V.Var 3
  pow2 x y z w
