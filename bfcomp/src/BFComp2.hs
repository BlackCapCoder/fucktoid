module BFComp2 where

import Control.Lens
import Control.Monad
import Debug.Trace
import Data.List
import Data.Char
import Optimize
import qualified Data.Map as M
import BF
import Unroll
import Numeric (showHex)
import CompileOpt


incStr x | x == 1 = "++"
         | x == (-1) = "--"
         | otherwise = incStr'' x

incStr' x | x > 0 = '+':show x
          | x < 0 = '-':show (abs x)
          | x == 0 = ""

incStr'' x | x < 0 = "-="++show (abs x)
           | otherwise = "+="++show x

printStr = concatMap $ \n ->
  if | n=='"' -> "\\\""
     | ['\'',x,'\''] <- show n -> [x]
     | ['\'','\\',x,'\''] <- show n -> ['\\', x]
     | otherwise -> "\\x" ++ showHex (ord n) "" ++ "\" \""

toC :: Program -> String
toC = unlines . map (\case
  Move x         -> "ptr" ++ incStr x ++ ";"
  In             -> "mem[ptr]=getchar();"
  Out x          -> "putchar(mem[ptr"++incStr' x++"]);"
  Put str        -> "printf(\""++printStr str++"\");"
  Loop l o       -> "while(mem[ptr"++incStr' l++"]){\n" ++ indent(toC o) ++ "}"
  Add  m         -> M.foldlWithKey (\a k x -> a++"mem[ptr"++incStr' k++"]" ++ incStr x ++ ";") "" m
  Set m          -> M.foldlWithKey (\a k x -> a++"mem[ptr"++incStr' k++"]=" ++ show x ++ ";") "" m
  Mult l m       -> M.foldlWithKey (\a k x -> a++"mem[ptr"++incStr' k++"]+=" ++ (if x == 1 then "" else show x ++ "*") ++ "mem[ptr"++ incStr' l ++ "];") "" m
  When l p       -> "if(mem["++incStr' l++"]){\n"++indent(toC p)++"}"
  DUMP           -> unlines
                      [ "for (int i=0; i<TAPE_SIZE; i++){"
                      ,   "if (mem[i]==0) continue;"
                      ,   "fprintf(stderr, \"%d %d\\n\\0\", i, mem[i]);"
                      , "}" ]
  _              -> ""
  ) . filter (/=NOP)

indent = unlines . map ("  "++) . lines

compile o p
  | p' <- p
  = unlines
    [ "#include <stdio.h>"
    , "#include <string.h>"
    , "#include <stdio.h>\n"
    , "int main () {"
    , "#define TAPE_SIZE 65536"
    , "char mem[TAPE_SIZE] = {0};"
    , "int ptr=0;\n"
    , toC $ o p
    , "return 0;"
    , "}"
    ]

--------------

optimize  = untilFail' $ atEvery' allOpts
optimize' = untilFail' $ tryAll [ outsideLoop, atEvery' allOpts
                                -- , unroll'                 -- Interpret with max iterations
                                , compileOpt (compile id) -- Compile and run with max time
                                ]

allOpts = tryAll
  [ joinMoves, moveMoves, joinFields, optFields
  , optLoop, {- createMult, -} put
  ]

outsideLoop = tryAll
  [ \p -> [ Set a : xs | (Add a : xs) <- pure p ]
  , \p -> [ Set n : xs | (Set a : xs) <- pure p, let n = M.filter (/=0) a, n/=a ]
  -- , untilFail' $ atEvery' $ \p -> [ [NOP] | [x,NOP]<-pure p, isPure x ]
  -- , atEvery' $ \p -> [ [x, NOP] | [x]<-pure p, x /= NOP ]
  ]



joinFields = tryAll
  [ \p -> [ Set (M.union b a)         : xs | (Set a : Set b : xs)<-pure p ]
  , \p -> [ Add q : Set b             : xs | (Add a : Set b : xs)<-pure p
          , let q = a M.\\ b, q /= a]
  , \p -> [ Add b : Set a             : xs | (Set a : Add b : xs)<-pure p
          , a M.\\ b == a ]
  , \p -> [ Mult a (M.union b d) : xs | (Mult a b : Mult c d : xs)<-pure p, a==c]
  , \p -> [ Set a : Add (M.map (*d) c) : xs | (Set a : Mult b c : xs)<-pure p,d <- M.lookup b a ]
  , \p -> [ Loop l a : Set (M.singleton l 0) : Add b : xs | (Loop l a : Add b : xs)<-pure p, c<-M.lookup l b ]

  , \p -> [ Add (M.unionWith (+) a b) : xs | (Add a : Add b : xs)<-pure p ]
  , \p -> [ Set (M.union x a) : Add (b M.\\ x) : xs
          | (Set a : Add b : xs)<-pure p
          , let x = M.intersectionWith (+) a b
          , not $ M.null x
          ]
  ]

optFields = tryAll
  [ \p -> [ x | (Add    m:x)<-pure p, M.null m ]
  , \p -> [ x | (Set    m:x)<-pure p, M.null m ]
  , \p -> [ x | (Mult _ m:x)<-pure p, M.null m ]
  , \p -> [ Add q : x | (Add a : x)<-pure p, let q = M.filter (/=0) a, q/=a ]
  ]

moveMoves = tryAll
  [ \p -> [ Out (a+b) : Move a : xs | (Move a : Out b : xs)<-pure p ]
  , \p -> [ Add (M.mapKeys (+a) b) : Move a : xs | (Move a : Add b : xs)<-pure p ]
  , \p -> [ Set (M.mapKeys (+a) b) : Move a : xs | (Move a : Set b : xs)<-pure p ]
  , \p -> [ Mult (m+a) (M.mapKeys (+a) b) : Move a : xs | (Move a : Mult m b : xs)<-pure p ]
  ]

joinMoves  p = [ if a+b==0 then x else Move (a+b) : x | (Move a : Move b : x)<-pure p ]
createMult p = [ Mult l (M.delete l m) : Set (M.singleton l 0) : x | (Loop l [Add m, NOP]:x)<-pure p
               , (-1) <- M.lookup l m ]

optLoop = tryAll
  [ \p -> [ Loop l t : xs | (Loop l o:xs) <- pure p, t <- optimize o]
  , \p -> [ Loop l a : xs | (Loop l a:Loop g _:xs)<-pure p, l==g ]
  , \p -> [ xs | (Set a:Loop l _:xs)<-pure p, 0 <- M.lookup l a ]
  -- , \p -> [ Loop (l+p) (shift p a) : Move p : xs | (Move p : Loop l a : xs)<-pure p ]
  ]

put = tryAll
  [ \p -> [ Put [chr $ fromIntegral x] : Set a : xs | (Set a : Out b : xs)<-pure p, x <- M.lookup b a ]
  , \p -> [ Put (a++b) : xs | (Put a : Put b : xs)<-pure p ]
  , \p -> [ Put a : Set b : xs | (Set b : Put a : xs)<-pure p ]
  ]

shift x = atEvery $ tryAll
  [ \p -> [ Out  (l+x) : xs | (Out l : xs)<-pure p ]
  , \p -> [ Add  (M.mapKeys (+x) o) : xs | (Add o : xs)<-pure p ]
  , \p -> [ Set  (M.mapKeys (+x) o) : xs | (Set o : xs)<-pure p ]
  , \p -> [ Loop (l+x) (shift x o) : xs | (Loop l o : xs)<-pure p ]
  , \p -> [ Mult (l+x) (M.mapKeys (+x) o) : xs | (Mult l o : xs)<-pure p ]
  ]
