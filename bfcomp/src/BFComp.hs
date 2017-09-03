module BFComp where

import Control.Lens
import Control.Monad
import Debug.Trace
import Data.List
import Optimize

data Op = Move Int
        | Add  Int
        | In
        | Out
        | Loop Program
        | Set  Int
        | NOP
        | Rel Int Op
        | AddMult Int Int
        | Scan Int
        | Field Int Int [Int]
        deriving (Read, Show, Eq)

type Program = [Op]


parse :: String -> Program
parse = fst . parse'
  where parse' []       = ([NOP], [])
        parse' (']':xs) = ([NOP], xs)
        parse' xs | Just (o, rst) <- parseOp xs = parse' rst & _1 %~ (o:)
                  | otherwise = parse' $ tail xs

        parseOp (x:xs)
          = case x of
              '+' -> Just (Add 1,     xs)
              '-' -> Just (Add (-1),  xs)
              '>' -> Just (Move 1,    xs)
              '<' -> Just (Move (-1), xs)
              ',' -> Just (In       , xs)
              '.' -> Just (Out      , xs)
              '[' | (x,xs) <- parse' xs -> Just (Loop x, xs)
              _   -> Nothing

incStr :: Int -> String
incStr x | x == 1 = "++"
         | x == (-1) = "--"
         | otherwise = incStr'' x

incStr' x | x > 0 = '+':show x
          | x < 0 = '-':show (abs x)
          | x == 0 = ""

incStr'' x | x < 0 = "-="++show (abs x)
           | x > 0 = "+="++show x

toC :: Program -> String
toC = unlines . map (\case
  Move x         -> "ptr" ++ incStr x ++ ";"
  Add  x         -> "mem[ptr]" ++ incStr x ++ ";"
  In             -> "mem[ptr]=getchar();"
  Out            -> "putchar(mem[ptr]);"
  Loop o         -> "while(mem[ptr]){\n" ++ indent(toC o) ++ "}"
  Set x          -> "mem[ptr]=" ++ show x ++ ";"
  Rel p (Add x)  -> "mem[ptr" ++ incStr' p ++ "]" ++ incStr x ++ ";"
  Rel p (Set x)  -> "mem[ptr" ++ incStr' p ++ "]=" ++ show x ++ ";"
  Rel p Out      -> "putchar(mem[ptr" ++ incStr' p ++ "]);"
  Rel p In       -> "mem[ptr" ++ incStr' p ++ "]=getchar();"
  AddMult b c    -> "mem[ptr]" ++ (if abs b == 1 then (if b < 0 then "-=" else "+=") else incStr'' b ++ "*") ++ "mem[ptr"++ incStr' c ++"];"
  Rel a (AddMult b c) -> "mem[ptr"++ incStr' a ++"]" ++ (if abs b == 1 then (if b < 0 then "-=" else "+=") else incStr'' b ++ "*") ++ "mem[ptr"++ incStr' c ++"];"
  Field s l xs   -> "memcpy(&mem[ptr"++ incStr' s++"],(char["++show l++"]){"
                 ++ intercalate "," (map show xs)++"},"++show (l*1)++");"
  Scan 1         -> "ptr+=(long)(memchr(mem+ptr,0,sizeof(mem))-(void*)(mem+ptr));"
  Scan (-1)      -> "ptr-=(long)((void*)(mem+ptr)-memrchr(mem,0,ptr+1));"
  Scan n         -> "while(mem[ptr]){ptr"++incStr n++";}"

  _              -> ""
  ) . filter (/=NOP)

indent = unlines . map ("  "++) . lines

compile :: Program -> String
compile p
  | p' <- p
  = unlines
    [ "#include <stdio.h>"
    , "#include <string.h>"
    , "#include <stdio.h>\n"
    , "int main () {"
    , "#define TAPE_SIZE 65536"
    , "char mem[TAPE_SIZE] = {0};"
    , "int ptr=0;\n"
    , toC $ runOpt optimize' p
    , "return 0;"
    , "}"
    ]

optimize :: Opt Op
optimize = untilFail' $ atEvery' $ untilFail' allOpts

optimize' = untilFail' $ tryAll [atEvery' $ untilFail' allOpts, setInit, atEvery' rmTrainingPure]


delNopAdd = tryAll
  [ \p -> do (Add  0          : x) <- pure p; Just x
  , \p -> do (Add  _ : Set b  : x) <- pure p; Just $ Set b : x
  , \p -> do (Add  _ : In     : x) <- pure p; Just $ In : x
  ]
delNopSet = tryAll
  [ \p -> do (Set  _ : Set b  : x) <- pure p; Just $ Set b : x
  , \p -> do (Set  _ : In     : x) <- pure p; Just $ In : x
  ]
delNopLoop = tryAll
  [ \p -> do (Set  0 : Loop _ : x) <- pure p; Just $ Set 0 : x
  , \p -> do (Loop a : Loop _ : x) <- pure p; Just $ Loop a : x
  , \p -> do (Scan a : Loop _ : x) <- pure p; Just $ Scan a : x
  ]
zeroLoop = tryAll
  [ \p -> do (Loop [Add _, NOP] : x) <- pure p; Just $ Set 0 : x
  , \p -> do (Loop [Set 0, NOP] : x) <- pure p; Just $ Set 0 : x
  ]
delNopMov  p = do (Move 0          : x) <- pure p; Just x


joinMoves  p = do (Move a : Move b : x) <- pure p; Just $ Move (a+b) : x
joinAdds   p = do (Add  a : Add  b : x) <- pure p; Just $ Add  (a+b) : x
joinSetAdd p = do (Set  a : Add  b : x) <- pure p; Just $ Set (a+b) : x

moveNOP    p = do (NOP : a         : x) <- pure p; Just $ a : NOP : x

makeSet = tryAll
  [ \p -> do (Loop a : Add b  : x) <- pure p; Just $ Loop a : Set b : x
  , \p -> do (Scan a : Add b  : x) <- pure p; Just $ Scan a : Set b : x
  ]

makeScan p = do (Loop [Move a, NOP] : x) <- pure p; Just $ Scan a : x


makeRel p = do
  (Move a : q : x) <- pure p
  case q of
    Add b -> Just $ Rel a (Add b) : Move a : x
    Set b -> Just $ Rel a (Set b) : Move a : x
    In    -> Just $ Rel a In      : Move a : x
    Out   -> Just $ Rel a Out     : Move a : x
    _     -> Nothing

unRel = tryAll
  [ \p -> do (Rel 0 q : x) <- pure p; Just $ q : x
  , \p -> do (Move a : Rel b (AddMult c d) : x) <- pure p; Just $ Rel (a+b) (AddMult c $ d + a) : Move a : x
  , \p -> do (Move a : Rel b c : x) <- pure p; Just $ Rel (b+a) c : Move a : x
  ]

isAddMult (AddMult _ _) = True
isAddMult _             = False

moveRel p = do
  (Move a : Rel b c : x) <- pure p
  guard . not $ isAddMult c
  Just $ Rel (a+b) c : Move a : x

joinRel = tryAll
  [ \p -> do (Rel a (Add b) : Rel c (Add d) : x) <- pure p; guard (a==c); Just $ Rel a (Add(b+d)) : x
  , \p -> do (Rel a (Set _) : Rel c (Set d) : x) <- pure p; guard (a==c); Just $ Rel a (Set d) : x
  , \p -> do (Rel a (Add _) : Rel c (Set d) : x) <- pure p; guard (a==c); Just $ Rel a (Set d) : x
  , \p -> do (Rel a (Set b) : Rel c (Add d) : x) <- pure p; guard (a==c); Just $ Rel a (Set(b+d)) : x
  , \p -> do (Rel a (Set _) : Rel c In      : x) <- pure p; guard (a==c); Just $ Rel c In : x
  , \p -> do (Rel a (Add _) : Rel c In      : x) <- pure p; guard (a==c); Just $ Rel c In : x
  , \p -> do (Set 0 : Rel _ (AddMult _ 0)   : x) <- pure p; Just $ Set 0 : x
  , \p -> do (Set a : Rel b (AddMult c 0)   : x) <- pure p; Just $ Rel b (Set(c*a)) : Set a : x
  , \p -> do (Set a : Rel b c               : x) <- pure p; Just $ Rel b c : Set a : x
  , \p -> do (Rel a (Set 0) : Rel c (AddMult d e) : x) <- pure p; guard (a==e); Just $ Rel c (Set 0) : x
  , \p -> do (Rel a (Set q) : b : Rel c (AddMult v e) : x) <- pure p; guard (a==e); Just $ Rel a (Set q) : b : Rel c (Add $ v*q) : x
  , \p -> do (Rel a (Set 0) : Rel b (Set v) : Rel c (AddMult d e) : x) <- pure p; guard (a==e); Just $ Rel a (Set 0) : Rel b (Set v) : x -- ?

  , \p -> do (Rel a (Set b) : Rel c (Set d) : x)<-pure p; guard (c>a); Just $ Rel c(Set d) : Rel a(Set b):x
  -- , \p -> do (Rel a (AddMult b c) : Rel d (Set e) : x)<-pure p; guard (d>a&&c/=e); Just $ Rel d(Set e):Rel a(AddMult b c):x
  , \p -> do (Rel a (Set b) : Rel d (Add e) : x)<-pure p; guard (a/=d); Just $ Rel d(Add e):Rel a (Set b):x

  ]
  -- mem[ptr+a]=0;
  -- mem[ptr+b]=v;
  -- mem[ptr+c]+=d*mem[ptr+a];

delRel p = do (Rel a (Add 0) : x) <- pure p; Just x

optLoop p = do
  (Loop p' : x) <- pure p
  t <- optimize p'
  Just $ Loop t : x

inLoop o p | Just p' <- o p = Just p'
           | otherwise   = do (Loop l : x) <- pure p; t <- o l; Just $ Loop t : x

orderRels p = do
  (Rel a b : Rel c d : x) <- pure p
  guard $ a < c
  -- Nothing
  case b of
    AddMult w q | q==c -> Nothing
                | otherwise -> Just $ Rel c d : Rel a b : x
    Out -> Nothing
    In  -> Nothing
    _ -> Nothing
    _ -> Just $ Rel c d : Rel a b : x


makeAddMult p = do
  (Loop a : x) <- pure p
  guard $ flip all a $ \case Add _         -> True
                             Rel _ (Add _) -> True
                             NOP           -> True
                             _             -> False
  q@(Add (-1)) <- flip find a
                  $ \case Add _ -> True
                          _     -> False

  let xs = map (\(Rel y (Add w)) -> Rel y $ AddMult w 0)
         $ filter (\t -> t/=q && t/=NOP) a

  Just $ xs ++ Set 0 : x


makeField p = do
  (Rel a (Set b) : Rel c (Set d) : x) <- pure p
  guard $ abs(a-c)==1
  if a < c
     then Just $ Field a 2 [b, d] : x
     else Just $ Field c 2 [d, b] : x

joinField = tryAll
  [ \p -> do
      (t@(Field q _ _):v@(Field w _ _): x) <- pure p
      let (Field a b xs, Field c d ys) = if q < w then (t, v) else (v, t)
      guard $ a+b<=c
      Just $ Field a (b+d-(a+b-c)) ((foldl (.) id (replicate (a+b-c) init) xs) ++ ys) : x

  , \p -> do
    (Field s l xs:Rel a (Add b):x)<-pure p
    guard $ a>=s&&a<s+l
    Just $ Field s l (take (a-s) xs ++ (xs!!(a-s))+b : drop (a-s+1) xs) : x
  , \p -> do
    (Rel a (Add _):Field s l xs:x)<-pure p
    guard $ a>=s&&a<s+l
    Just $ Field s l xs : x
  , \p -> do
    (Rel a (Set _):Field s l xs:x)<-pure p
    guard $ a>=s&&a<s+l
    Just $ Field s l xs : x
  , \p -> do
    (Field s l xs : Rel a (AddMult v b) : x) <- pure p
    guard $ b>=s && b <= s+l
    Just $ Field s l xs : Rel a (Set $ v * xs!!(s-b)) : x
  ]

moveOut = tryAll
  [ \p -> do (Rel a Out : Rel b c : x)<-pure p; guard $ (a /= b) && c/=Out; Just $ Rel b c : Rel a Out : x
  ]

orderField = tryAll
  [ \p -> do (a@(Field s l _):t@(Rel q (Add _)):x)<-pure p; guard $ q<s||q>s+l; Just $ t:a:x
  , \p -> do (a@(Field s l _):t@(Rel q (Set _)):x)<-pure p; guard $ q<s||q>s+l; Just $ t:a:x
  ]



allOpts :: Opt Op
allOpts = tryAll [ joinMoves, joinAdds, joinSetAdd, joinRel, joinField
                 , delNopMov, delNopAdd, delNopSet, delNopLoop, delRel, unRel
                 , moveNOP, moveRel, orderField, orderRels, moveOut
                 , makeRel, makeSet, makeScan, makeAddMult, makeField
                 , zeroLoop, optLoop
                 ]

----------

setInit = tryAll
  [ \p -> do (Add a : x) <- pure p; Just $ Set a : x
  , \p -> do (Rel a (Add b) : x) <- pure p; Just $ Rel a (Set b) : x
  ]

isPure In  = False
isPure Out = False
isPure (Rel _ In) = False
isPure (Rel _ Out) = False
isPure (Loop o) = all isPure o
isPure _   = True

rmTrainingPure p = do [x,NOP] <- pure p; guard $ isPure x; Just [NOP]



{-
    [>]  scanr 0
    [<]  scanl 0
    [>-] for (i=0; i<=scanR 1; i++) mem[i]--

    Mandel:
      No optimization: 21.11s
      Joins:           17.98s
      Joins in loop:   4.24s
      Zero loops:      4.22s
      AddRel:          3.98s
      NOPs:            3.73s
      Refactor:        3.54s
      AddMult:         3.33s
      Rel AddMult:     2.99s
-}
