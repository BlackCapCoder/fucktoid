module BFComp where

import qualified Brainfuck as B
import qualified Data.Map as M
import Debug.Trace
import Control.Monad
import Data.List
import Data.Char

compile bf = unlines
  [ "#include <stdio.h>"
  , "#include <string.h>"
  , "#include <stdio.h>\n"
  , "int main () {"
  , "  #define TAPE_SIZE 65536"
  , "  char mem[TAPE_SIZE] = {0};"
  , "  int ptr;\n"
  , converted
  , "\n  return 0;"
  , "}"
  ]
  where converted = clever bf

naive bf = unlines $ (map $ \x -> case x of
  B.R -> "  ptr++;"
  B.L -> "  ptr--;"
  B.Inc -> "  mem[ptr]++;"
  B.Dec -> "  mem[ptr]--;"
  B.Write -> "  putchar(mem[ptr]);"
  B.Read -> "  mem[ptr]=getchar();"
  (B.Loop x) -> "  while (mem[ptr]) {" ++ naive x ++ "}") bf

testComp = do
  forM_ (words "mandelbroth") $ \x -> do --LostKng 
    c <- compile <$> B.parseFile (x++".bf")
    writeFile (x++".c") c

testIm = do
  let pth = "LostKng"
  -- g <- parse <$> B.parseFile (pth++".bf")
  g <- readFile (pth ++ ".im")
  let v = read g :: [Op]
  -- print v
  -- t <- contract $ read g
  writeFile (pth++".imo") $ show $ trimTail $ contract v
  writeFile (pth++".c") $ unlines
    [ "#include <stdio.h>"
    , "#include <string.h>"
    , "#include <stdio.h>\n"
    , "int main () {"
    , "  #define TAPE_SIZE 65536"
    , "  char mem[TAPE_SIZE] = {0};"
    , "  int ptr;\n"
    , write v
    , "\n  return 0;"
    , "}"
    ]


data Op = Move Int | Inc Int | Inp | Out | Loop [Op]
        | Clear | Copy Int Int | ScanL | ScanR
        | IncRel Int Int | SetRel Int Int | CopyRel Int Int Int
        | InpRel Int | OutRel Int
        | AssRel Int Int | Field Int Int [Int] | Put [Int]
        deriving (Eq, Show, Read)

clever x | q <- opti $ Field 0 10 (replicate 10 0) : parse x = {- trace (show q) $ -} write q
  where opti x | y <- contract x
               = if x == y then trimTail y else opti y



write = (=<<) $ \x -> case x of
  Move   1       -> "ptr++;"
  Move (-1)      -> "ptr--;"
  Move x | x > 0 -> "ptr+=" ++ show x ++ ";"
  Move x | x < 0 -> "ptr-=" ++ show (abs x) ++ ";"
  Inc   1        -> "mem[ptr]++;"
  Inc (-1)       -> "mem[ptr]--;"
  Inc  x | x > 0 -> "mem[ptr]+=" ++ show x ++ ";"
  Inc  x | x < 0 -> "mem[ptr]-=" ++ show (abs x) ++ ";"
  Inp -> "mem[ptr]=getchar();"
  InpRel o -> "mem[ptr" ++ addStr o ++ "]=getchar();"
  Out -> "putchar(mem[ptr]);"
  OutRel o -> "putchar(mem[ptr" ++ addStr o ++ "]);"
  Loop o -> "\nwhile(mem[ptr]){\n" ++ write o ++ "\n}\n"
  Clear -> "mem[ptr]=0;"
  SetRel n x -> "mem[ptr" ++ (addStr n) ++ "]=" ++ (show x) ++ ";"
  AssRel a b -> "mem[ptr"++addStr a++"]=mem[ptr"++addStr b++"];"
  Copy m x | x > 0 -> "mem[ptr+" ++ show x ++ "]+=mem[ptr]" ++ (if m == 1 then "" else "*" ++ show m) ++ ";"
  Copy m x | x < 0 -> "mem[ptr-" ++ show (abs x) ++ "]+=mem[ptr]" ++ (if m == 1 then "" else "*" ++ show m) ++ ";"
  CopyRel a b m -> "mem[ptr" ++ (addStr a) ++ "]+=mem[ptr" ++ (addStr b) ++ "]"
               ++ (if m == 1 then "" else "*" ++ show m) ++ ";"
  ScanL -> "ptr-=(long)((void*)(mem+ptr)-memrchr(mem,0,ptr+1));"
  ScanR -> "ptr+=(long)(memchr(mem+ptr,0,sizeof(mem))-(void*)(mem+ptr));"
  IncRel o n -> "mem[ptr" ++ (if o > 0 then "+" ++ show o else "-" ++ show (abs o)) ++ "]"
             ++ (if n > 0 then "+=" ++ show n else "-=" ++ show (abs n)) ++ ";"
  Field s l xs -> "memcpy(&mem[ptr"++addStr s++"],(char["++show l++"]){"
               ++ intercalate "," (map show xs)++"},"++show (l*1)++");"
  Put xs -> "fputs("++show (map chr xs)++", stdout);"
  _ -> ""
  -- x -> error $ show x

addStr n
 | n == 0 = ""
 | n > 0 = "+" ++ show n
 | n < 0 = "-" ++ show (abs n)

parse = map $ \x -> case x of
  B.R     -> Move   1
  B.L     -> Move (-1)
  B.Inc   -> Inc    1
  B.Dec   -> Inc  (-1)
  B.Read  -> Inp
  B.Write -> Out
  (B.Loop x) -> Loop (parse x)

-- Clear = SetRel 0 0

updArr a i f | (x,y:z) <- splitAt i a = x ++ f y : z
             | otherwise = error $ show a ++ ", " ++ show i

contract (Clear:xs) = contract $ SetRel 0 0 : xs
contract (IncRel 0 x:xs) = contract $ Inc x : xs
contract (InpRel 0:xs) = contract $ Inp : xs
contract (OutRel 0:xs) = contract $ Out : xs

contract (Move x:CopyRel a b m:xs) = contract $ CopyRel (a+x) (b+x) m : Move x : xs
contract (Move a:SetRel m x:xs) = contract $ SetRel (m+a) x : Move a : xs -- Expensive!

contract (Inc 0:xs)  = contract $ xs
contract (IncRel _ 0:xs) = contract $ xs
contract (Move 0:xs) = contract $ xs
contract (ScanL:Loop _:xs) = contract $ ScanL : xs
contract (ScanR:Loop _:xs) = contract $ ScanR : xs
contract (ScanL:Copy _ _:xs) = contract $ ScanL : xs
contract (ScanR:Copy _ _:xs) = contract $ ScanR : xs

contract (SetRel a 0:CopyRel c d 1:xs)
  | a == c = contract $ AssRel a d : xs

contract (SetRel a b:CopyRel c d m:xs)
  | a == d = contract $ SetRel a b : SetRel c (b*m) : xs

contract (Move a:Move b:xs) = contract $ Move (a+b) : xs
contract (Move a:Inc b:xs) = contract $ IncRel a b : Move a : xs
contract (Move a:IncRel b i:xs) = contract $ IncRel (a+b) i : Move a : xs

contract (SetRel a b:SetRel c d:xs)
  | a == c = contract $ SetRel a (b+d) : xs
  | a > c = SetRel c d : (contract $ SetRel a b : xs)
  | c-a == 1 = contract $ Field a 2 [b,d] : xs
  | a-c == 1 = contract $ Field c 2 [d,b] : xs

contract (IncRel a b:IncRel c d:xs)
  | a==c = contract $ IncRel a (b+d) : xs
  | a > c = IncRel c d : (contract $ IncRel a b : xs)

contract (IncRel a b:SetRel c d:xs)
  | a == c = contract $ SetRel c d : xs

contract (SetRel a b:IncRel c d:xs)
  | a == c = contract $ SetRel a (b+d) : xs

contract (Field s l es:IncRel a b:xs)
  | s <= a, a-s <= l = contract $ Field s l (updArr es (a-s) (+b)) : xs

contract (Field s l es:CopyRel a b m:xs)
  | s < a, a-s < l, s < b, b-s < l = contract $ Field s l (updArr es (a-s) (\x->x+(es!!b)*m)) : xs

contract (Field s l es:Inc b:xs)
  = contract $ Field s l (updArr es 0 (+b)) : xs

contract (Field s l es:Out:xs)
  = contract $ Put [head es] : Field s l es : xs

contract (Field s l es:OutRel a:xs)
  | s <= a, a-s <= l = contract $ Put [es!!a-s] : Field s l es : xs

contract (Field a b c:Field d e f:xs)
  | a + b == d = contract $ Field a (b+e) (c++f) : xs

contract (Field a b c:SetRel d e:xs)
  | a+b == d = contract $ Field a (b+1) (c++[e]) : xs

contract (Put a:Put b:xs) = contract $ Put (a++b) : xs

contract (Inc _:Inp:xs) = contract $ Inp : xs
contract (SetRel 0 _:Inp:xs) = contract $ Inp : xs

contract (Move x:Inp:xs) = contract $ InpRel x : Move x : xs
contract (Move x:Out:xs) = contract $ OutRel x : Move x : xs


-- mem[ptr+5]+=3;mem[ptr+6]+=mem[ptr+5]*5;mem[ptr+5]=0;
-- IncRel 5 3 : CopyRel 6 5 5 : SetRel 5 0
-- CopyRel 6 5 5 : IncRel 6 3*5 : IncRel 5 3 : SetRel 5 0
contract (IncRel a b:CopyRel c d m:xs)
  | a == d = contract $ CopyRel c d m : IncRel c (b*m) : IncRel a b : xs

-- CopyRel a b m = mem[a]+=mem[b]*m
-- SetRel d e    = mem[d]=e
contract (CopyRel a b m:SetRel d e:xs)
  | a == d = contract $ SetRel d e:xs
  | b /= d = contract $ SetRel d e : CopyRel a b m : xs
contract (CopyRel a b m:SetRel c d:SetRel e f:xs)
  = contract $ CopyRel a b m:SetRel e f:SetRel c d:xs -- Move around to detect NOP

contract (CopyRel a b c:CopyRel d e f:xs)
  | a == d, b == e = contract $ CopyRel a b (c*f) : xs

-- Order
contract (SetRel a b:IncRel c d:xs)
  | a > c = contract $ IncRel c d : SetRel a b : xs
contract (IncRel a b:SetRel c d:xs) | a > c = contract $ SetRel c d : IncRel a b : xs

contract (IncRel a b:Inc x:xs) = Inc x : (contract $ IncRel a b : xs)
contract (Inc a:SetRel 0 x:xs) = contract $ SetRel 0 x : xs
contract (Inc a:Inc  b:xs) = contract $ Inc  (a+b) : xs

contract (SetRel 0 0:Loop _:xs)  = contract $ SetRel 0 0 : xs
contract (SetRel 0 0:Copy _ _:xs)  = contract $ SetRel 0 0 : xs
contract (Loop [Inc _]:xs) = contract $ SetRel 0 0 : xs
contract (Loop [Move 1]:xs) = contract $ ScanR : xs
contract (Loop [Move(-1)]:xs) = contract $ ScanL : xs
contract (Loop x:Loop _:xs) = contract $ Loop x : xs
contract (Loop x:SetRel 0 0:xs)  = contract $ Loop x : xs
contract (Loop x:Copy _ _:xs)  = contract $ Loop x : xs
contract (Loop a:xs) | Just a <- unwindLoop a = contract $ a ++ xs
contract (Loop a:xs) = Loop (contract a) : contract xs
contract (x:xs)      = x : contract xs
contract []          = []

isPure Inp = False
isPure Out = False
isPure (InpRel _) = False
isPure (OutRel _) = False
isPure (Put _) = False
isPure (Loop x) = arePure x
isPure _   = True
arePure xs = all isPure xs

offset (Move x:xs) | Just a <- offset xs = Just $ x + a
offset (Loop x:xs) | isRound x = offset xs
offset (Inc _:xs) = offset xs
offset (Copy _ _:xs) = offset xs
offset (SetRel _ _:xs) = offset xs
offset (Clear:xs) = offset xs
offset (x:_) = Nothing
offset [] = Just 0

isRound x = case offset x of
  Just 0 -> True
  _   -> False

unwindLoop x
  | not $ isRound x = Nothing
  | any supported x = Nothing
  | m <- fst $ foldl f (M.empty, 0) x
  , Just h <- M.lookup 0 m
  , h < 0
  , l <- M.toList $ M.delete 0 m
  = Just $ (map (\(k, n) -> CopyRel k 0 n) l) ++ [SetRel 0 0] --[Clear] --
  | otherwise = Nothing
  where f (m, p) x = case x of
          Move n -> (m, p+n)
          Inc  n -> (M.insertWith (+) p n m, p)
          IncRel o n -> (M.insertWith (+) (p+o) n m, p)
          Copy n k -> foldl f (m, p) [Move k, Inc n, Move (-k), Inc (-1)]
          Loop x -> foldl f (m, p) x -- has to clear
        supported x = case x of
                        Move _ -> False
                        Inc  _ -> False
                        IncRel _ _ -> False
                        -- Copy _ _ -> False
                        -- Loop x -> any supported x
                        _      -> True

trimTail = reverse . dropWhile isPure . reverse
