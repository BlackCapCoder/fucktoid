{-# LANGUAGE FlexibleInstances #-}
module Lib where
import Data.Digits
import Control.Monad.Omega
import Control.Monad
import System.Timeout
import Debug.Trace
import Data.List
import qualified Data.Map as M
import qualified Data.Vector as V
import qualified Data.Vector.Persistent as P
import Data.List.Utils
import System.Directory
import Control.Monad.ST
import qualified Data.HashTable.ST.Cuckoo as H
-- import qualified Data.Array.MArray as A

tapeLength = 10 * 1000
ptime      = 50000
file       = "data"

data Op = Halt
        | Prev -- Previous datacell
        | Next -- Next datacell and flip, wraps
        | Skip -- Skip next if zero
        deriving (Show, Eq, Enum, Ord)

type Prog = [Op]


toIndex :: Prog -> Integer
toIndex = unDigits 4 . map f
  where f x = case x of Halt -> 0
                        Prev -> 1
                        Next -> 2
                        Skip -> 3

fromIndex :: Integer -> Prog
fromIndex = map f . digits 4
  where f x = case x of 0 -> Halt
                        1 -> Prev
                        2 -> Next
                        3 -> Skip

-- programs !! x /= fromIndex x
programs :: [Prog]
programs | ps <- [Halt .. Skip] = (pure <$> ps) ++ (runOmega $ (:) <$> each ps <*> each programs)

-- programs is faster than programs', somehow
programs' = fromIndex <$> [0..]

halts :: Prog -> IO (Maybe Bool)
halts ps
  | Halt `notElem` ps = return $ Just False
  | not $ haltCheck ps = return $ Just True
  | Next `notElem` ps = return $ Just False
  | not $ nextCheck ps = return $ Just False
  | Just x <- circCheck ps = return $ Just x
  | otherwise = timeout ptime $ return $! evaluate ps
  where haltCheck (Skip:Halt:_) = True
        haltCheck (Skip:_:xs)   = haltCheck xs
        haltCheck (_:xs)        = haltCheck xs
        haltCheck _             = False
        nextCheck (Next:_)    = True
        nextCheck (Skip:_:xs) = nextCheck xs
        nextCheck (_:xs)      = nextCheck xs
        nextCheck _           = False

evaluate :: Prog -> Bool
evaluate ps = f (cycle ps, 0, P.fromList $ replicate tapeLength False)
  where f (Halt:_, _, _) = True
        f (Prev:x, y, z) = f (x, mod (y-1) tapeLength, z)
        f (Skip:x, y, z) | P.unsafeIndex z y = f (x,y,z) | otherwise = f (tail x, y, z)
        f (Next:x, y, z) | q <- mod (y+1) tapeLength
                         , h <- P.unsafeIndex z q
                         = f (x, q, P.update q (not h) z)
        g x@(h:_, p, m) = trace (show h ++ " - " ++ show p ++ ": " ++ show m) $ f x

circCheck ps
  | last ps == Skip = Nothing
  | otherwise = f (ps, 0, P.fromList $ replicate tapeLength False)
  where f (Halt:_, _, _) = Just True
        f (Prev:x, y, z) = f (x, mod (y-1) tapeLength, z)
        f (Skip:x, y, z) | P.unsafeIndex z y = f (x,y,z) | otherwise = f (tail x, y, z)
        f (Next:x, y, z) | q <- mod (y+1) tapeLength
                         , h <- P.unsafeIndex z q
                         = f (x, q, P.update q (not h) z)
        f ([], x, y)
          | x /= 0 = Nothing
          | not $ [Skip, Prev] `isInfixOf` ps
          , not $ [Skip, Next] `isInfixOf` ps = Just False
          | otherwise = Nothing

runChunk ps = do
  as <- mapM halts ps
  let ts = flip map as $ \x -> case x of
            Just False -> '0'
            Just True -> '1'
            Nothing -> '2'
  appendFile file ts

runChunk' ps vs = do
  ts <- forM ps $ \p ->
    let p'  = refactor p
        p'' = refactor' p
    in if False && p /= p'
          then return $ vs V.! (fromIntegral $ toIndex p')
          else if False && p /= p''
               then return $ vs V.! (fromIntegral $ toIndex p'')
               else do
                 h <- halts p''
                 return $ case h of
                           Just False -> '0'
                           Just True -> '1'
                           Nothing   -> '2'
  appendFile file ts


chunkSize = 100
run = do
  ps <- readFile file
  let vs = V.fromList ps
  let l = fromIntegral . length $ ps
  let ps = [(l+(i*chunkSize), map fromIndex [l+i*chunkSize .. (l+(i+1)*chunkSize)-1]) | i <- [0..100]]
  forM_ ps $ \(i, c) -> trace (show i) $ runChunk' c vs
  run

retry = do
  ps <- readFile file
  let vs = V.fromList ps

  ps' <- forM (zip [-1..] ps) $ \(i, c) ->
    if (c /= '2') then return c
       else do
        let p = fromIndex i
        let q = toIndex $ refactor' p

        print p

        if q < i
           then return $ vs V.! fromIntegral q
           else do h <- halts p
                   case h of
                         Just False -> print 0 >> return '0'
                         Just True -> print 1 >> return '1'
                         Nothing -> print 2 >> return '2'

  writeFile file ps'


nonterms  = map (fromIndex.fst) . filter ((=='2').snd) . zip [0..] <$> readFile file
nonterms' = map (fromIndex.fst) . filter ((=='3').snd) . zip [0..] <$> readFile file



test = do
  x <- head <$> nonterms
  print $ stateCheck x

hard = [Prev,Skip,Next,Skip,Halt,Next,Next]

hards = [ [Prev,Skip,Next,Skip,Halt,Next,Next]
        , [Next,Next,Prev,Skip,Next,Skip,Halt]
        , [Skip,Halt,Next,Next,Prev,Skip,Next]
        , [Skip,Next,Skip,Halt,Next,Next,Prev]
        , [Prev,Next,Skip,Prev,Skip,Halt,Prev,Skip]
        ]

-- refactor (Skip:Skip:xs) = refactor $ Skip : xs
refactor (x:Next:Prev:Next:Prev:xs) = if x == Skip then x : Next : refactor (Prev:Next:Prev:xs) else refactor $ x : refactor xs
refactor (x:Prev:Next:Prev:Next:xs) = if x == Skip then x : Next : refactor (Next:Prev:Next:xs) else refactor $ x : refactor xs
refactor (Next:Prev:Next:Prev:xs) = refactor xs
refactor (Prev:Next:Prev:Next:xs) = refactor xs
refactor (Skip:Next:Skip:Prev:Skip:Next:Skip:Prev:xs) = refactor xs
refactor (Skip:Prev:Skip:Next:Skip:Prev:Skip:Next:xs) = refactor xs
refactor (x:xs) = x : refactor xs
refactor [] = []

refactor' xs
  | skipOnly xs || xs == xs' = xs
  | otherwise = refactor' xs'
  where xs' = refactor $ mv xs
        -- mv  (Skip:Skip:xs) = mv $ Skip : xs
        mv  (Skip:x:xs) = mv $ xs ++ Skip : [x]
        mv  x = x
        skipOnly (Skip:_:xs) = skipOnly xs
        skipOnly [Skip] = True
        skipOnly [] = True
        skipOnly _ = False


-- If the state of the program repeats it must be looping. Super expensive- last resort
stateCheck ps = g M.empty (ps, 0, V.replicate tapeLength False)
  where f s (Halt:_, _, _) = True
        f s (Prev:x, y, z) = g s (x, mod (y-1) tapeLength, z)
        f s (Skip:x, y, z) | z V.! y = g s (x,y,z)
                           | null x = g s (tail ps, y, z)
                           | otherwise = g s (tail x, y, z)
        f s (Next:x, y, z) | q <- mod (y+1) tapeLength
                         , h <- z V.! q
                         = g s (x, q, V.update z $ V.singleton (q, not h))
        f s ([], y, z)   = g s (ps, y, z)
        g s x@(a,b,c)
          | null a, M.member (b,c) s = False
          | null a    = f (M.insert (b,c) () s) x
          | otherwise = f s x


-- If the state of the program repeats it must be looping. Super expensive- last resort
stateCheck' ps = runST $ H.newSized tapeLength >>= \s -> g s (ps, 0, V.replicate tapeLength False)
  where f s (Halt:_, _, _) = return True
        f s (Prev:x, y, z) = g s (x, mod (y-1) tapeLength, z)
        f s (Skip:x, y, z) | z V.! y = g s (x,y,z)
                           | null x = g s (tail ps, y, z)
                           | otherwise = g s (tail x, y, z)
        f s (Next:x, y, z) | q <- mod (y+1) tapeLength
                         , h <- z V.! q
                         = g s (x, q, V.update z $ V.singleton (q, not h))
        f s ([], y, z)   = g s (ps, y, z)
        g s x@(a,b,c) = do
          if not $ null a
             then f s x
             else do
               t <- H.lookup s (hash c `comb` b)
               case t of
                    Just _ -> return $ False
                    Nothing -> H.insert s (hash c `comb` b) () >> f s x
        hash c = V.ifoldl (\acc i x -> if x then comb acc i else acc) 0 c
        comb a b = ((a+b)*(a+b+1) `div` 2)+b



retry' = do
  ps <- readFile file
  let vs = V.fromList ps

  let (a,b:bs) = break (=='2') ps

  ps' <- forM (zip [0..] $ a ++ [b]) $ \(i, c) ->
    if c /= '2' then return c
       else do
        let p = fromIndex i
        let q = toIndex $ refactor' p

        print p

        if q < i
           then return $ vs V.! fromIntegral q
           else do
            x <- timeout 15000000 $ return $! stateCheck' p
            case x of
              Just False -> print 0 >> return '0'
              Just True -> print 1 >> return '1'
              Nothing -> print 3 >> return '3'

  writeFile "tmp" $ ps'++bs
  copyFile "tmp" file
  retry'

w = [Skip,Skip,Prev,Next,Prev,Next,Prev,Skip,Halt,Skip]


sanityCheck = do
  str <- readFile file
  let l = length str
  let sample = [0, div l 1000 .. l]
  let as = map (str !!) sample
  control <- mapM (halts.fromIndex.fromIntegral) sample
  let bs = map (\x -> case x of Just False -> '0'; Just True -> '1'; Nothing -> '2') control

  return $ zip sample $ zip as bs

sanityCheck' = do
  xs <- sanityCheck
  return $ filter (\(i,(a,b)) -> a /= b && a /= '2' && a /= '3' && b /= '2' && b /= '3') xs

{-
Prev,Skip,Next,Skip,Halt,Next,Next

1 0 1 1 .. 0
    ^

-}
