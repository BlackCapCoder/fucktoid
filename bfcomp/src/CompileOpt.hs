module CompileOpt where

import Control.Monad
import Optimize
import BF
import System.IO.Unsafe
import System.Process
import Debug.Trace
import qualified Data.Map as M
import System.Timeout
import System.IO

maxTime = 10^6 * 5

compileOpt :: (Program -> String) -> Program -> Maybe Program
compileOpt c p = unsafePerformIO $ do
  let (a,b) = trace "Purity check" $ span isPure' p

  if any (\case (Loop _ _) -> True; _ -> False) a then do
    let code = c (a++[DUMP])

    trace "Compiling.." $ void $ readProcess "gcc" (words "-xc - -O2 -o tmp") code

    Just (out, err) <- {- timeout maxTime -} trace "Running" $ fmap Just $ do
      (_, stdout, _stderr, _) <- runInteractiveCommand "./tmp"
      hSetEncoding stdout char8
      hSetEncoding _stderr char8
      (,) <$> hGetContents stdout <*> hGetContents _stderr

    let dump = map (map read . words) $ lines err :: [[Int]]
    let mem  = Set . M.fromList $ map (\[x,y] -> (fromIntegral x, fromIntegral y)) dump
    return . Just $ (if null out then mem : b else Put out : mem : b)
         ++ (if null b then [NOP] else [])
  else
    trace "Unpure" $ return Nothing

isPure' (In      ) = False
isPure' (Loop _ o) = all isPure' o
isPure' _          = True
